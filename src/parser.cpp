#include <memory>
#include <vector>
#include "cpp/contracts.hpp"
#include "cpp/format.hpp"
#include "cpp/variant.hpp"
#include "lexer.hpp"
#include "source_manager.hpp"
#include "program.hpp"
#include "parser.hpp"

namespace ccompiler
{

namespace
{

using TokenData = TokenStream::TokenData;
using TokenDebug = TokenStream::TokenDebug;
using TokenIterator = TokenStream::const_iterator;

// Represents a successfully parsed AST.
struct ParserSuccess
{
  std::unique_ptr<SyntaxTree> tree;

  explicit ParserSuccess(std::unique_ptr<SyntaxTree> tree) noexcept
    : tree(std::move(tree))
  {}

  explicit ParserSuccess(std::nullptr_t) noexcept
    : tree(nullptr)
  {}
};

// Error represents a trivial error.
//
// GiveUp means a rule is not able
// to start parsing. This is useful in helper functions,
// such as `parser_one_of`, where if all rules are plain wrong,
// then it's safe to assume an "expected one of" kind of error.
enum class ParserStatus
{
  Error,      //< Syntax is partly wrong.
  ErrorNote,  //< Note following a ParserStatus::Error.
  GiveUp, //< Parser couldn't make sense at all.
};

// Represents an error when some parser
// couldn't generate an AST.
struct ParserError
{
  ParserStatus status;
  TokenIterator where;  //< Where it happened.
  std::string error;    //< Error message/explanation.

#if 0
  explicit ParserError(ParserStatus status, TokenIterator where) noexcept
    : status(status), where(where)
  {}
#endif

  explicit ParserError(ParserStatus status, TokenIterator where, std::string error) noexcept
    : status(status), where(where), error(std::move(error))
  {}
};

// Sequence of errors.
//
// This is the outcome of a parser when it couldn't make sense
// out of the input tokens.
//
// TODO: use LLVM SmallVector so dynamic allocation only happens
// for more than one error.
using ParserFailure = std::vector<ParserError>;

// State of a parser.
//
// A parser can either result in a successfully parsed AST
// (ParserSuccess), or a sequence of errors (ParserFailure)
// explaining why it couldn't succeed.
using ParserState = variant<ParserFailure, ParserSuccess>;

// Return type of a parser.
struct ParserResult
{
  TokenIterator next_token; //< AST is parsed until `next_token`.
  ParserState state;        //< Result of a parser.

  explicit ParserResult(TokenIterator it, ParserState state) noexcept
    : next_token(it), state(std::move(state))
  {}
};

// Constructs a state with a ParserFailure containing one error.
//
// Useful for adding an error to a ParserState.
template <typename... Args>
auto make_error(Args&&... args) -> ParserState
{
  return ParserFailure{ParserError(std::forward<Args>(args)...)};
}

// Assigns `state` to `error` if it's a `ParserSuccess`.
// Otherwise just append `error`.
void add_error(ParserState& state, ParserError error)
{
  if (is<ParserSuccess>(state))
  {
    state = make_error(std::move(error));
  }
  else
  {
    get<ParserFailure>(state).emplace_back(std::move(error));
  }
}

// Appends a sequence of errors into `state`.
void add_error(ParserState& state, ParserFailure errors)
{
  for (auto& error : errors)
  {
    add_error(state, std::move(error));
  }
}
// Appends any existing error in `other` into `state`.
void add_error(ParserState& state, ParserState other)
{
  if (is<ParserFailure>(other))
  {
    add_error(state, get<ParserFailure>(std::move(other)));
  }
}

// Checks whether all `ParserError`s in `state` are `ParserStatus::GiveUp`.
auto is_giveup(const ParserState& state) -> bool
{
  if (is<ParserFailure>(state))
  {
    for (const auto& error : get<ParserFailure>(state))
    {
      if (error.status != ParserStatus::GiveUp)
      {
        return false;
      }
    }

    return true;
  }
  else
  {
    return false;
  }
}

auto giveup_to_expected(ParserState state, string_view what) -> ParserState
{
  if (is<ParserFailure>(state))
  {
    ParserState new_state = ParserSuccess(nullptr);

    for (auto& e : get<ParserFailure>(state))
    {
      if (e.status == ParserStatus::GiveUp)
      {
        add_error(new_state, ParserError(ParserStatus::Error, e.where, fmt::format("expected {}", what.data())));

        if (!e.error.empty())
          add_error(new_state, ParserError(ParserStatus::ErrorNote, e.where, fmt::format("{}", e.error)));
      }
      else
        add_error(new_state, std::move(e));
    }

    state = std::move(new_state);
  }

  return state;
}

auto giveup_to_expected(ParserState state) -> ParserState
{
  if (is<ParserFailure>(state))
  {
    ParserState new_state = ParserSuccess(nullptr);

    for (auto& e : get<ParserFailure>(state))
    {
      if (e.status == ParserStatus::GiveUp)
        add_error(new_state, ParserError(ParserStatus::Error, e.where, fmt::format("expected {}", e.error)));
      else
        add_error(new_state, std::move(e));
    }

    state = std::move(new_state);
  }

  return state;
}

// Adds a node to state's tree if it's a `ParserSuccess`.
void add_node(ParserState& state, std::unique_ptr<SyntaxTree> node)
{
  Expects(node != nullptr);

  if (is<ParserSuccess>(state))
  {
    auto& tree = get<ParserSuccess>(state).tree;

    if (tree == nullptr)
    {
      state = ParserSuccess(std::move(node));
    }
    else if (node->type() == NodeType::None)
    {
      tree->take_children(std::move(node));
    }
    else
    {
      tree->add_child(std::move(node));
    }
  }
}

// Accumulates one state into another.
void add_state(ParserState& state, ParserState other)
{
  if (is<ParserSuccess>(other))
  {
    add_node(state, get<ParserSuccess>(std::move(other)).tree);
  }
  else
  {
    add_error(state, std::move(other));
  }
}

struct ParserContext
{
  ProgramContext& program;
  const TokenStream& token_stream;

  explicit ParserContext(ProgramContext& program, const TokenStream& tokens)
    : program(program),
      token_stream(tokens)
  {}

  template <typename... Args>
  void note(TokenIterator token, Args&&... args)
  {
    program.note(this->token_info(token), std::forward<Args>(args)...);
  }

  template <typename... Args>
  void warning(TokenIterator token, Args&&... args)
  {
    program.warn(this->token_info(token), std::forward<Args>(args)...);
  }

  template <typename... Args>
  void error(TokenIterator token, Args&&... args)
  {
    program.error(this->token_info(token), std::forward<Args>(args)...);
  }

  template <typename... Args>
  void pedantic(TokenIterator token, Args&&... args)
  {
    program.pedantic(this->token_info(token), std::forward<Args>(args)...);
  }

private:
  auto token_info(TokenIterator token) const noexcept -> TokenStream::TokenDebug
  {
    const SourceManager& source = this->token_stream.source_manager();
    SourceRange range = token->data;
    const auto pos = source.linecol_from_location(range.begin());

    return TokenStream::TokenDebug{source, pos, range};
  }
};

// Tries to parse one of the rules.
// If a rule returns a trivial error, then that's the result.
// Otherwise, if all rules return GiveUp errors, then the result
// is "expected one of" GiveUp error.
template <typename Rule, typename... Rules>
auto parser_one_of(ParserContext& parser, TokenIterator begin, TokenIterator end,
                   string_view expected_what, Rule rule, Rules&&... rules)
  -> ParserResult
{
  auto [it, state] = rule(parser, begin, end);

  if (!is_giveup(state))
    return ParserResult(it, std::move(state));

  if constexpr (sizeof...(rules) > 0)
  {
    return parser_one_of(parser, begin, end, expected_what, std::forward<Rules>(rules)...);
  }
  else
  {
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, expected_what.data()));
  }
}

template <typename Rule, typename Predicate>
auto parser_many_of(ParserContext& parser, TokenIterator begin, TokenIterator end,
                    Rule rule, Predicate pred)
  -> ParserResult
{
  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>());
  auto it = begin;

  while (it != end && pred(it))
  {
    auto [r_it, r_state] = rule(parser, it, end);

    add_state(state, std::move(r_state));
    it = r_it;
  }

  return ParserResult(it, std::move(state));
}

template <typename Rule, typename Predicate>
auto parser_one_many_of(ParserContext& parser, TokenIterator begin, TokenIterator end,
                        string_view expected_what, Rule rule, Predicate pred)
  -> ParserResult
{
  if (begin != end)
  {
    ParserState state = ParserSuccess(std::make_unique<SyntaxTree>());
    auto it = begin;

    do
    {
      auto [r_it, r_state] = rule(parser, it, end);

      add_state(state, std::move(r_state));
      it = r_it;
    }
    while (it != end && pred(it));

    return ParserResult(it, std::move(state));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, expected_what.data()));
}

template <typename OpMatch>
auto parser_operator(NodeType op_type, OpMatch op_match)
{
  return [=] (ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end && op_match(begin))
    {
      return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(op_type, *begin)));
    }
    else
    {
      return ParserResult(end, make_error(ParserStatus::GiveUp, begin, to_string(op_type)));
    }
  };
};

template <typename LhsRule, typename OpRule, typename RhsRule>
auto parser_left_binary_operator(LhsRule lhs_rule, OpRule op_rule, RhsRule rhs_rule)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin == end)
      return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "binary operator"));

    auto [lhs_it, lhs_state] = lhs_rule(parser, begin, end);

    if (is_giveup(lhs_state))
      return ParserResult(end, std::move(lhs_state));

    while (true)
    {
      auto [op_it, op_state] = op_rule(parser, lhs_it, end);

      if (!is_giveup(op_state))
      {
        auto op_token = string_ref(lhs_it->data);
        auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);
        lhs_it = op_it;

        if (is<ParserSuccess>(rhs_state))
          lhs_it = rhs_it;
        else if (lhs_it != end)
          std::advance(lhs_it, 1);

        add_state(op_state, giveup_to_expected(std::move(lhs_state), fmt::format("expression for operator '{}'", op_token)));
        add_state(op_state, giveup_to_expected(std::move(rhs_state), fmt::format("expression for operator '{}'", op_token)));

        lhs_state = std::move(op_state);
      }
      else
        break;
    }

    return ParserResult(lhs_it, std::move(lhs_state));
  };
}

template <typename LhsRule, typename OpRule, typename RhsRule>
auto parser_right_binary_operator(LhsRule lhs_rule, OpRule op_rule, RhsRule rhs_rule)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end)
    {
      auto [lhs_it, lhs_state] = lhs_rule(parser, begin, end);

      if (!is_giveup(lhs_state))
      {
        auto [op_it, op_state] = op_rule(parser, lhs_it, end);

        if (!is_giveup(op_state))
        {
          auto op_token = string_ref(lhs_it->data);
          auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);

          add_state(op_state, std::move(lhs_state));
          add_state(op_state, giveup_to_expected(std::move(rhs_state), fmt::format("expression for operator '{}'", op_token)));

          return ParserResult(rhs_it, std::move(op_state));
        }
      }
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "binary operator"));
  };
}

auto expect_token(ParserState& state, TokenIterator it, TokenIterator end, TokenType token)
  -> bool
{
  if (it != end && it->type != token)
  {
    add_error(state, make_error(ParserStatus::Error, it, fmt::format("expected '{}'", to_string(token))));
    return false;
  }
  return it != end;
}

auto expect_end_token(ParserState& state, TokenIterator begin, TokenIterator end, TokenIterator it, TokenType token)
  -> bool
{
  if (it != end)
  {
    if (it->type != token)
    {
      add_error(state, make_error(ParserStatus::Error, it, fmt::format("expected '{}'", to_string(token))));
      add_error(state, make_error(ParserStatus::ErrorNote, begin, fmt::format("to match this '{}'", to_string(begin->type))));

      return false;
    }

    return true;
  }
  else
  {
    add_error(state, make_error(ParserStatus::Error, begin, fmt::format("missing '{}' for this", to_string(token))));
    return false;
  }
}

template <typename Rule>
auto parser_parens(Rule rule, TokenType left_brace = TokenType::LeftParen,
                   TokenType right_brace = TokenType::RightParen)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end && begin->type == left_brace)
    {
      auto [it, state] = rule(parser, std::next(begin), end);

      if (!is_giveup(state))
      {
        if (it != end && expect_end_token(state, begin, end, it, right_brace))
          std::advance(it, 1);
      }

      return ParserResult(it, std::move(state));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, fmt::format("'{}'", to_string(left_brace))));
  };
}

template <typename Rule>
auto parser_list_of(Rule rule, bool allow_trailing_comma = false)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end)
    {
      ParserState state = ParserSuccess(std::make_unique<SyntaxTree>());
      auto it = begin;

      while (it != end)
      {
        auto [r_it, r_state] = rule(parser, it, end);

        Ensures(r_it <= end);

        add_state(state, giveup_to_expected(std::move(r_state)));
        it = r_it;

        if (it != end && it->type == TokenType::Comma)
          std::advance(it, 1);

        if (allow_trailing_comma)
        {
          if (it != end &&
              (it->type == TokenType::RightCurlyBraces ||
               it->type == TokenType::RightBraces ||
               it->type == TokenType::RightParen))
          {
            break;
          }

        }

        if (r_it == end || r_it->type != TokenType::Comma)
          break;
      }

      return ParserResult(it, std::move(state));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "parser_list_of"));
  };
}


auto parser_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_primary_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_cast_expression(ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_assignment_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

// identifier:
//    [a-zA-Z_$] ([a-zA-Z_$] | [0-9])*
//
// -> ^(Identifier)

auto parser_identifier(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end && begin->type == TokenType::Identifier)
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::Identifier, *begin);
    return ParserResult(std::next(begin), ParserSuccess(std::move(tree)));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "identifier"));
}

// string-literal:
//    encoding-prefix? '"' schar-sequence? '"'
//
// encoding-prefix: one of
//    u8 u U L
//
// schar-sequence:
//    schar+
//
// schar:
//    ~["\\\r\n]
//    escape-sequence
//    '\\\n'
//    '\\\r\n'

auto parser_string_literal(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  optional<TokenData> encoding_prefix = nullopt;
  auto it = begin;

  if (it != end && it->type == TokenType::EncodingPrefix)
  {
    encoding_prefix = *it;
    std::advance(it, 1);
  }

  if (it != end && it->type == TokenType::StringConstant)
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::StringLiteral, *it);

    if (encoding_prefix)
    {
      tree->add_child(std::make_unique<SyntaxTree>(NodeType::EncodingPrefix, *encoding_prefix));
    }

    return ParserResult(std::next(it), ParserSuccess(std::move(tree)));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, it, "string literal"));
}

// string-literal-list:
//    string-literal+

auto parser_string_literal_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  auto [it, strings] = parser_one_many_of(parser, begin, end, "string literal", parser_string_literal, [] (TokenIterator t) {
    return t->type == TokenType::StringConstant;
  });

  if (is<ParserSuccess>(strings))
  {
    auto& strings_tree = get<ParserSuccess>(strings).tree;

    if (strings_tree->child_count() == 1)
    {
      return ParserResult(it, ParserSuccess(strings_tree->pop_child()));
    }
  }


  if (!is_giveup(strings))
  {
    ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StringLiteralList));
    add_state(state, std::move(strings));

    return ParserResult(it, std::move(state));
  }
  else
  {
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "string literal list"));
  }
}

// constant:
//    integer-constant
//    floating-constant
//    character-constant
//    enumeration-constant

auto parser_constant(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "constant"));

  auto const_type = [&] {
    switch (begin->type)
    {
      case TokenType::IntegerConstant:
      case TokenType::OctIntegerConstant:
      case TokenType::HexIntegerConstant: return NodeType::IntegerConstant;
      case TokenType::FloatConstant: return NodeType::FloatingConstant;
      case TokenType::CharConstant: return NodeType::CharacterConstant;
      case TokenType::Identifier: return NodeType::EnumerationConstant;
      default: return NodeType::None;
    }
  }();

  if (const_type != NodeType::None)
  {
    return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(const_type, *begin)));
  }
  else
  {
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "constant"));
  }
}

// TODO
auto parser_parameter_type_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_identifier(parser, begin, end);
}

// typedef-name:
//   identifier
//  -> ^(TypedefName)

auto parser_type_name(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

auto parser_typedef_name(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  // TODO check if typedef-name is valid.
  if (/*DISABLE CODE*/ (false) && begin != end && begin->type == TokenType::Identifier)
  {
    return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(NodeType::TypedefName, *begin)));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "typedef name"));
}

// atomic-type-specifier:
//   '_Atomic' '(' type-name ')'
//  -> ^(AtomicTypeSpecifier type-name)

auto parser_atomic_type_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end && std::next(begin) != end)
  {
    if (begin->type == TokenType::Atomic &&
        std::next(begin)->type == TokenType::LeftParen)
    {
      auto [it, type_name] = parser_parens(parser_type_name)(parser, std::next(begin), end);
      ParserState atomic_type_spec = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::AtomicTypeSpecifier, *begin));
      add_state(atomic_type_spec, giveup_to_expected(std::move(type_name), "type name for atomic type specifier"));

      return ParserResult(it, std::move(atomic_type_spec));
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "atomic type specifier"));
}

// type-specifier:
//   'void'
//   'char'
//   'short'
//   'int'
//   'long'
//   'float'
//   'double'
//   'signed'
//   'unsigned'
//   '_Bool'
//   '_Complex'
//   '__m128'
//   '__m128d'
//   '__m128i'
//  -> ^(TypeSpecifier)
//
//   atomic-type-specifier
//   struct-or-union-specifier
//   enum-specifier
//   typedef-name
//  -> ^(TypeSpecifier sub-type-specifier)

auto parser_struct_or_union_specifier(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_enum_specifier(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

auto parser_type_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    ParserState type_spec = ParserSuccess(nullptr);
    auto it = begin;

    switch (begin->type)
    {
      case TokenType::VoidType:
      case TokenType::CharType:
      case TokenType::ShortType:
      case TokenType::IntType:
      case TokenType::LongType:
      case TokenType::FloatType:
      case TokenType::DoubleType:
      case TokenType::Signed:
      case TokenType::Unsigned:
      case TokenType::Bool:
      case TokenType::Complex:
      case TokenType::VectorM128:
      case TokenType::VectorM128d:
      case TokenType::VectorM128i:
      {
        add_node(type_spec, std::make_unique<SyntaxTree>(NodeType::TypeSpecifier, *begin));
        it = std::next(begin);
        break;
      }

      default:
      {
        auto [type_it, sub_type_spec] =
          parser_one_of(parser, begin, end, "type specifier",
                        parser_atomic_type_specifier,
                        parser_struct_or_union_specifier,
                        parser_enum_specifier,
                        parser_typedef_name);

        if (is<ParserSuccess>(sub_type_spec))
          add_node(type_spec, std::make_unique<SyntaxTree>(NodeType::TypeSpecifier));

        add_state(type_spec, std::move(sub_type_spec));
        it = type_it;
      }
    }

    return ParserResult(it, std::move(type_spec));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "type specifier"));
}

// type-qualifier:
//   'const'
//   'restrict'
//   'volatile'
//   '_Atomic'
//  -> ^(TypeQualifier)

auto parser_type_qualifier(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    switch (begin->type)
    {
      case TokenType::Const:
      case TokenType::Restrict:
      case TokenType::Volatile:
      case TokenType::Atomic:
        return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(NodeType::TypeQualifier, *begin)));

      default:
        break;
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "type qualifier"));
}

// type-qualifier-list:
//   type-qualifier+

auto parser_type_qualifier_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  auto [it, quals] = parser_one_many_of(parser, begin, end, "type qualifier list", parser_type_qualifier, [] (TokenIterator t) {
    return t->type == TokenType::Const    ||
           t->type == TokenType::Restrict ||
           t->type == TokenType::Volatile ||
           t->type == TokenType::Atomic;
  });

  ParserState qualifiers = ParserSuccess(nullptr);

  if (is<ParserSuccess>(quals))
    qualifiers = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::TypeQualifierList));

  add_state(qualifiers, std::move(quals));

  return ParserResult(it, std::move(qualifiers));
}

// pointer:
//   '*' type-qualifier-list?
//  -> ^(PointerDeclarator type-qualifier-list?)
//
//   '*' type-qualifier-list? pointer
//  -> ^(PointerDeclarator type-qualifier-list? PointerDeclarator?)

auto parser_pointer(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end && begin->type == TokenType::Times)
  {
    ParserState pointer = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::PointerDeclarator, *begin));
    auto it = std::next(begin);

    if (auto [qual_it, qual_list] = parser_type_qualifier_list(parser, it, end);
        !is_giveup(qual_list))
    {
      add_state(pointer, giveup_to_expected(std::move(qual_list), "type qualifier list for pointer type"));
      it = qual_it;
    }

    if (it != end && it->type == TokenType::Times)
    {
      auto [sub_ptr_it, sub_pointer] = parser_pointer(parser, it, end);
      add_state(pointer, giveup_to_expected(std::move(sub_pointer), "nested pointer"));
      it = sub_ptr_it;
    }

    return ParserResult(it, std::move(pointer));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "pointer"));
}

// direct-abstract-declarator:
//   '[' ']'
//  -> ^(ArrayVLADeclarator)
//
//   '[' type-qualifier-list? assignment-expression? ']'
//  -> ^(ArrayDeclarator TypeQualifierList? AssignmentExpression?)
//
//   '[' 'static' type-qualifier-list? assignment-expression ']'
//  -> ^(ArrayStaticDeclarator TypeQualifierList? AssignmentExpression)
//
//   '[' type-qualifier-list 'static' assignment-expression ']'
//  -> ^(ArrayStaticDeclarator TypeQualifierList AssignmentExpression)
//
//   '[' type-qualifier-list? '*' ']'
//  -> ^(ArrayVLADeclarator TypeQualifierList?)
//
//   direct-abstract-declarator '[' type-qualifier-list? assignment-expression? ']'
//   direct-abstract-declarator '[' 'static' type-qualifier-list? assignment-expression ']'
//   direct-abstract-declarator '[' type-qualifier-list 'static' assignment-expression ']'
//   direct-abstract-declarator '[' '*' ']'
//   direct-abstract-declarator '(' parameter-type-list? ')'

auto parser_direct_abstract_declarator(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  auto array_declarator = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end)
    {
      auto array_token = *std::prev(begin);
      auto it = begin;

      // '[' ']'
      if (it->type == TokenType::RightBraces)
      {
        return ParserResult(it, ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArrayVLADeclarator, array_token)));
      }

      // '[' '*' ']'
      if (it->type == TokenType::Times && std::next(it) != end && std::next(it)->type == TokenType::RightBraces)
      {
        return ParserResult(std::next(it), ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArrayVLADeclarator, array_token)));
      }

      // '[' 'static' type-qualifier-list? assignment-expression ']'
      if (it->type == TokenType::Static)
      {
        ParserState decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArrayStaticDeclarator, array_token));
        std::advance(it, 1);

        if (auto [qual_it, qual_list] = parser_type_qualifier_list(parser, it, end);
            !is_giveup(qual_list))
        {
          add_state(decl, giveup_to_expected(std::move(qual_list), "qualifiers for array declarator"));
          it = qual_it;
        }

        auto [assign_it, assign_expr] = parser_assignment_expression(parser, it, end);
        add_state(decl, giveup_to_expected(std::move(assign_expr), "expression for array length"));
        it = assign_it;

        return ParserResult(it, std::move(decl));
      }
      else
      {
        auto [qual_it, qualifiers] = parser_type_qualifier_list(parser, it, end);

        // '[' type-qualifier-list 'static' assignment-expression ']'
        //  -> ^(ArrayStaticDeclarator TypeQualifierList AssignmentExpression)
        if (!is_giveup(qualifiers) && qual_it != end && qual_it->type == TokenType::Static)
        {
          auto [assign_it, assign_expr] = parser_assignment_expression(parser, /*skip 'static'*/ std::next(qual_it), end);
          ParserState decl = ParserSuccess(nullptr);
          it = assign_it;

          if (is<ParserSuccess>(assign_expr))
            decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArrayStaticDeclarator, array_token));

          add_state(decl, giveup_to_expected(std::move(qualifiers), "qualifiers for array declarator"));
          add_state(decl, giveup_to_expected(std::move(assign_expr), "expression for array length"));

          return ParserResult(it, std::move(decl));
        }

        // '[' type-qualifier-list? '*' ']'
        if (qual_it != end && qual_it->type == TokenType::Times)
        {
          ParserState decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArrayVLADeclarator, array_token));
          it = /*skip '*'*/ std::next(qual_it);

          if (!is_giveup(qualifiers))
            add_state(decl, std::move(qualifiers));

          return ParserResult(it, std::move(decl));
        }

        // '[' type-qualifier-list? assignment-expression? ']'
        auto [assign_it, assign_expr] = parser_assignment_expression(parser, (qual_it != end)? qual_it : it, end);

        ParserState decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArrayDeclarator, array_token));

        if (!is_giveup(qualifiers))
        {
          add_state(decl, std::move(qualifiers));
          it = qual_it;
        }

        if (!is_giveup(assign_expr))
        {
          add_state(decl, std::move(assign_expr));
          it = assign_it;
        }

        return ParserResult(it, std::move(decl));
      }
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "direct abstract declarator"));
  };

  if (begin != end)
  {
    auto array_declarator_production = parser_parens(array_declarator, TokenType::LeftBraces, TokenType::RightBraces);
    auto [it, array_decl] = array_declarator_production(parser, begin, end);

    if (is_giveup(array_decl))
      return ParserResult(it, std::move(array_decl));

    while (it != end)
    {
      // direct-abstract-declarator '(' parameter-type-list? ')'
      if (it->type == TokenType::LeftParen)
      {
        auto [params_it, parameters] = parser_parens(parser_parameter_type_list)(parser, it, end);

        ParserState func_decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::FunctionDeclarator));

        add_state(func_decl, giveup_to_expected(std::move(array_decl), "array declarator"));
        add_state(func_decl, giveup_to_expected(std::move(parameters), "parameter type list"));

        array_decl = std::move(func_decl);
        it = params_it;
      }
      else if (it->type == TokenType::LeftBraces)
      {
        auto [decl_it, declarator] = array_declarator_production(parser, it, end);
        ParserState direct_decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::DirectAbstractDeclarator));

        add_state(direct_decl, giveup_to_expected(std::move(array_decl), "array declarator"));
        add_state(direct_decl, giveup_to_expected(std::move(declarator), "array declarator"));

        array_decl = std::move(direct_decl);
        it = decl_it;
      }
      else
        break;
    }

    return ParserResult(it, std::move(array_decl));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "direct abstract declarator"));
}

// abstract-declarator:
//   pointer
//   pointer? direct-abstract-declarator

auto parser_abstract_declarator(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    // pointer
    if (begin->type == TokenType::Times)
    {
      auto [ptr_it, pointer_decl] = parser_pointer(parser, begin, end);

      // direct-abstract-declarator
      if (ptr_it != end && ptr_it->type == TokenType::LeftBraces)
      {
        ParserState abstract_decl = ParserSuccess(nullptr);
        auto [decl_it, decl] = parser_direct_abstract_declarator(parser, ptr_it, end);

        if (!is_giveup(decl))
          add_node(abstract_decl, std::make_unique<SyntaxTree>(NodeType::AbstractDeclarator));

        add_state(abstract_decl, giveup_to_expected(std::move(pointer_decl)));
        add_state(abstract_decl, giveup_to_expected(std::move(decl)));

        return ParserResult(decl_it, std::move(abstract_decl));
      }

      return ParserResult(ptr_it, std::move(pointer_decl));
    }
    else if (begin->type == TokenType::LeftBraces)
    {
      return parser_direct_abstract_declarator(parser, begin, end);
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "abstract declarator"));
}

// specifier-qualifier-list:
//   (type-specifier | type-qualifier)+

auto parser_specifier_qualifier_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    ParserState qualifiers = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::SpecifierQualifierList));
    auto it = begin;

    while (true)
    {
      auto [s_it, spec_qual] = parser_one_of(parser, it, end, "type specifier or qualifier",
                                             parser_type_specifier,
                                             parser_type_qualifier);

      if (!is_giveup(spec_qual))
      {
        add_state(qualifiers, std::move(spec_qual));
        it = s_it;
      }
      else
        break;
    }

    return ParserResult(it, std::move(qualifiers));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "specifier qualifier list"));
}

// type-name:
//    specifier-qualifier-list abstract-declarator?

auto parser_type_name(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto [spec_it, spec_qual_list] = parser_specifier_qualifier_list(parser, begin, end);
    auto it = spec_it;

    if (is_giveup(spec_qual_list))
      return ParserResult(spec_it, std::move(spec_qual_list));

    ParserState type_name = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::TypeName));
    add_state(type_name, std::move(spec_qual_list));

    if (auto [decl_it, abstract_decl] = parser_abstract_declarator(parser, it, end);
        !is_giveup(abstract_decl))
    {
      add_state(type_name, std::move(abstract_decl));
      it = decl_it;
    }

    return ParserResult(it, std::move(type_name));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "type name"));
}

auto parser_conditional_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

auto parser_constant_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_conditional_expression(parser, begin, end);
}

// static-assert-declaration:
//   '_Static_assert' '(' constant-expression ',' string-literal+ ')' ';'

auto parser_static_assert_declaration(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  // constant-expression ',' string-literal+
  //  -> ^(None constant-expression string-literal+)
  auto static_assert_args_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    ParserState args = ParserSuccess(std::make_unique<SyntaxTree>());
    auto it = begin;

    auto [const_it, const_expr] = parser_constant_expression(parser, it, end);
    add_state(args, giveup_to_expected(std::move(const_expr)));
    //it = const_it;

    if (expect_token(args, it, end, TokenType::Comma))
      std::advance(it, 1);

    auto [strings_it, strings] = parser_string_literal_list(parser, it, end);
    add_state(args, giveup_to_expected(std::move(strings)));
    it = strings_it;

    return ParserResult(it, std::move(args));
  };

  if (begin != end && begin->type == TokenType::StaticAssert)
  {
    auto [it, arguments] = parser_parens(static_assert_args_production)(parser, std::next(begin), end);

    if (is<ParserSuccess>(arguments))
    {
      if (expect_end_token(arguments, begin, end, it, TokenType::Semicolon))
        std::advance(it, 1);
    }

    ParserState static_assert_decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StaticAssertDeclaration));
    add_state(static_assert_decl, std::move(arguments));

    return ParserResult(it, std::move(static_assert_decl));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "static assert declaration"));
}

// declarator:
//   pointer? direct-declarator

auto parser_pointer(ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult;

auto parser_declarator(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    ParserStatus declarator = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::Declarator));
    auto [ptr_it, pointer_decl] = parser_pointer(parser, begin, end);

    if (!is_giveup(pointer_decl))
      add_node(declarator, std::move(pointer_decl));

    auto it = ptr_it != end
      ? ptr_it
      : begin;

    auto [dir_it, direct_decl] = parser_direct_declarator(parser, it, end);

    add_state(declarator, giveup_to_expected(std::move(direct_decl), "direct declarator for declarator"));

    return ParserResult(dir_it, std::move(declarator));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "declarator"));
}

// init-declarator:
//   declarator
//   declarator '=' initializer

auto parser_init_declarator(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto [decl_it, declarator] = parser_declarator(parser, begin, end);

    if (!is_giveup(declarator))
    {
      ParserState init_decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::InitDeclarator));
      auto it = decl_it;

      add_state(init_decl, std::move(declarator));

      if (it != end && it->type == TokenType::Assign)
      {
        auto [init_it, initializer] = parser_initializer(parser, std::next(decl_it), end);
        it = init_it;

        add_state(init_decl, giveup_to_expected(stD::move(initializer), "initializer for init declarator"));
      }

      return ParserResult(it, std::move(init_decl));
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "init declarator"));
}

// init-declarator-list:
//   init-declarator
//   init-declarator-list ',' init-declarator

auto parser_init_declarator_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_list_of(parser_init_declarator)(parser, begin, end);
}

// storage-class-specifier:
//   'typedef'
//   'extern'
//   'static'
//   '_Thread_local'
//   'auto'
//   'register'

auto parser_storage_class_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    switch (begin->type)
    {
      case TokenType::Typedef:
      case TokenType::Extern:
      case TokenType::Static:
      case TokenType::ThreadLocal:
      case TokenType::Auto:
      case TokenType::Register:
        return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StorageClassSpecifier, *begin)));

      default:
        break;
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "storage class specifier"));
}

// function-specifier:
//   ('inline'
//   '_Noreturn'
//   '__stdcall')
//   '__declspec' '(' identifier ')'

auto parser_function_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    switch (begin->type)
    {
      case TokenType::Inline:
      case TokenType::Noreturn:
      case TokenType::Stdcall:
        return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(NodeType::FunctionSpecifier, *begin)));

      case TokenType::Declspec:
      {
        auto [it, identifier] = parser_parens(parser_identifier)(parser, std::next(begin), end);
        ParserState func_spec = ParserSuccess(nullptr);

        if (is<ParserSuccess>(identifier))
          func_spec = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::FunctionSpecifier, *begin));

        add_state(func_spec, giveup_to_expected(std::move(identifier), "declspec argument"));

        return ParserResult(it, std::move(func_spec));
      }

      default:
        break;
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "function specifier"));
}

// alignment-specifier:
//   '_Alignas' '(' type-name ')'
//   '_Alignas' '(' constant-expression ')'

auto parser_alignment_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end && begin->type == TokenType::Alignas)
  {
    auto alignas_arg_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      return parser_one_of(parser, begin, end, "type name or constant expression",
                           parser_type_name,
                           parser_constant_expression);
    };

    auto [it, alignas_arg] = parser_parens(alignas_arg_production)(parser, std::next(begin), end);
    ParserStatus alignas_spec = ParserSuccess(nullptr);

    if (is<ParserSuccess>(alignas_spec))
      add_node(alignas_spec, std::make_unique<SyntaxTree>(NodeType::Alignas, *begin));

    add_state(alignas_spec, giveup_to_expected(std::move(alignas_arg), "alignas argument"));

    return ParserResult(it, std::move(alignas_spec));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "alignment specifier"));
}


// declaration-specifier:
//   storage-class-specifier
//   type-specifier
//   type-qualifier
//   function-specifier
//   alignment-specifier

auto parser_declaration_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_one_of(parser, begin, end, "declaration specifier",
                       parser_storage_class_specifier,
                       parser_type_specifier,
                       parser_type_qualifier,
                       parser_function_specifier,
                       parser_alignment_specifier);
}

// enum-specifier:
//   'enum' identifier? '{' enumerator-list ','opt '}'
//      -> ^(EnumSpecifier identifier? enumerator-list)
//
//   'enum' identifier
//      -> ^(EnumSpecifier identifier)
//
// enumerator-list:
//   enumerator
//   enumerator-list ',' enumerator
//
// enumerator:
//   enumeration-constant
//   enumeration-constant '=' constant-expression
//
// enumeration-constant:
//   identifier

auto parser_enumeration_constant(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  // TODO register enumerations in symbol table.

  if (begin != end && begin->type == TokenType::Identifier)
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::Enumerator, *begin);
    return ParserResult(std::next(begin), ParserSuccess(std::move(tree)));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "enumerator"));
}

auto parser_enum_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end && begin->type == TokenType::Enum)
  {
    // enumerator:
    //   enumeration-constant
    //   enumeration-constant '=' constant-expression
    //
    //   -> ^(Enumerator const-expr?)

    auto enumerator_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      if (begin != end && begin->type == TokenType::Identifier)
      {
        auto [it, enumerator] = parser_enumeration_constant(parser, begin, end);

        if (it != end && it->type == TokenType::Assign)
        {
          auto [const_it, const_expr] = parser_constant_expression(parser, std::next(it), end);
          add_state(enumerator, giveup_to_expected(std::move(const_expr), "constant expression"));
          it = const_it;
        }

        return ParserResult(it, std::move(enumerator));
      }

      return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "enumerator"));
    };

    // enumerator-list:
    //   enumerator
    //   enumerator-list ',' enumerator
    //
    //  -> ^(EnumeratorList enumerator+)

    auto enumerator_list_production = parser_list_of(enumerator_production, /*allow_trailing_comma=*/ true);

    // '{' enumerator-list ','opt '}' -> enumerator-list

    auto enum_list_production = parser_parens(enumerator_list_production,
                                              TokenType::LeftCurlyBraces,
                                              TokenType::RightCurlyBraces);

    ParserState enum_spec = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::EnumSpecifier, *begin));
    auto it = std::next(begin); //< Skips TokenType::Enum.

    // enum-specifier:
    //   'enum' identifier? '{' enumerator-list ','opt '}' -> ^(EnumSpecifier identifier? enumerator-list)
    //   'enum' identifier                                 -> ^(EnumSpecifier identifier)

    if (it != end)
    {
      if (it->type == TokenType::Identifier)
      {
        auto [ident_it, identifier] = parser_identifier(parser, it, end);
        add_state(enum_spec, std::move(identifier));
        it = ident_it;

        if (it != end && it->type == TokenType::LeftCurlyBraces)
        {
          auto [enum_list_it, enum_list] = enum_list_production(parser, it, end);
          add_state(enum_spec, giveup_to_expected(std::move(enum_list)));
          it = enum_list_it;
        }
      }
      else if (it->type == TokenType::LeftCurlyBraces)
      {
        auto [enum_list_it, enum_list] = enum_list_production(parser, it, end);
        add_state(enum_spec, giveup_to_expected(std::move(enum_list)));
        it = enum_list_it;
      }
      else
      {
        add_error(enum_spec, make_error(ParserStatus::Error, it, "expected identifier or '{'"));
        add_error(enum_spec, make_error(ParserStatus::ErrorNote, begin, "for this enumerator specifier"));
      }

      return ParserResult(it, std::move(enum_spec));
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "enumerator specifier"));
}

// struct-or-union-specifier:
//   struct-or-union identifier? '{' struct-declaration-list '}'
//    -> ^(StructOrUnionSpecifier identifier? struct-declaration-list)
//
//   struct-or-union identifier
//    -> ^(StructOrUnionSpecifier identifier)
//
// struct-or-union:
//   'struct'
//   'union'
//
// struct-declaration-list:
//   struct-declaration
//   struct-declaration-list struct-declaration
//
// struct-declaration:
//   specifier-qualifier-list struct-declarator-list? ';'
//   static-assert-declaration
//
// struct-declarator-list:
//   struct-declarator
//   struct-declarator-list ',' struct-declarator
//
// struct-declarator:
//   declarator
//   declarator? ':' constant-expression

auto parser_struct_or_union_specifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  // TODO make error messages issue struct or union depending on the current token.

  if (begin != end && (begin->type == TokenType::Struct || begin->type == TokenType::Union))
  {
    // struct-declarator:
    //   declarator                           -> ^(StructDeclarator declarator)
    //   declarator? ':' constant-expression  -> ^(StructDeclarator declarator? constant-expression)

    auto struct_declarator = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      if (begin != end)
      {
        ParserState struct_decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StructDeclarator));
        auto it = begin;

        // declarator
        if (begin->type != TokenType::Colon)
        {
          auto [decl_it, declarator] = parser_declarator(parser, begin, end);
          add_state(struct_decl, giveup_to_expected(std::move(declarator), "declarator"));
          it = decl_it;

          if (it != end && it->type == TokenType::Colon)
          {
            auto [const_expr_it, const_expr] = parser_constant_expression(parser, std::next(it), end);
            add_state(struct_decl, giveup_to_expected(std::move(const_expr), "constant expression"));
            it = const_expr_it;
          }
        }
        else
        {
          auto [const_expr_it, const_expr] = parser_constant_expression(parser, std::next(it), end);
          add_state(struct_decl, giveup_to_expected(std::move(const_expr), "constant expression"));
          it = const_expr_it;
        }

        return ParserResult(it, std::move(struct_decl));
      }

      return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "struct declarator"));
    };

    // struct-declarator-list:
    //   struct-declarator
    //   struct-declarator-list ',' struct-declarator

    auto struct_declarator_list = parser_list_of(struct_declarator);

    // struct-declaration:
    //   specifier-qualifier-list struct-declarator-list? ';'
    //   static-assert-declaration

    auto struct_declaration = [struct_declarator_list] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      if (begin != end)
      {
        if (begin->type == TokenType::StaticAssert)
        {
          return parser_static_assert_declaration(parser, begin, end);
        }
        else
        {
          ParserState struct_decl = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StructDeclaration));
          auto it = begin;

          auto [spec_qual_it, spec_qual_list] = parser_specifier_qualifier_list(parser, begin, end);
          add_state(struct_decl, giveup_to_expected(std::move(spec_qual_list), "specifier qualifier list"));
          it = spec_qual_it;

          auto [decl_it, decl_list] = struct_declarator_list(parser, spec_qual_it, end);

          if (!is_giveup(decl_list))
          {
            add_state(struct_decl, std::move(decl_list));
            it = decl_it;
          }

          if (expect_token(struct_decl, it, end, TokenType::Semicolon))
            std::advance(it, 1);

          return ParserResult(it, std::move(struct_decl));
        }
      }

      return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "struct declaration"));
    };

    // struct-declaration-list:
    //   struct-declaration
    //   struct-declaration-list struct-declaration

    auto struct_declaration_list = [struct_declaration] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      return parser_one_many_of(parser, begin, end, "struct declaration list", struct_declaration, [] (TokenIterator t) {
        return t->type != TokenType::RightCurlyBraces;
      });
    };

    // '{' struct-declaration-list '}'

    auto struct_decl_list_production = parser_parens(struct_declaration_list,
                                                     TokenType::LeftCurlyBraces,
                                                     TokenType::RightCurlyBraces);

    ParserState struct_spec = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StructOrUnionSpecifier, *begin));
    auto it = std::next(begin);

    if (it != end)
    {
      if (it->type == TokenType::Identifier)
      {
        auto [ident_it, identifier] = parser_identifier(parser, it, end);
        add_state(struct_spec, std::move(identifier));
        it = ident_it;

        if (it != end && it->type == TokenType::LeftCurlyBraces)
        {
          auto [decl_list_it, decl_list] = struct_decl_list_production(parser, it, end);
          add_state(struct_spec, giveup_to_expected(std::move(decl_list)));
          it = decl_list_it;
        }
      }
      else if (it->type == TokenType::LeftCurlyBraces)
      {
        auto [decl_list_it, decl_list] = struct_decl_list_production(parser, it, end);
        add_state(struct_spec, giveup_to_expected(std::move(decl_list)));
        it = decl_list_it;
      }
      else
      {
        add_error(struct_spec, make_error(ParserStatus::Error, it, "expected identifier or '{'"));
        add_error(struct_spec, make_error(ParserStatus::ErrorNote, begin, fmt::format("for this {} specifier", to_string(begin->type))));
      }

      return ParserResult(it, std::move(struct_spec));
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "struct or union specifier"));
}

// initializer:
//    assignment-expression
//    '{' initializer-list '}'
//    '{' initializer-list ',' '}'

auto parser_initializer_list(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_assignment_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

auto parser_initializer(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    if (begin->type == TokenType::LeftCurlyBraces)
    {
      auto init_list_production = parser_parens(parser_initializer_list,
                                                TokenType::LeftCurlyBraces,
                                                TokenType::RightCurlyBraces);

      return init_list_production(parser, begin, end);
    }
    else
    {
      return parser_assignment_expression(parser, begin, end);
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "initializer"));
}

// initializer-list:
//    designation? initializer
//    initializer-list ',' designation? initializer

auto parser_initializer_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  // designator:
  //    '[' constant-expression ']' -> ^(ArraySubscripting constant-expression)
  //    '.' identifier              -> ^(MemberAccess identifier)

  auto designator_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end)
    {
      if (begin->type == TokenType::LeftBraces)
      {
        auto subscript_production = parser_parens(parser_constant_expression,
                                                  TokenType::LeftBraces,
                                                  TokenType::RightBraces);

        auto [it, subscript] = subscript_production(parser, begin, end);

        ParserState designator = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArraySubscripting));
        add_state(designator, std::move(subscript));

        return ParserResult(it, std::move(designator));
      }
      else if (begin->type == TokenType::Dot)
      {
        auto [it, identifier] = parser_identifier(parser, std::next(begin), end);

        ParserState designator = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::MemberAccess));
        add_state(designator, giveup_to_expected(std::move(identifier), "identifier for designator"));


        return ParserResult(it, std::move(designator));
      }
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "designator"));
  };


  // designation:
  //    designator+ '='
  //  -> ^(Designation designator+)

  auto designation_production = [designator_production] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end)
    {
      ParserState designation = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::Designation));

      auto [it, designator] = designator_production(parser, begin, end);

      if (!is_giveup(designator))
      {
        add_state(designation, giveup_to_expected(std::move(designator), "designator"));

        while (it != end && it->type != TokenType::Assign)
        {
          auto [des_it, designator] = designator_production(parser, it, end);

          if (is_giveup(designator))
          {
            // Missing assignment operator.
            add_error(designation, make_error(ParserStatus::Error, it, "expected assignment operator for designator"));
            break;
          }

          add_state(designation, giveup_to_expected(std::move(designator), "designator"));
          it = des_it;
        }

        if (it != end && it->type == TokenType::Assign)
          return ParserResult(std::next(it), std::move(designation));
        else
          return ParserResult(it, std::move(designation));
      }
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "designation"));
  };

  // initializer-list:
  //    designation? initializer
  //    initializer-list ',' designation? initializer
  // -> ^(InitializerList (initializer | ^(designation initializer))+)

  auto init_list_production = [designation_production] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end)
    {
      ParserState init_list = ParserSuccess(nullptr);
      auto [des_it, designation] = designation_production(parser, begin, end);
      auto it = des_it;

      if (!is_giveup(designation))
      {
        auto [init_it, initializer] = parser_initializer(parser, des_it, end);

        add_state(init_list, std::move(designation));
        add_state(init_list, giveup_to_expected(std::move(initializer), "initializer"));
        it = init_it;
      }
      else
      {
        auto [init_it, initializer] = parser_initializer(parser, begin, end);
        add_state(init_list, std::move(initializer));
        it = init_it;
      }

      return ParserResult(it, std::move(init_list));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "initializer list"));
  };

  auto [it, inits] = parser_list_of(init_list_production, /*allow_trailing_comma=*/ true)(parser, begin, end);

  // Do `parser_list_of` manually.
  /*
  ParserState inits = ParserSuccess(std::make_unique<SyntaxTree>());
  auto it = begin;

  while (it != end)
  {
    auto [il_it, init_list] = init_list_production(parser, it, end);

    Ensures(il_it <= end);

    add_state(inits, giveup_to_expected(std::move(init_list), "initializer list"));
    it = il_it;

    if (it != end && it->type == TokenType::Comma)
      std::advance(it, 1);

    if (it != end && it->type == TokenType::RightCurlyBraces)
      break;

    if (il_it == end || il_it->type != TokenType::Comma)
      break;
  }
  */

  if (!is_giveup(inits))
  {
    ParserState init_list = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::InitializerList));
    add_state(init_list, std::move(inits));

    return ParserResult(it, std::move(init_list));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "initializer list"));
}

// postfix-expression:
//    primary-expression
//    compound-literal
//
//    postfix-expression '[' expression ']'
//      -> ^(ArraySubscripting expression postfix-expr)
//
//    postfix-expression '(' argument-expression-list? ')'
//      -> ^(FunctionCall argument-list postfix-expr)
//
//    postfix-expression '.' identifier
//      -> ^(MemberAccess ident postfix-expr)
//
//    postfix-expression '->' identifier
//      -> ^(PointerMemberAccess ident postfix-expr)
//
//    postfix-expression '++' -> ^(PostfixIncrement postfix-expr)
//    postfix-expression '--' -> ^(PostfixDecrement postfix-expr)
//
// compound-literal:
//    '(' type-name ')' '{' initializer-list '}'     //< TODO
//    '(' type-name ')' '{' initializer-list ',' '}' //< TODO
//      -> ^(CompoundLiteral type init-list)

// TODO
auto parser_postfix_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    // compound-literal:
    //    '(' type-name ')' '{' initializer-list '}'
    //    '(' type-name ')' '{' initializer-list ',' '}'

    auto compound_literal_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      if (begin != end)
      {
        auto [type_it, type_name] = parser_parens(parser_type_name)(parser, begin, end);

        if (!is_giveup(type_name))
        {
          auto initializer_list_production =
            parser_parens(parser_initializer_list,
                          TokenType::LeftCurlyBraces,
                          TokenType::RightCurlyBraces);

          auto [init_it, initalizer_list] = initializer_list_production(parser, type_it, end);

          if (!is_giveup(initalizer_list))
          {
            ParserState compound_literal = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::CompoundLiteral));
            add_state(compound_literal, std::move(type_name));
            add_state(compound_literal, std::move(initalizer_list));

            return ParserResult(init_it, std::move(compound_literal));
          }
        }
      }

      return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "compound literal"));
    };

    auto postfix_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
      -> ParserResult
    {
      if (begin == end)
        return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "postfix operator"));

      switch (begin->type)
      {
        // '[' expression ']' -> ^(ArraySubscripting expression)
        case TokenType::LeftBraces:
        {
          auto [expr_it, expression] = parser_expression(parser, std::next(begin), end);

          if (expect_end_token(expression, begin, end, expr_it, TokenType::RightBraces))
          {
            ParserState postfix_op =
              ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArraySubscripting, *begin));

            add_state(postfix_op, giveup_to_expected(std::move(expression), "expression"));

            return ParserResult(std::next(expr_it), std::move(postfix_op));
          }
          else
          {
            return ParserResult(expr_it, std::move(expression));
          }
        }

        // '(' argument-expression-list? ')' -> ^(FunctionCall arguments?)
        case TokenType::LeftParen:
        {
          ParserState postfix_op =
            ParserSuccess(std::make_unique<SyntaxTree>(NodeType::FunctionCall, *begin));

          if (std::next(begin) != end && std::next(begin)->type == TokenType::RightParen)
          {
            // Empty argument list.
            return ParserResult(std::next(begin, 2), std::move(postfix_op));
          }

          auto arg_list = parser_parens(parser_list_of(parser_assignment_expression));
          auto [arg_it, argument_list] = arg_list(parser, begin, end);

          ParserState arguments =
            ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ArgumentExpressionList));

          add_state(arguments, giveup_to_expected(std::move(argument_list), "argument list"));
          add_state(postfix_op, std::move(arguments));

          return ParserResult(arg_it, std::move(postfix_op));
        }

        // '.' identifier  -> ^(MemberAccess identifier)
        // '->' identifier -> ^(PointerMemberAccess identifier)
        case TokenType::Dot:
        case TokenType::RightArrow:
        {
          const auto node_type = begin->type == TokenType::Dot
            ? NodeType::MemberAccess
            : NodeType::PointerMemberAccess;

          auto [ident_it, identifier] = parser_identifier(parser, std::next(begin), end);

          ParserState postfix_op =
            ParserSuccess(std::make_unique<SyntaxTree>(node_type, *begin));

          add_state(postfix_op, giveup_to_expected(std::move(identifier)));

          return ParserResult(ident_it, std::move(postfix_op));
        }

        // '++' -> ^(PostfixIncrement)
        // '--' -> ^(PostfixDecrement)
        case TokenType::Increment:
        case TokenType::Decrement:
        {
          const auto node_type = begin->type == TokenType::Increment
            ? NodeType::PostfixIncrement
            : NodeType::PostfixDecrement;

          auto tree = std::make_unique<SyntaxTree>(node_type, *begin);
          return ParserResult(std::next(begin), ParserSuccess(std::move(tree)));
        }

        default:
          return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "postfix operator"));
      }
    };

    auto [expr_it, expr] = parser_one_of(parser, begin, end, "compound literal or expression",
                                         compound_literal_production,
                                         parser_primary_expression);
    auto it = expr_it;

    if (is_giveup(expr))
      return ParserResult(end, std::move(expr));

    while (true)
    {
      auto [op_it, postfix_op] = postfix_production(parser, it, end);

      if (!is_giveup(postfix_op))
      {
        add_state(postfix_op, std::move(expr));
        expr = std::move(postfix_op);
        it = op_it;
      }
      else
        break;
    }

    return ParserResult(it, std::move(expr));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "postfix expression"));
}

// unary-expression:
//    postfix-expression
//    '++' unary-expression
//    '--' unary-expression
//    unary-operator cast-expression
//    'sizeof' unary-expression
//    'sizeof' '(' type-name ')'
//    '_Alignof' '(' type-name ')'
//
// unary-operator: one of
//    & * + - ~ !

auto parser_unary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "unary expression"));

  // '++' unary-expression
  // '--' unary-expression

  auto incremental_unary_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    auto incremental_operator =
      parser_operator(NodeType::UnaryExpression, [] (TokenIterator t) {
        return t->type == TokenType::Increment
            || t->type == TokenType::Decrement;
      });

    if (auto [incr_it, incr_op] = incremental_operator(parser, begin, end);
        !is_giveup(incr_op))
    {
      auto [unary_it, unary_expr] = parser_unary_expression(parser, incr_it, end);
      add_state(incr_op, giveup_to_expected(std::move(unary_expr), "unary expression"));

      return ParserResult(unary_it, std::move(incr_op));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "unary operator"));
  };

  // '&' cast-expression
  // '~' cast-expression
  // '*' cast-expression
  // '+' cast-expression
  // '-' cast-expression
  // '!' cast-expression

  auto unary_cast_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    auto unary_operator = parser_operator(NodeType::UnaryExpression, [] (TokenIterator t) {
      switch (t->type)
      {
        case TokenType::BitwiseAnd:
        case TokenType::BitwiseNot:
        case TokenType::Times:
        case TokenType::Plus:
        case TokenType::Minus:
        case TokenType::NotEqualTo:
          return true;

        default:
          return false;
      }
    });

    if (auto [unary_it, unary_op] = unary_operator(parser, begin, end);
        !is_giveup(unary_op))
    {
      auto [cast_it, cast_expr] = parser_cast_expression(parser, unary_it, end);
      add_state(unary_op, std::move(cast_expr));

      return ParserResult(cast_it, std::move(unary_op));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "basic unary expression"));
  };

  // '(' type-name ')'

  auto parens_type_name = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin->type == TokenType::LeftParen)
    {
      auto [type_it, type_name] = parser_type_name(parser, std::next(begin), end);
      auto it = type_it;

      if (is<ParserSuccess>(type_name))
      {
        if (expect_end_token(type_name, begin, end, type_it, TokenType::RightParen))
          it = std::next(type_it);

        return ParserResult(it, std::move(type_name));
      }

      return ParserResult(it, std::move(type_name));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "type name inside parentheses"));
  };

  // 'sizeof' unary-expression
  // 'sizeof' '(' type-name ')'
  // '_Alignof' '(' type-name ')'

  auto size_of_production = [&] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin->type == TokenType::Sizeof)
    {
      auto [it, unary_expr] = parser_one_of(parser, std::next(begin), end, "type name, or unary expression",
                                            parens_type_name,
                                            parser_unary_expression);

      ParserState sizeof_op =
        ParserSuccess(std::make_unique<SyntaxTree>(NodeType::UnaryExpression, *begin));

      add_state(sizeof_op, giveup_to_expected(std::move(unary_expr), "unary expression or type name inside parentheses"));

      return ParserResult(it, std::move(sizeof_op));
    }
    else if (begin->type == TokenType::Alignof)
    {
      auto [it, type_name] = parens_type_name(parser, std::next(begin), end);

      ParserState alignof_op =
        ParserSuccess(std::make_unique<SyntaxTree>(NodeType::UnaryExpression, *begin));

      add_state(alignof_op, giveup_to_expected(std::move(type_name), "type name inside parentheses"));

      return ParserResult(it, std::move(alignof_op));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "sizeof/alignof expression"));
  };

  return parser_one_of(parser, begin, end, "unary expression",
                       parser_postfix_expression,
                       incremental_unary_production,
                       size_of_production,
                       unary_cast_production);
}

// cast-expression:
//    unary-expression
//    '(' type-name ')' cast-expression

auto parser_cast_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  auto cast_production = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end && begin->type == TokenType::LeftParen)
    {
      auto [type_it, type_name] = parser_type_name(parser, std::next(begin), end);

      if (!is_giveup(type_name))
      {
        if (expect_end_token(type_name, begin, end, type_it, TokenType::RightParen))
        {
          auto [cast_it, cast_expr] = parser_cast_expression(parser, std::next(type_it), end);

          // Not a compound literal
          if (cast_it != end && cast_it->type != TokenType::LeftCurlyBraces)
          {
            ParserState cast = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::CastExpression));

            add_state(cast, std::move(type_name));
            add_state(cast, giveup_to_expected(std::move(cast_expr), "cast expression"));

            return ParserResult(cast_it, std::move(cast));
          }
        }
      }
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "cast expression"));
  };

  return parser_one_of(parser, begin, end, "cast or unary expression",
                       cast_production,
                       parser_unary_expression);
}

// multiplicative-expression:
//    cast-expression
//    multiplicative-expression multiplicative-operator cast-expression
//
// multiplicative-operator: one of
//    * / %

auto parser_multiplicative_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto multiplicative_operator =
      parser_operator(NodeType::MultiplicativeExpression, [] (TokenIterator t) {
        return t->type == TokenType::Times  ||
               t->type == TokenType::Divide ||
               t->type == TokenType::Percent;
      });

    auto multiplicative_production =
      parser_left_binary_operator(parser_cast_expression,
                                  multiplicative_operator,
                                  parser_cast_expression);

    return multiplicative_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "multiplicative expression"));
}

// additive-expression:
//    multiplicative-expression
//    additive-expression '+' multiplicative-expression
//
// additive-operator: one of
//    + -

auto parser_additive_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto additive_operator = parser_operator(NodeType::AdditiveExpression, [] (TokenIterator t) {
      return t->type == TokenType::Plus ||
             t->type == TokenType::Minus;
    });

    auto additive_production =
      parser_left_binary_operator(parser_multiplicative_expression,
                                  additive_operator,
                                  parser_multiplicative_expression);

    return additive_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "additive expression"));
}

// shift-expression:
//    additive-expression
//    shift-expression shift-operator additive-expression
//
// shift-operator: one of
//    << >>

auto parser_shift_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto shift_operator = parser_operator(NodeType::ShiftExpression, [] (TokenIterator t) {
      return t->type == TokenType::BitwiseLeftShift ||
             t->type == TokenType::BitwiseRightShift;
    });

    auto shift_production =
      parser_left_binary_operator(parser_additive_expression,
                                  shift_operator,
                                  parser_additive_expression);

    return shift_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "shift expression"));
}

// relational-expression:
//    shift-expresion
//    relational-expression relational-operator shift-expression
//
// relational-operator: one of
//    < > <= >=

auto parser_relational_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto relational_operator =
      parser_operator(NodeType::RelationalExpression, [] (TokenIterator t) {
        return t->type == TokenType::LessThan    ||
               t->type == TokenType::GreaterThan ||
               t->type == TokenType::LessEqual   ||
               t->type == TokenType::GreaterEqual;
      });

    auto relational_production =
      parser_left_binary_operator(parser_shift_expression,
                                  relational_operator,
                                  parser_shift_expression);

    return relational_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "relational expression"));
}

// equality-expression:
//    relational-expression
//    equality-expression equality-operator relational-expression
//
// equality-operator: one of
//  == !=

auto parser_equality_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto equality_operator =
      parser_operator(NodeType::EqualityExpression, [] (TokenIterator t) {
        return t->type == TokenType::EqualsTo ||
               t->type == TokenType::NotEqualTo;
      });

    auto equality_production =
      parser_left_binary_operator(parser_relational_expression,
                                  equality_operator,
                                  parser_relational_expression);

    return equality_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "equality expression"));
}

// and-expression:
//    equality-expression
//    and-expression '&' equality-expression

auto parser_and_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto and_operator =
      parser_operator(NodeType::AndExpression, [] (TokenIterator t) {
        return t->type == TokenType::BitwiseAnd;
      });

    auto and_production =
      parser_left_binary_operator(parser_equality_expression,
                                  and_operator,
                                  parser_equality_expression);

    return and_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "and expression"));
}

// exclusive-or-expression:
//    and-expression
//    exclusive-or-expression '^' and-expression

auto parser_exclusive_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto exclusive_or_operator =
      parser_operator(NodeType::ExclusiveOrExpression, [] (TokenIterator t) {
        return t->type == TokenType::BitwiseXor;
      });

    auto exclusive_or_production =
      parser_left_binary_operator(parser_and_expression,
                                  exclusive_or_operator,
                                  parser_and_expression);

    return exclusive_or_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "exclusive or expression"));
}

// inclusive-or-expression:
//    exclusive-or-expression
//    inclusive-or-expression '|' exclusive-or-expression

auto parser_inclusive_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto inclusive_or_operator =
      parser_operator(NodeType::InclusiveOrExpression, [] (TokenIterator t) {
        return t->type == TokenType::BitwiseOr;
      });

    auto inclusive_or_production =
      parser_left_binary_operator(parser_exclusive_or_expression,
                                  inclusive_or_operator,
                                  parser_exclusive_or_expression);

    return inclusive_or_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "inclusive or expression"));
}

// logical-and-expression:
//    inclusive-or-expression
//    logical-and-expression '&&' inclusive-or-expression

auto parser_logical_and_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto logical_and_operator =
      parser_operator(NodeType::LogicalAndExpression, [] (TokenIterator t) {
        return t->type == TokenType::LogicalAnd;
      });

    auto logical_and_production =
      parser_left_binary_operator(parser_inclusive_or_expression,
                                  logical_and_operator,
                                  parser_inclusive_or_expression);

    return logical_and_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "logical and expression"));
}

// logical-or-expression:
//    logical-and-expression
//    logical-or-expression '||' logical-and-expression

auto parser_logical_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto logical_or_operator =
      parser_operator(NodeType::LogicalOrExpression, [] (TokenIterator t) {
        return t->type == TokenType::LogicalOr;
      });

    auto logical_or_production =
      parser_left_binary_operator(parser_logical_and_expression,
                                  logical_or_operator,
                                  parser_logical_and_expression);

    return logical_or_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "logical or expression"));
}

// conditional-expression:
//    logical-or-expression ('?' expression ':' conditional-expression)?

auto parser_conditional_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto [or_it, or_expr] = parser_logical_or_expression(parser, begin, end);

    if (!is_giveup(or_expr))
    {
      if (or_it != end && or_it->type == TokenType::QuestionMark)
      {
        const auto ternary_op_it = or_it;

        // Parses a ternary operator.
        ParserState condition = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ConditionalExpression));
        add_state(condition, std::move(or_expr));

        auto [true_it, true_expr] = parser_expression(parser, std::next(ternary_op_it), end);
        add_state(condition, giveup_to_expected(std::move(true_expr), "expression"));

        if (expect_end_token(condition, ternary_op_it, end, true_it, TokenType::Colon))
        {
          auto [false_it, false_expr] = parser_conditional_expression(parser, std::next(true_it), end);
          add_state(condition, giveup_to_expected(std::move(false_expr), "expression"));

          return ParserResult(false_it, std::move(condition));
        }
        else
        {
          return ParserResult(true_it, std::move(condition));
        }
      }

      return ParserResult(or_it, std::move(or_expr));
    }
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "conditional expression"));
}

// assignment-expression:
//    conditional-expression
//    unary-expression assignment-operator assignment-expression
//
// assignment-operator: one of
//    = *= /= %= += -= <<= >>= &= ^= |=

auto parser_assignment_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "assignment expression"));

  auto assign_operator = parser_operator(NodeType::AssignmentExpression, [] (TokenIterator t) {
    switch (t->type)
    {
      case TokenType::Assign:
      case TokenType::TimesAssign:
      case TokenType::DivideAssign:
      case TokenType::ModuloAssign:
      case TokenType::PlusAssign:
      case TokenType::MinusAssign:
      case TokenType::BitwiseLeftShiftAssign:
      case TokenType::BitwiseRightShiftAssign:
      case TokenType::BitwiseAndAssign:
      case TokenType::BitwiseXorAssign:
      case TokenType::BitwiseOrAssign:
        return true;

      default:
        return false;
    }
  });

  auto assignment_expr_production =
    parser_right_binary_operator(parser_unary_expression,
                                 assign_operator,
                                 parser_assignment_expression);

  return parser_one_of(parser, begin, end, "assignment or conditional expression",
                       assignment_expr_production,
                       parser_conditional_expression);
}

// expression:
//    assignment-expression
//    expression ',' assignment-expression

auto parser_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end)
  {
    auto comma_operator = parser_operator(NodeType::Expression, [] (TokenIterator t) {
      return t->type == TokenType::Comma;
    });

    auto expr_production =
      parser_left_binary_operator(parser_assignment_expression,
                                  comma_operator,
                                  parser_assignment_expression);

    return expr_production(parser, begin, end);
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "expression"));
}

// primary-expression:
//    identifier
//    constant
//    string-literal+
//    '(' expression ')'

auto parser_primary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  auto parser_parens_expr = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin != end && begin->type == TokenType::LeftParen)
    {
      auto [it, expr] = parser_expression(parser, std::next(begin), end);

      if (!is_giveup(expr) && expect_end_token(expr, begin, end, it, TokenType::RightParen))
        std::advance(it, 1);

      return ParserResult(it, giveup_to_expected(std::move(expr), "expression"));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "expression"));
  };

  return parser_one_of(parser, begin, end, "primary expression",
                       parser_identifier,
                       parser_constant,
                       parser_string_literal_list,
                       parser_parens_expr);
}

} // namespace

// TODO
auto SyntaxTree::parse(ProgramContext& program, const TokenStream& tokens)
  -> std::unique_ptr<SyntaxTree>
{
  ParserContext parser{program, tokens};

  auto result = parser_primary_expression(parser, tokens.begin(), tokens.end());
  result.state = giveup_to_expected(std::move(result.state));

  if (is<ParserSuccess>(result.state))
  {
    auto tree = std::move(get<ParserSuccess>(std::move(result.state)).tree);

    return tree;
  }
  else
  {
    for (const auto& fail : get<ParserFailure>(result.state))
    {
      TokenIterator context = fail.where == tokens.end()
        ? std::prev(fail.where) //< std::prev(tokens.end())->type == TokenType::Eof
        : fail.where;

      if (fail.status == ParserStatus::ErrorNote)
        parser.note(context, "{}", fail.error.c_str());
      else
        parser.error(context, "{}", fail.error.c_str());
    }

    return nullptr;
  }
}

void SyntaxTree::dump(std::FILE* out, size_t indent_level) const
{
  auto indent = std::string(indent_level * 2, ' ');
  auto type = to_string(this->type());

  if (this->has_text())
  {
    auto text = fmt::StringRef(this->text().begin(), this->text().size());
    fmt::print(out, "{}{}({}){}\n", indent, type, text, this->child_count() ? ":" : "");
  }
  else
  {
    fmt::print(out, "{}{}{}\n", indent, type, this->child_count() ? ":" : "");
  }

  for (const auto& child : *this)
  {
    child->dump(out, indent_level + 1);
  }
}

auto to_string(const NodeType node_type) -> const char*
{
  switch (node_type)
  {
    case NodeType::PrimaryExpression:
      return "primary expression";
    case NodeType::GenericSelection:
      return "generic selection";
    case NodeType::GenericAssocList:
      return "generic assoc list";
    case NodeType::GenericAssociation:
      return "generic association";
    case NodeType::PostfixExpression:
      return "postfix expression";
    case NodeType::ArgumentExpressionList:
      return "argument expression list";
    case NodeType::UnaryExpression:
      return "unary expression";
    case NodeType::CastExpression:
      return "cast expression";
    case NodeType::MultiplicativeExpression:
      return "multiplicative expression";
    case NodeType::AdditiveExpression:
      return "additive expression";
    case NodeType::ShiftExpression:
      return "shift expression";
    case NodeType::RelationalExpression:
      return "relatioal expression";
    case NodeType::EqualityExpression:
      return "equality expression";
    case NodeType::AndExpression:
      return "and expression";
    case NodeType::ExclusiveOrExpression:
      return "exclusive or expression";
    case NodeType::InclusiveOrExpression:
      return "inclusive or expression";
    case NodeType::LogicalAndExpression:
      return "logical and expression";
    case NodeType::LogicalOrExpression:
      return "logical or expression";
    case NodeType::ConditionalExpression:
      return "conditional expression";
    case NodeType::AssignmentExpression:
      return "assignment expression";
    case NodeType::Expression:
      return "expression";
    case NodeType::ConstantExpression:
      return "constant expression";
    case NodeType::Declaration:
      return "declaration";
    case NodeType::DeclarationSpecifiers:
      return "declaration specifiers";
    case NodeType::DeclarationSpecifier:
      return "declaration specifier";
    case NodeType::InitDeclaratorList:
      return "init declarator list";
    case NodeType::InitDeclarator:
      return "init declarator";
    case NodeType::StorageClassSpecifier:
      return "storage class specifier";
    case NodeType::TypeSpecifier:
      return "type specifier";
    case NodeType::StructOrUnionSpecifier:
      return "struct or union specifier";
    case NodeType::StructOrUnion:
      return "struct or union";
    case NodeType::StructDeclarationList:
      return "struct declaration list";
    case NodeType::StructDeclaration:
      return "struct declaration";
    case NodeType::SpecifierQualifierList:
      return "specifier qualifier list";
    case NodeType::StructDeclaratorList:
      return "struct declarator list";
    case NodeType::StructDeclarator:
      return "struct declarator";
    case NodeType::EnumSpecifier:
      return "enum specifier";
    case NodeType::EnumeratorList:
      return "enumerator list";
    case NodeType::Enumerator:
      return "enumerator";
    case NodeType::AtomicTypeSpecifier:
      return "atomic type specifier";
    case NodeType::TypeQualifier:
      return "type qualifier";
    case NodeType::FunctionSpecifier:
      return "function specifier";
    case NodeType::AlignmentSpecifier:
      return "alignment specifier";
    case NodeType::Declarator:
      return "declarator";
    case NodeType::DirectDeclarator:
      return "direct declarator";
    case NodeType::NestedParenthesesBlock:
      return "nested parentheses block";
    case NodeType::Pointer:
      return "pointer";
    case NodeType::TypeQualifierList:
      return "type qualifier list";
    case NodeType::ParameterTypeList:
      return "parameter type list";
    case NodeType::ParameterList:
      return "parameter list";
    case NodeType::ParameterDeclaration:
      return "parameter declaration";
    case NodeType::IdentifierList:
      return "identifier list";
    case NodeType::TypeName:
      return "type name";
    case NodeType::AbstractDeclarator:
      return "abstract declarator";
    case NodeType::DirectAbstractDeclarator:
      return "direct abstract declarator";
    case NodeType::TypedefName:
      return "typedef name";
    case NodeType::Initializer:
      return "initializer";
    case NodeType::InitializerList:
      return "initializer list";
    case NodeType::Designation:
      return "designation";
    case NodeType::DesignatorList:
      return "designator list";
    case NodeType::Designator:
      return "designator";
    case NodeType::StaticAssertDeclaration:
      return "static assert declaration";
    case NodeType::Statement:
      return "statement";
    case NodeType::LabeledStatement:
      return "labeled statement";
    case NodeType::CompoundStatement:
      return "compound statement";
    case NodeType::BlockItemList:
      return "block item list";
    case NodeType::BlockItem:
      return "block item";
    case NodeType::ExpressionStatement:
      return "expression statement";
    case NodeType::SelectionStatement:
      return "selection statement";
    case NodeType::IterationStatement:
      return "iteration statement";
    case NodeType::JumpStatement:
      return "jump statement";
    case NodeType::CompilationUnit:
      return "compilation unit";
    case NodeType::TranslationUnit:
      return "translation unit";
    case NodeType::ExternalDeclaration:
      return "external declaration";
    case NodeType::FunctionDefinition:
      return "function definition";
    case NodeType::DeclarationList:
      return "declaration list";
    case NodeType::Identifier:
      return "identifier";
    case NodeType::Constant:
      return "constant";
    case NodeType::IntegerConstant:
      return "integer constant";
    case NodeType::FloatingConstant:
      return "floating constant";
    case NodeType::EnumerationConstant:
      return "enumeration constant";
    case NodeType::CharacterConstant:
      return "character constant";
    case NodeType::EncodingPrefix:
      return "encoding prefix";
    case NodeType::StringLiteral:
      return "string literal";
    case NodeType::StringLiteralList:
      return "string literal list";
    case NodeType::AsmBlock:
      return "asm block";
    case NodeType::CompoundLiteral:
      return "compound literal";
    case NodeType::ArraySubscripting:
      return "array subscripting";
    case NodeType::FunctionCall:
      return "function call";
    case NodeType::MemberAccess:
      return "member access";
    case NodeType::PointerMemberAccess:
      return "pointer member access";
    case NodeType::PostfixIncrement:
      return "postfix increment";
    case NodeType::PostfixDecrement:
      return "postfix decrement";
    case NodeType::PointerDeclarator:
      return "pointer declarator";
    case NodeType::ArrayDeclarator:
      return "array declarator";
    case NodeType::ArrayStaticDeclarator:
      return "array (with static) declarator";
    case NodeType::ArrayVLADeclarator:
      return "variable length array declarator";
    case NodeType::FunctionDeclarator:
      return "function declarator";
    case NodeType::None:
      Unreachable();
  }

  throw std::logic_error(
      fmt::format("missing implementation of enumerator '{}' for to_string(NodeType)", static_cast<int>(node_type)));
}

} // namespace ccompiler
