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

  explicit ParserError(ParserStatus status, TokenIterator where) noexcept
    : status(status), where(where)
  {}

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
        add_error(new_state, ParserError(ParserStatus::Error, e.where, fmt::format("expected {}", what.data())));
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
                   Rule rule, Rules&&... rules)
  -> ParserResult
{
  auto [it, state] = rule(parser, begin, end);

  if (!is_giveup(state))
    return ParserResult(it, std::move(state));

  if constexpr (sizeof...(rules) > 0)
  {
    return parser_one_of(parser, begin, end, std::forward<Rules>(rules)...);
  }
  else
  {
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin));
  }
}

template <typename Rule, typename Predicate>
auto parser_many(ParserContext& parser, TokenIterator begin, TokenIterator end,
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
auto parser_one_many(ParserContext& parser, TokenIterator begin, TokenIterator end,
                     Rule rule, Predicate pred)
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

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin));
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
        auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);

        if (is<ParserSuccess>(rhs_state))
          lhs_it = rhs_it;
        else if (lhs_it != end)
          std::advance(lhs_it, 1);

        add_state(op_state, giveup_to_expected(std::move(lhs_state)));
        add_state(op_state, giveup_to_expected(std::move(rhs_state)));

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
          auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);

          add_state(op_state, std::move(lhs_state));
          add_state(op_state, giveup_to_expected(std::move(rhs_state)));

          return ParserResult(rhs_it, std::move(op_state));
        }
      }
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "binary operator"));
  };
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


#if 0

// type-name:
//    specifier-qualifier-list abstract-declarator?

// TODO
auto parser_type_name(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin != end && begin->type == TokenType::VoidType)
  {
    return ParserResult(std::next(begin), ParserSuccess(std::make_unique<SyntaxTree>(NodeType::TypeName, *begin)));
  }

  return ParserResult(end, make_error(ParserStatus::GiveUp, begin));
}

// postfix-expression:
//    primary-expression
//    postfix-expression '[' expression ']'
//    postfix-expression '(' argument-expression-list? ')'
//    postfix-expression '.' identifier
//    postfix-expression '->' identifier
//    postfix-expression '++'
//    postfix-expression '--'
//    '(' type-name ')' '{' initializer-list '}'
//    '(' type-name ')' '{' initializer-list ',' '}'
//    '__extension__' '(' type-name ')' '{' initializer-list '}'
//    '__extension__' '(' type-name ')' '{' initializer-list ',' '}'

// TODO
auto parser_postfix_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_primary_expression(parser, begin, end);
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

// TODO
auto parser_unary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end));

  auto parser_type_name_braces =
    parser_make_bounded_expression(TokenType::LeftParen, parser_type_name, TokenType::RightParen);

  auto parser_incr_decr_expr = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    auto incr_decr_operator =
      parser_make_operator(NodeType::UnaryExpression, [] (TokenIterator t) {
        return t->type == TokenType::Increment
            || t->type == TokenType::Decrement;
        });

    auto rule = parser_make_unary_expr_prefix(incr_decr_operator,
                                              parser_unary_expression);
    return rule(parser, begin, end);
  };

  auto parser_unary_op_cast_expr = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    auto unary_operator =
      parser_make_operator(NodeType::UnaryExpression, [] (TokenIterator token) {
        switch (token->type)
        {
          case TokenType::BitwiseAnd:
          case TokenType::BitwiseNot:
          case TokenType::Times:
          case TokenType::Plus:
          case TokenType::NotEqualTo:
            return true;

          default:
            return true;
        }
      });

    auto rule = parser_make_unary_expr_prefix(unary_operator, parser_cast_expression);

    return rule(parser, begin, end);
  };

  auto parser_sizeof = [parser_type_name_braces] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    auto sizeof_operator =
      parser_make_operator(NodeType::UnaryExpression, [] (TokenIterator t) { return t->type == TokenType::Sizeof; });

    auto rule =
      parser_make_unary_expr_prefix(sizeof_operator,
                                    parser_make_one_of(
                                      parser_type_name_braces,
                                      parser_unary_expression));
    return rule(parser, begin, end);
  };

  auto parser_alignof = [parser_type_name_braces] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto alignof_operator =
      parser_make_operator(NodeType::UnaryExpression, [] (TokenIterator t) { return t->type == TokenType::Alignof; });

    auto rule =
      parser_make_unary_expr_prefix(alignof_operator,
                                    parser_type_name_braces);
    return rule(parser, begin, end);
  };

  auto result = parser_one_of(parser, begin, end,
                              parser_postfix_expression,
                              parser_incr_decr_expr,
                              parser_unary_op_cast_expr,
                              parser_sizeof,
                              parser_alignof);

  return result;
}

// cast-expression:
//    unary-expression
//    '(' type-name ')' cast-expression

// TODO
auto parser_cast_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end));

  auto parser_cast_expr = [] (ParserContext& parser, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    auto parser_type_name_braces =
      parser_make_bounded_expression(TokenType::LeftParen,
                                     parser_type_name,
                                     TokenType::RightParen);

    auto [it, type_state] = parser_type_name_braces(parser, begin, end);

    ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::CastExpression));
    add_state(state, giveup_to_expected(std::move(type_state), NodeType::TypeName));

    if (is<ParserSuccess>(state))
    {
      auto [cast_it, cast_state] = parser_cast_expression(parser, it, end);
      add_state(state, giveup_to_expected(std::move(cast_state), NodeType::CastExpression));

      return ParserResult(cast_it, std::move(state));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, it));
  };

  return parser_one_of(parser, begin, end,
                       parser_cast_expr,
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
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end));

  auto parser_multiplicative_operator =
    parser_make_operator(NodeType::MultiplicativeExpression, [] (TokenIterator t) {
        return t->type == TokenType::Times  ||
               t->type == TokenType::Divide ||
               t->type == TokenType::Percent;
      });

  auto parser_multiplicative_expr =
    parser_left_associate(parser_cast_expression,
                          parser_multiplicative_operator,
                          parser_cast_expression);

  return parser_multiplicative_expr(parser, begin, end);
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
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end));

  auto parser_additive_operator =
    parser_make_operator(NodeType::AdditiveExpression, [] (TokenIterator t) {
        return t->type == TokenType::Plus ||
               t->type == TokenType::Minus;
      });

  auto parser_additive_expr =
    parser_left_associate(parser_multiplicative_expression,
                          parser_additive_operator,
                          parser_multiplicative_expression);

  return parser_additive_expr(parser, begin, end);
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
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "shift expression"));

  auto parser_shift_operator =
    parser_make_operator(NodeType::ShiftExpression, [] (TokenIterator t) {
          return t->type == TokenType::BitwiseLeftShift ||
                 t->type == TokenType::BitwiseRightShift;
      });

  auto parser_shift_expr =
    parser_left_associate(parser_additive_expression,
                          parser_shift_operator,
                          parser_additive_expression);

  return parser_shift_expr(parser, begin, end);
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
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "relational expression"));

  auto parser_relational_operator =
    parser_make_operator(NodeType::RelationalExpression, [] (TokenIterator t) {
        switch (t->type)
        {
          case TokenType::LessThan:
          case TokenType::GreaterThan:
          case TokenType::LessEqual:
          case TokenType::GreaterEqual:
            return true;

          default:
            return false;
        }
      });

  auto parser_relational_expr =
    parser_left_associate(parser_shift_expression,
                          parser_relational_operator,
                          parser_shift_expression);

  return parser_relational_expr(parser, begin, end);
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
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "equality expression"));

  auto parser_equality_operator =
    parser_make_operator(NodeType::EqualityExpression, [] (TokenIterator t) {
        return t->type == TokenType::EqualsTo ||
               t->type == TokenType::NotEqualTo;
      });

  auto parser_equality_expr =
    parser_left_associate(parser_relational_expression,
                          parser_equality_operator,
                          parser_relational_expression);

  return parser_equality_expr(parser, begin, end);
}

// and-expression:
//    equality-expression
//    and-expression '&' equality-expression

auto parser_and_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "and expression"));

  auto parser_and_operator =
    parser_make_operator(NodeType::AndExpression, [] (TokenIterator it) {
        return it->type == TokenType::BitwiseAnd;
      });

  auto parser_and_expr =
    parser_left_associate(parser_equality_expression,
                          parser_and_operator,
                          parser_equality_expression);

  return parser_and_expr(parser, begin, end);
}

// exclusive-or-expression:
//    and-expression
//    exclusive-or-expression '^' and-expression

auto parser_exclusive_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "exclusive or expression"));

  auto parser_exclusive_or_operator =
    parser_make_operator(NodeType::ExclusiveOrExpression, [] (TokenIterator it) {
        return it->type == TokenType::BitwiseXor;
      });

  auto parser_exclusive_or_expr =
    parser_left_associate(parser_and_expression,
                          parser_exclusive_or_operator,
                          parser_and_expression);

  return parser_exclusive_or_expr(parser, begin, end);
}

// inclusive-or-expression:
//    exclusive-or-expression
//    inclusive-or-expression '|' exclusive-or-expression

auto parser_inclusive_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "inclusive or expression"));

  auto parser_inclusive_or_operator =
    parser_make_operator(NodeType::InclusiveOrExpression, [] (TokenIterator it) {
        return it->type == TokenType::BitwiseOr;
      });

  auto parser_inclusive_or_expr =
    parser_left_associate(parser_exclusive_or_expression,
                          parser_inclusive_or_operator,
                          parser_exclusive_or_expression);

  return parser_inclusive_or_expr(parser, begin, end);
}

// logical-and-expression:
//    inclusive-or-expression
//    logical-and-expression '&&' inclusive-or-expression

auto parser_logical_and_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "logical and expression"));

  auto parser_logical_and_operator =
    parser_make_operator(NodeType::LogicalAndExpression, [] (TokenIterator it) {
        return it->type == TokenType::LogicalAnd;
      });

  auto parser_logical_and_expr =
    parser_left_associate(parser_inclusive_or_expression,
                          parser_logical_and_operator,
                          parser_inclusive_or_expression);

  return parser_logical_and_expr(parser, begin, end);
}

// logical-or-expression:
//    logical-and-expression
//    logical-or-expression '||' logical-and-expression

auto parser_logical_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::GiveUp, end, "logical or expression"));

  auto parser_logical_or_operator =
    parser_make_operator(NodeType::LogicalOrExpression, [] (TokenIterator it) {
        return it->type == TokenType::LogicalOr;
      });

  auto parser_logical_or_expr =
    parser_left_associate(parser_logical_and_expression,
                          parser_logical_or_operator,
                          parser_logical_and_expression);

  return parser_logical_or_expr(parser, begin, end);
}

#endif

inline namespace v2
{

auto parser_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_primary_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

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
  auto [it, strings] = parser_one_many(parser, begin, end, parser_string_literal, [] (TokenIterator t) {
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
    ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::Constant));
    add_node(state, std::make_unique<SyntaxTree>(const_type, *begin));

    return ParserResult(std::next(begin), std::move(state));
  }
  else
  {
    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "constant"));
  }
}

auto parser_unary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_primary_expression(parser, begin, end);
}

auto parser_logical_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return parser_primary_expression(parser, begin, end);
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

        // Parse a ternary operator.
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

  auto parser_assignment_expr = parser_right_binary_operator(parser_unary_expression,
                                                             assign_operator,
                                                             parser_assignment_expression);

  auto [it, state] = parser_one_of(parser, begin, end,
                                   parser_assignment_expr,
                                   parser_conditional_expression);

  return ParserResult(it, giveup_to_expected(std::move(state), "assignment expression"));
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

      if (!is_giveup(expr))
        expect_end_token(expr, begin, end, it, TokenType::RightParen);

      return ParserResult(std::next(it), giveup_to_expected(std::move(expr), "expression"));
    }

    return ParserResult(end, make_error(ParserStatus::GiveUp, begin, "expression"));
  };

  auto [it, state] = parser_one_of(parser, begin, end,
                                   parser_identifier,
                                   parser_constant,
                                   parser_string_literal_list,
                                   parser_parens_expr);

  return ParserResult(it, giveup_to_expected(std::move(state), "primary expression"));
}

} // namespace v2

} // namespace

// TODO
auto SyntaxTree::parse(ProgramContext& program, const TokenStream& tokens)
  -> std::unique_ptr<SyntaxTree>
{
  ParserContext parser{program, tokens};

  auto result = parser_primary_expression(parser, tokens.begin(), tokens.end());

  if (is<ParserSuccess>(result.state))
  {
    auto tree = std::move(get<ParserSuccess>(std::move(result.state)).tree);

    return tree;
  }
  else
  {
    for (const auto& fail : get<ParserFailure>(result.state))
    {
      if (fail.where == tokens.end())
      {
        if (fail.status == ParserStatus::ErrorNote)
          parser.program.note(no_context, "{}", fail.error.c_str());
        else
          parser.program.error(no_context, "{}", fail.error.c_str());
      }
      else
      {
        if (fail.status == ParserStatus::ErrorNote)
          parser.note(fail.where, "{}", fail.error.c_str());
        else
          parser.error(fail.where, "{}", fail.error.c_str());
      }
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
    default:
      Unreachable();
  }
}

} // namespace ccompiler
