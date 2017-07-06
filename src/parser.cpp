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
// PlainWrong means a rule is not able
// to start parsing. This is useful in helper functions,
// such as `parser_one_of`, where if all rules are plain wrong,
// then it's safe to assume an "expected one of" kind of error.
enum class ParserStatus
{
  Error,      //< Syntax is partly wrong.
  ErrorNote,  //< Note following a ParserStatus::Error.
  PlainWrong, //< Parser couldn't make sense at all.
};

// Represents an error when some parser
// couldn't generate an AST.
struct ParserError
{
  ParserStatus status;
  TokenIterator where;  //< Where it happened.
  std::string error;    //< Error message/explanation.

  explicit ParserError(TokenIterator where) noexcept
    : status(ParserStatus::PlainWrong), where(where), error()
  {}

  explicit ParserError(TokenIterator where, std::string error) noexcept
    : status(ParserStatus::PlainWrong), where(where), error(std::move(error))
  {}

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

// Checks whether there's a plain-wrong error in `state`.
auto is_plain_wrong(const ParserState& state) -> bool
{
  if (is<ParserFailure>(state))
  {
    for (const auto& fail : get<ParserFailure>(state))
    {
      if (fail.status == ParserStatus::PlainWrong)
      {
        return true;
      }
    }
  }

  return false;
}

void expect_one_of(ParserState& state, string_view what)
{
  ParserState new_errors = ParserFailure();

  if (is<ParserFailure>(state))
  {
    for (auto& fail : get<ParserFailure>(state))
    {
      if (fail.status == ParserStatus::PlainWrong)
      {
        add_error(new_errors, ParserError(fail.where, fmt::format("expected {}", what.data())));
      }
      else
      {
        add_error(new_errors, std::move(fail));
      }
    }

    state = std::move(new_errors);
  }
}

void expect_one_of(ParserState& state, NodeType node_type)
{
  return expect_one_of(state, to_string(node_type));
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
    add_error(state, get<ParserFailure>(std::move(other)));
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
// Otherwise, if all rules return plain-wrong errors, then the result
// is "expected one of" plain-wrong error.
template <typename Rule, typename... Rules>
auto parser_one_of(ParserContext& parser, TokenIterator begin,
                   TokenIterator end, Rule rule, Rules&&... rules)
  -> ParserResult
{
  auto result = rule(parser, begin, end);

  if (!is_plain_wrong(result.state))
  {
    return result;
  }

  if constexpr (sizeof...(rules) > 0)
  {
    return parser_one_of(parser, begin, end, std::forward<Rules>(rules)...);
  }
  else
  {
    return ParserResult(end, make_error(ParserStatus::PlainWrong, begin));
  }
}

// Parses a sequence of rules.
template <typename... Rules>
auto parser_all_of(ParserContext& parser, TokenIterator begin,
                   TokenIterator end, Rules&&... rules)
  -> ParserResult
{
  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>());
  auto it = begin;

  auto add_result = [&] (ParserResult result)
  {
    it = result.next_token;
    add_state(state, std::move(result.state));
  };

  (add_result(std::forward<Rules>(rules)(parser, it, end)), ...);

  return ParserResult(it, std::move(state));
}

// Parses `rule` while condition is true.
template <typename Rule, typename Condition>
auto parser_take_while(ParserContext& parser, TokenIterator begin,
                       TokenIterator end, Rule rule, Condition cond)
  -> ParserResult
{
  auto it = begin;
  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>());

  while (it != end && cond(it))
  {
    auto result = rule(parser, it, end);

    add_state(state, std::move(result.state));
    it = result.next_token;
  }

  return ParserResult(it, std::move(state));
}

// Parses `rule` once, then keep on parsing while condition is true.
template <typename Rule, typename Condition>
auto parser_take_repeat(ParserContext& parser, TokenIterator begin,
                        TokenIterator end, Rule rule, Condition cond)
  -> ParserResult
{
  auto it = begin;
  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>());

  while (it != end)
  {
    auto result = rule(parser, it, end);

    add_state(state, std::move(result.state));
    it = result.next_token;

    if (it == end || !cond(it))
    {
      break;
    }
  }

  return ParserResult(it, std::move(state));
}

auto expect_token(ParserState& state, TokenIterator it,
                  TokenIterator end, TokenType token_type)
  -> bool
{
  if (it == end || it->type != token_type)
  {
    it = (it == end) ? std::prev(it) : it;
    std::string msg = fmt::format("expected '{}', got '{}'", to_string(token_type), string_ref(it->data));
    add_error(state, make_error(ParserStatus::Error, it, std::move(msg)));
    return false;
  }

  return true;
}

auto expect_end_token(ParserState& state, TokenIterator begin,
                      TokenIterator end, TokenIterator it, TokenType end_token)
  -> bool
{
  if (it == end || it->type != end_token)
  {
    it = (it == end) ? std::prev(it) : it;
    add_error(state, make_error(ParserStatus::Error, it, fmt::format("expected '{}'", to_string(end_token))));
    add_error(state, make_error(ParserStatus::ErrorNote, begin, fmt::format("to match this '{}'", to_string(begin->type))));
    return false;
  }

  return true;
}

// Parses the token sequence into a left-to-right associated tree.
//
// E.g.: the following sequence of tokens:
//
//    A op B op C
//
// Parses into:
//
//    node(op):
//      node(op):
//        node(A)
//        node(B)
//      node(C)
template <typename LhsRule, typename OpRule, typename RhsRule>
auto parser_left_associate(LhsRule lhs_rule, OpRule op_rule, RhsRule rhs_rule)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto [it, lhs_state] = lhs_rule(parser, begin, end);

    while (true)
    {
      auto [op_it, op_state] = op_rule(parser, it, end);

      if (is<ParserSuccess>(op_state))
      {
        auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);

        add_state(op_state, std::move(lhs_state));
        add_state(op_state, std::move(rhs_state));

        it = rhs_it;
        lhs_state = std::move(op_state);
      }
      else
      {
        break;
      }
    }

    return ParserResult(it, std::move(lhs_state));
  };
}

// Returns a parser rule that matches a binary expression.
//
// binary-expression:
//    lhs_rule op_rule rhs_rule -> ^($op_rule $lhs_rule $rhs_rule)
template <typename LhsRule, typename OpRule, typename RhsRule>
auto make_binary_expr(LhsRule lhs_rule, OpRule op_rule, RhsRule rhs_rule)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto [lhs_it, lhs_state] = lhs_rule(parser, begin, end);
    auto [op_it, op_state] = op_rule(parser, lhs_it, end);
    auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);

    add_state(op_state, std::move(lhs_state));
    add_state(op_state, std::move(rhs_state));

    return ParserResult(rhs_it, std::move(op_state));
  };
}

// Returns a parser rule that matches a unary expression (prefix).
//
// binary-expression
//    op_rule rhs_rule -> ^($op_rule $rhs_rule)
template <typename OpRule, typename RhsRule>
auto make_unary_expr_prefix(OpRule op_rule, RhsRule rhs_rule)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto [op_it, op_state] = op_rule(parser, begin, end);
    auto [rhs_it, rhs_state] = rhs_rule(parser, op_it, end);

    add_state(op_state, std::move(rhs_state));

    return ParserResult(rhs_it, std::move(op_state));
  };
}

// Returns a parser rule that matches a unary expression (postfix).
//
// binary-expression
//    lhs_rule op_rule -> ^($op_rule $lhs_rule)
template <typename OpRule, typename LhsRule>
auto make_unary_expr_postfix(LhsRule lhs_rule, OpRule op_rule)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto [lhs_it, lhs_state] = lhs_rule(parser, begin, end);
    auto [op_it, op_state] = op_rule(parser, lhs_it, end);

    add_state(op_state, std::move(lhs_state));

    return ParserResult(op_it, std::move(op_state));
  };
}

// Returns a parser rule that matches an operator, or token.
template <typename OpMatch>
auto parser_make_operator(NodeType op_type, OpMatch op_match)
{
  return [=] (ParserContext& /*parser*/, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    if (begin == end)
      return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

    ParserState state = ParserSuccess(nullptr);

    if (op_match(begin))
    {
      add_node(state, std::make_unique<SyntaxTree>(op_type, *begin));
    }
    else
    {
      add_error(state, make_error(begin, to_string(op_type)));
    }

    return ParserResult(std::next(begin), std::move(state));
  };
}

// Parses a rule repeatedly separated by `token`.
template <typename Rule>
auto parser_make_sequence_list(NodeType node_type, Rule rule, TokenType token)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto [it, list_state] = parser_take_repeat(parser, begin, end, rule, [token] (TokenIterator& it) {
        if (it->type == token)
        {
          std::advance(it, 1);
          return true;
        }
        return false;
      });

    if (is<ParserSuccess>(list_state))
    {
      auto& tree = get<ParserSuccess>(list_state).tree;

      if (tree->child_count() == 1)
      {
        auto child = tree->pop_child();
        return ParserResult(it, ParserSuccess(std::move(child)));
      }
    }

    ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(node_type));
    add_state(state, std::move(list_state));

    return ParserResult(it, std::move(state));
  };
}

template <typename Rule>
auto parser_make_inside_brackets(TokenType lhs_token, Rule rule, TokenType rhs_token)
{
  return [=] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    ParserState state = ParserSuccess(nullptr);
    auto it = begin;

    if (it->type == lhs_token)
    {
      std::advance(it, 1);
      auto [rule_it, rule_state] = rule(parser, it, end);

      add_state(state, std::move(rule_state));
      it = rule_it;

      if (expect_end_token(state, begin, end, it, rhs_token))
      {
        std::advance(it, 1);
      }
    }
    else
    {
      add_error(state, ParserError(ParserStatus::PlainWrong, begin));
    }

    return ParserResult(it, std::move(state));
  };
}

// C grammar

auto parser_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;
auto parser_primary_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

// identifier:
//    [a-zA-Z_$] ([a-zA-Z_$] | [0-9])*

auto parser_identifier(ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  // XXX: Means I'm an encoding prefix.
  // Should remove this once NodeType::EncodingPrefix is implemented.
  // NOTE: See parser_string_literal.
  if (std::next(begin) != end && std::next(begin)->type == TokenType::StringConstant)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  ParserState state = ParserSuccess(nullptr);

  if (begin->type == TokenType::Identifier)
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::Identifier, *begin);
    state = ParserSuccess(std::move(tree));
  }
  else
  {
    add_error(state, make_error(begin, "identifier"));
  }

  return ParserResult(std::next(begin), std::move(state));
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
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  ParserState state = ParserSuccess(nullptr);
  optional<TokenData> prefix = nullopt;
  auto it = begin;

  // Encoding prefix.
  if (it->type == TokenType::Identifier)
  {
    string_view token = it->data;

    if (token == "u8" || token == "u" || token == "U" || token == "L")
    {
      prefix = *it;
      std::advance(it, 1);
    }
    else
    {
      add_error(state, make_error(it, "encoding prefix"));
    }
  }

  if (it->type == TokenType::StringConstant)
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::StringLiteral, *it);

    // TODO: Turn it into an annotation.
    if (prefix)
    {
      tree->add_child(std::make_unique<SyntaxTree>(NodeType::EncodingPrefix, *prefix));
    }

    add_node(state, std::move(tree));
  }
  else
  {
    add_error(state, make_error(it, "string literal"));
  }

  return ParserResult(std::next(it), std::move(state));
}

// string-literal-list:
//    string-literal+

auto parser_string_literal_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StringLiteralList));

  auto [it, strs_state] = parser_take_repeat(
      parser, begin, end,
      parser_string_literal,
      [] (TokenIterator t) { return t->type == TokenType::StringConstant || t->type == TokenType::Identifier; });

  add_state(state, std::move(strs_state));


  if (is<ParserSuccess>(state))
  {
    auto& tree = get<ParserSuccess>(state).tree;

    // Turn string-literal-list with one child into a single string-literal.
    if (tree->child_count() == 1)
    {
      tree = std::make_unique<SyntaxTree>(NodeType::StringLiteral, *begin);
    }
  }

  return ParserResult(it, std::move(state));
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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  auto tree = std::make_unique<SyntaxTree>(NodeType::Constant);
  ParserState state = ParserSuccess(nullptr);

  switch (begin->type)
  {
    case TokenType::IntegerConstant:
    case TokenType::OctIntegerConstant:
    case TokenType::HexIntegerConstant:
      tree->add_child(std::make_unique<SyntaxTree>(NodeType::IntegerConstant, *begin));
      break;

    case TokenType::FloatConstant:
      tree->add_child(std::make_unique<SyntaxTree>(NodeType::FloatingConstant, *begin));
      break;

    case TokenType::CharConstant:
      tree->add_child(std::make_unique<SyntaxTree>(NodeType::CharacterConstant, *begin));
      break;

    case TokenType::Identifier:
      // TODO: Check in parser.symbols.enum_table if *begin is a valid enumeration constant.
      tree->add_child(std::make_unique<SyntaxTree>(NodeType::EnumerationConstant, *begin));
      break;

    default:
      add_error(state, ParserError(begin, "constant"));
  }

  if (is<ParserSuccess>(state))
  {
    state = ParserSuccess(std::move(tree));
  }

  return ParserResult(std::next(begin), std::move(state));
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
//    ++ -- & * + - ~ !

// TODO
auto parser_unary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  auto parser_incr_decr_expr = [] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    auto incr_decr_operator =
      parser_make_operator(NodeType::UnaryOperator, [] (TokenIterator t) {
        return t->type == TokenType::Increment
            || t->type == TokenType::Decrement;
        });

    auto rule = make_unary_expr_prefix(incr_decr_operator, parser_unary_expression);

    return rule(parser, begin, end);
  };

  auto result = parser_one_of(parser, begin, end,
                              parser_incr_decr_expr,
                              parser_identifier);

  expect_one_of(result.state, NodeType::UnaryExpression);

  return result;
}

// cast-expression:
//    unary-expression
//    '(' type-name ')' cast-expression

auto parser_cast_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  return parser_primary_expression(parser, begin, end);
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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

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

// conditional-expression:
//    logical-or-expression ('?' expression ':' conditional-expression)?

auto parser_conditional_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  ParserState state = ParserSuccess(nullptr);
  auto [it, or_state] = parser_logical_or_expression(parser, begin, end);

  add_state(state, std::move(or_state));

  if (it != end && it->type == TokenType::QuestionMark)
  {
    // Parse a ternary operator.
    ParserState if_state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ConditionalExpression));
    std::swap(if_state, state);
    add_state(state, std::move(if_state));

    auto then_branch = parser_expression(parser, std::next(it), end);
    add_state(state, std::move(then_branch.state));
    it = then_branch.next_token;

    if (expect_token(state, it, end, TokenType::Colon))
    {
      auto else_branch = parser_conditional_expression(parser, std::next(it), end);
      add_state(state, std::move(else_branch.state));
      it = else_branch.next_token;
    }
  }

  return ParserResult(it, std::move(state));
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
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  auto parser_assign_operator =
    parser_make_operator(NodeType::AssignmentExpression, [] (TokenIterator t) {
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

  auto parser_assignment_expr =
    make_binary_expr(
      parser_unary_expression,
      parser_assign_operator,
      parser_assignment_expression);

  auto result = parser_one_of(parser, begin, end,
                              parser_assignment_expr,
                              parser_conditional_expression);

  expect_one_of(result.state, NodeType::AssignmentExpression);
  return result;
}

// expression:
//    assignment-expression
//    expression ',' assignment-expression

auto parser_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  auto parser_expr_list =
    parser_make_sequence_list(NodeType::Expression, parser_assignment_expression, TokenType::Comma);

  return parser_expr_list(parser, begin, end);
}

// primary-expression:
//    identifier
//    constant
//    string-literal+
//    '(' expression ')'
//
// TODO: generic-selection

auto parser_primary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  if (begin == end)
    return ParserResult(end, make_error(ParserStatus::PlainWrong, end));

  auto parser_parens_expression =
    parser_make_inside_brackets(TokenType::LeftParen,
                                parser_expression,
                                TokenType::RightParen);

  auto result = parser_one_of(parser, begin, end,
                              parser_identifier,
                              parser_constant,
                              parser_string_literal_list,
                              parser_parens_expression);

  expect_one_of(result.state, NodeType::PrimaryExpression);
  return result;
}

// End of rules

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
    case NodeType::UnaryOperator:
      return "unary operator";
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
