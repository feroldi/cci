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

  ParserSuccess(ParserSuccess&&) noexcept = default;
  ParserSuccess& operator= (ParserSuccess&&) noexcept = default;
};

// Represents an error when some parser
// couldn't generate an AST.
struct ParserError
{
  TokenIterator where; //< Where it happened.
  std::string error;   //< Error explanation.

  explicit ParserError(TokenIterator where) noexcept
    : where(where)
  {}

  explicit ParserError(TokenIterator where, std::string error) noexcept
    : where(where), error(std::move(error))
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
    else
    {
      tree->add_child(std::move(node));
    }
  }
}

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
};

// Tries to parse one of the rules.
template <typename Rule, typename... Rules>
auto parser_one_of(ParserContext& parser, TokenIterator begin,
                   TokenIterator end, Rule rule, Rules&&... rules)
  -> ParserResult
{
  auto result = rule(parser, begin, end);

  if (is<ParserSuccess>(result.state))
  {
    return result;
  }

  if constexpr (sizeof...(rules) > 0)
  {
    return parser_one_of(parser, begin, end, std::forward<Rules>(rules)...);
  }
  else
  {
    return ParserResult(end, make_error(begin, "expected one of"));
  }
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
    add_error(state, make_error((it == end ? std::prev(it) : it), "expected token"));
    return false;
  }

  return true;
}

auto expect_match_token(ParserState& state, TokenIterator begin,
                        TokenIterator end, TokenIterator it, TokenType token_type)
  -> bool
{
  if (it == end || it->type != token_type)
  {
    add_error(state, make_error(begin, "expected match token for this"));
    add_error(state, make_error((it == end ? std::prev(it) : it), "expected here"));
    return false;
  }

  return true;
}

// Returns a parser rule that parses a binary expression.
//
// binaryExpression:
//  : lhs_rule op_rule rhs_rule
//  ;
//    -> ^($op_rule $lhs_rule $rhs_rule)
template <typename LhsRule, typename OpRule, typename RhsRule>
auto make_parser_binary_expr(LhsRule lhs_rule, OpRule op_rule, RhsRule rhs_rule)
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

// C grammar

// Identifier

auto parser_identifier(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

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

// StringLiteral

auto parser_string_literal(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(nullptr);

  if (begin->type == TokenType::StringConstant)
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::StringLiteral, *begin);
    state = ParserSuccess(std::move(tree));
  }
  else
  {
    add_error(state, make_error(begin, "string literal"));
  }

  return ParserResult(std::next(begin), std::move(state));
}

// StringLiteral+

auto parser_string_literal_many(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  auto result = parser_take_while(parser, begin, end, parser_string_literal,
                                  [] (TokenIterator it) { return it->type == TokenType::StringConstant; });

  // A StringLiteral can also be made out of consecutive strings.
  if (is<ParserSuccess>(result.state))
  {
    auto tree = std::make_unique<SyntaxTree>(NodeType::StringLiteral);
    tree->take_children(std::move(get<ParserSuccess>(result.state).tree));

    return ParserResult(result.next_token, ParserSuccess(std::move(tree)));
  }
  else
  {
    return result;
  }
}

// Constant
//  :   IntegerConstant
//  |   FloatingConstant
//  |   CharacterConstant
//  |   EnumerationConstant
//  ;

auto parser_constant(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

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

// unaryExpression
//  :   postfixExpression
//  |   '++' unaryExpression
//  |   '--' unaryExpression
//  |   unaryOperator castExpression
//  |   'sizeof' unaryExpression
//  |   'sizeof' '(' typeName ')'
//  |   '_Alignof' '(' typeName ')'
//  ;
// 
// unaryOperator
//  :   '&' | '*' | '+' | '-' | '~' | '!'
//  ;

auto parser_unary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return ParserResult(begin, ParserSuccess(nullptr));
}

// conditionalExpression
//  :   logicalOrExpression ('?' expression ':' conditionalExpression)?
//  ;

// TODO
auto parser_conditional_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  return ParserResult(begin, ParserSuccess(nullptr));
}

// assignmentExpression
//  :   conditionalExpression
//  |   unaryExpression assignmentOperator assignmentExpression
//  ;
//
// assignmentOperator
//  :   '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
//  ;

// TODO
auto parser_assignment_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  auto parser_assign_operator = [] (ParserContext& /*parser*/, TokenIterator begin, TokenIterator end)
    -> ParserResult
  {
    if (begin == end)
    {
      return ParserResult(end, make_error(std::prev(end), "assignment operator"));
    }

    ParserState state = ParserSuccess(nullptr);

    switch (begin->type)
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
        state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::AssignmentOperator, *begin));
        break;

      default:
        add_error(state, make_error(begin, "assignment operator"));
    }

    return ParserResult(std::next(begin), std::move(state));
  };

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::AssignmentExpression));

  auto parser_binary_expr = 
    make_parser_binary_expr(
      parser_unary_expression,
      parser_assign_operator,
      parser_assignment_expression);

  auto result = parser_one_of(parser, begin, end,
                              parser_conditional_expression,
                              parser_binary_expr);
  
  add_state(state, std::move(result.state));

  return ParserResult(result.next_token, std::move(state));
}

// expression
//  :   assignmentExpression
//  |   expression ',' assignmentExpression
//  ;

auto parser_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::Expression));

  auto [it, expr_state] = parser_take_repeat(
      parser, begin, end,
      parser_assignment_expression,
      [] (TokenIterator& it) {
        if (it->type == TokenType::Comma)
        {
          std::advance(it, 1);
          return true;
        }

        return false;
      });

  add_state(state, std::move(expr_state));

  return ParserResult(it, std::move(state));
}

// primaryExpression
//  :   Identifier
//  |   Constant
//  |   StringLiteral+
//  |   '(' expression ')' //< TODO
//  ;
//
// TODO: genericSelection

auto parser_primary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  // TODO
  auto parser_parens_expression = [] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    return ParserResult(begin, ParserSuccess(nullptr));
  };

  auto [it, state] = parser_one_of(parser, begin, end, 
                                   parser_identifier,
                                   parser_constant,
                                   parser_string_literal_many,
                                   parser_parens_expression);

  if (is<ParserSuccess>(state))
  {
    // FIXME: primaryExpression should have only one child.
    auto tree = std::make_unique<SyntaxTree>(NodeType::PrimaryExpression);
    tree->take_children(std::move(get<ParserSuccess>(state).tree));

    return ParserResult(it, ParserSuccess(std::move(tree)));
  }
  else
  {
    return ParserResult(it, std::move(state));
  }
}

// End of rules

} // namespace

// TODO
auto SyntaxTree::parse(ProgramContext& program, const TokenStream& tokens)
  -> std::unique_ptr<SyntaxTree>
{
  ParserContext context{program, tokens};

  auto result = parser_primary_expression(context, tokens.begin(), tokens.end());

  if (is<ParserSuccess>(result.state))
  {
    return get<ParserSuccess>(std::move(result.state)).tree;
  }
  else
  {
    for (const auto& fail : get<ParserFailure>(result.state))
    {
      program.error(nocontext(), "syntax error: {}", fail.error);
    }

    return nullptr;
  }
}

} // namespace ccompiler
