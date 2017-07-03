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

// Returns a parser rule that matches a binary expression.
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

// Returns a parser rule that matches an operator, or token.
template <typename OpMatch>
auto make_parser_operator(NodeType op_type, OpMatch op_match)
{
  return [=] (ParserContext& /*parser*/, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    if (begin == end)
      return ParserResult(end, make_error(std::prev(end), to_string(op_type)));

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

// C grammar

auto parser_expression(ParserContext&, TokenIterator begin, TokenIterator end) -> ParserResult;

// identifier
//    [a-zA-Z_$] ([a-zA-Z_$] | [0-9])*

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

// string-literal
//    encoding-prefix? '"' schar-sequence? '"'
//
// encoding-prefix
//    'u8' | 'u' | 'U' | 'L'
//
// schar-sequence
//    schar+
//
// schar
//    ~["\\\r\n]
//    escape-sequence
//    '\\\n'
//    '\\\r\n'

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

// string-literal-list

auto parser_string_literal_list(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::StringLiteralList));

  auto [it, strs_state] = parser_take_repeat(
      parser, begin, end,
      parser_string_literal,
      [] (TokenIterator t) { return t->type == TokenType::StringConstant; });

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

// TODO
auto parser_unary_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  auto parser_unary_operator =
    make_parser_operator(NodeType::UnaryOperator, [] (TokenIterator t) {
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

  return ParserResult(begin, make_error(begin, "unary expression"));
}

// relationalExpression
//  :   shiftExpression
//  |   relationalExpression relationalOperator shiftExpression
//  ;
//
// relationalOperator
//  : '<' | '>' | '<=' | '>='
//  ;

auto parser_relational_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  // TODO
  return ParserResult(std::next(begin), make_error(begin, "relational expression"));
}

// equalityExpression
//  :   relationalExpression
//  |   equalityExpression equalityOperator relationalExpression
//  ;
//
// equalityOperator
//  : '==' | '!='
//  ;

auto parser_equality_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  auto parser_equality_operator =
    make_parser_operator(NodeType::EqualityOperator, [] (TokenIterator t) {
        switch (t->type)
        {
          case TokenType::EqualsTo:
          case TokenType::NotEqualTo:
            return true;

          default:
            return false;
        }
      });

  auto parser_equality_expr =
    make_parser_binary_expr(
        parser_relational_expression,
        parser_equality_operator,
        parser_equality_expression);

  auto result = parser_one_of(parser, begin, end,
                              parser_equality_expr,
                              parser_relational_expression);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::EqualityExpression));
  add_state(state, std::move(result.state));

  return ParserResult(result.next_token, std::move(state));
}

// andExpression
//  :   equalityExpression
//  |   andExpression '&' equalityExpression
//  ;

auto parser_and_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::AndExpression));

  auto [it, bitand_state] =
    parser_take_repeat(
        parser, begin, end,
        parser_equality_expression,
        [] (TokenIterator& it) {
          if (it->type == TokenType::BitwiseAnd)
          {
            std::advance(it, 1);
            return true;
          }
          return false;
        });

  add_state(state, std::move(bitand_state));

  return ParserResult(it, std::move(state));
}

// exclusiveOrExpression
//  :   andExpression
//  |   exclusiveOrExpression '^' andExpression
//  ;

auto parser_exclusive_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ExclusiveOrExpression));

  auto [it, xor_state] =
    parser_take_repeat(
        parser, begin, end,
        parser_and_expression,
        [] (TokenIterator& it) {
          if (it->type == TokenType::BitwiseXor)
          {
            std::advance(it, 1);
            return true;
          }
          return false;
        });

  add_state(state, std::move(xor_state));

  return ParserResult(it, std::move(state));
}

// inclusiveOrExpression
//  :   exclusiveOrExpression
//  |   inclusiveOrExpression '|' exclusiveOrExpression
//  ;

auto parser_inclusive_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::InclusiveOrExpression));

  auto [it, bitor_state] =
    parser_take_repeat(
        parser, begin, end,
        parser_exclusive_or_expression,
        [] (TokenIterator& it) {
          if (it->type == TokenType::BitwiseOr)
          {
            std::advance(it, 1);
            return true;
          }
          return false;
        });

  add_state(state, std::move(bitor_state));

  return ParserResult(it, std::move(state));
}

// logicalAndExpression
//  :   inclusiveOrExpression
//  |   logicalAndExpression '&&' inclusiveOrExpression
//  ;

auto parser_logical_and_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::LogicalAndExpression));

  auto [it, and_state] =
    parser_take_repeat(
        parser, begin, end,
        parser_inclusive_or_expression,
        [] (TokenIterator& it) {
          if (it->type == TokenType::LogicalAnd)
          {
            std::advance(it, 1);
            return true;
          }
          return false;
        });

  add_state(state, std::move(and_state));

  return ParserResult(it, std::move(state));
}

// logicalOrExpression
//  : logicalAndExpression
//  | logicalOrExpression '||' logicalAndExpression
//  ;

auto parser_logical_or_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::LogicalOrExpression));

  auto [it, or_state] =
    parser_take_repeat(
        parser, begin, end,
        parser_logical_and_expression,
        [] (TokenIterator& it) {
          if (it->type == TokenType::LogicalOr)
          {
            std::advance(it, 1);
            return true;
          }
          return false;
        });

  add_state(state, std::move(or_state));

  return ParserResult(it, std::move(state));
}

// conditionalExpression
//  :   logicalOrExpression ('?' expression ':' conditionalExpression)?
//  ;

auto parser_conditional_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::ConditionalExpression));
  auto [it, or_state] = parser_logical_or_expression(parser, begin, end);

  add_state(state, std::move(or_state));

  if (it != end && it->type == TokenType::QuestionMark)
  {
    auto expr = parser_expression(parser, std::next(it), end);
    add_state(state, std::move(expr.state));
    it = expr.next_token;

    if (expect_token(state, it, end, TokenType::Colon))
    {
      auto cond_expr = parser_conditional_expression(parser, std::next(it), end);
      add_state(state, std::move(cond_expr.state));
      it = cond_expr.next_token;
    }
  }

  return ParserResult(it, std::move(state));
}

// assignmentExpression
//  :   conditionalExpression
//  |   unaryExpression assignmentOperator assignmentExpression
//  ;
//
// assignmentOperator
//  :   '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
//  ;

auto parser_assignment_expression(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  auto parser_assign_operator =
    make_parser_operator(NodeType::AssignmentOperator, [] (TokenIterator t) {
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

  ParserState state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::PrimaryExpression));

  // TODO
  auto parser_parens_expression = [] (ParserContext& parser, TokenIterator begin, TokenIterator end) -> ParserResult
  {
    return ParserResult(end, make_error(begin, "parens expression"));
  };

  auto [it, expr_state] = parser_one_of(parser, begin, end, 
                                        parser_identifier,
                                        parser_constant,
                                        parser_string_literal_list,
                                        parser_parens_expression);

  add_state(state, std::move(expr_state));

  return ParserResult(it, std::move(state));
}

// End of rules

} // namespace

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
    case NodeType::RelatioalExpression:
      return "relatioal expression";
    case NodeType::EqualityExpression:
      return "equality expression";
    case NodeType::EqualityOperator:
      return "equality operator";
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
    case NodeType::AssignmentOperator:
      return "assignment operator";
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

} // namespace ccompiler
