#include "cci/syntax/parser.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/semantics/sema.hpp"
#include "cci/syntax/lexer.hpp"
#include "cci/util/contracts.hpp"
#include <string_view>

using namespace cci;

auto Parser::parse_expression() -> ASTResult<Expr>
{
  ASTResult<Expr> res;

  switch (tok.kind)
  {
    default: cci_unreachable();
    case TokenKind::numeric_constant:
      res = sema.act_on_numeric_constant(tok);
      consume_token();
      break;

    case TokenKind::char_constant:
    case TokenKind::utf8_char_constant:
    case TokenKind::utf16_char_constant:
    case TokenKind::utf32_char_constant:
    case TokenKind::wide_char_constant:
      res = sema.act_on_char_constant(tok);
      consume_token();
      break;

    case TokenKind::string_literal:
    case TokenKind::utf8_string_literal:
    case TokenKind::utf16_string_literal:
    case TokenKind::utf32_string_literal:
    case TokenKind::wide_string_literal:
      res = parse_string_literal_expression();
      break;
    case TokenKind::l_paren:
    {
      auto lparen_loc = consume_token();
      res = parse_expression();
      if (tok.is_not(TokenKind::r_paren))
      {
        diag.report(tok.location(), "expected ')'");
        diag.report(lparen_loc, "to match this '('");
        // FIXME: Should break here?
        break;
      }
      auto rparen_loc = consume_token();
      res = sema.act_on_paren_expr(res.value(), lparen_loc, rparen_loc);
      break;
    }
  }

  return res;
}

auto Parser::parse_string_literal_expression() -> ASTResult<StringLiteral>
{
  cci_expects(is_string_literal(tok.kind));

  small_vector<Token, 4> string_toks;

  string_toks.push_back(tok);
  consume_token();

  while (is_string_literal(tok.kind))
  {
    string_toks.push_back(tok);
    consume_token();
  }

  return sema.act_on_string_literal(string_toks);
}
