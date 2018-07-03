#include "cci/parser/parser.hpp"
#include "cci/lex/lexer.hpp"
#include "cci/util/contracts.hpp"

using namespace cci;

auto Parser::parse_expression() -> std::unique_ptr<Expr>
{
  std::unique_ptr<Expr> res;

  switch (tok.kind)
  {
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
    default:
      cci_unreachable();
  }

  return res;
}

auto Parser::parse_string_literal_expression() -> std::unique_ptr<StringLiteral>
{
  cci_expects(is_string_literal(tok.kind));

  small_vector<Token, 1> string_toks;

  string_toks.push_back(tok);
  consume_token();

  while (is_string_literal(tok.kind))
  {
    string_toks.push_back(tok);
    consume_token();
  }

  return sema.act_on_string_literal(string_toks);
}
