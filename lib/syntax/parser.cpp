#include "cci/syntax/parser.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/semantics/sema.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/small_vector.hpp"
#include <string_view>

using namespace cci;

auto Parser::parse_expression() -> std::optional<arena_ptr<Expr>>
{
  std::optional<arena_ptr<Expr>> res;

  switch (tok.category())
  {
    default: cci_unreachable();
    case Category::numeric_constant:
      res = sema.act_on_numeric_constant(tok);
      consume_token();
      break;

    case Category::char_constant:
    case Category::utf8_char_constant:
    case Category::utf16_char_constant:
    case Category::utf32_char_constant:
    case Category::wide_char_constant:
      res = sema.act_on_char_constant(tok);
      consume_token();
      break;

    case Category::string_literal:
    case Category::utf8_string_literal:
    case Category::utf16_string_literal:
    case Category::utf32_string_literal:
    case Category::wide_string_literal:
      res = parse_string_literal_expression();
      break;
    case Category::l_paren:
    {
      auto lparen_loc = consume_token();
      res = parse_expression();
      if (tok.is_not(Category::r_paren))
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

auto Parser::parse_string_literal_expression()
  -> std::optional<arena_ptr<StringLiteral>>
{
  cci_expects(is_string_literal(tok.category()));

  small_vector<Token, 4> string_toks;

  string_toks.push_back(tok);
  consume_token();

  while (is_string_literal(tok.category()))
  {
    string_toks.push_back(tok);
    consume_token();
  }

  return sema.act_on_string_literal(string_toks);
}
