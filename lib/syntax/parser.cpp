#include "cci/syntax/parser.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/semantics/sema.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/small_vector.hpp"
#include <string_view>

using namespace cci;

auto Parser::peek(size_t lookahead) -> Token
{
  cci_expects(peeked_toks.size() <= lookahead);

  if (lookahead < peeked_toks.size())
    return peeked_toks[lookahead];

  Token peeking = scanner.next_token();
  peeked_toks.push_back(peeking);
  cci_ensures(lookahead + 1 == peeked_toks.size());
  return peeking;
}

auto Parser::consume() -> Token
{
  Token consumed = peek();
  std::move(std::next(peeked_toks.begin()), peeked_toks.end(),
            peeked_toks.begin());
  peeked_toks.pop_back();
  return consumed;
}

auto Parser::parse_expression() -> std::optional<arena_ptr<Expr>>
{
  std::optional<arena_ptr<Expr>> res;

  switch (peek().category())
  {
    default: cci_unreachable();
    case Category::numeric_constant:
      res = sema.act_on_numeric_constant(consume());
      break;

    case Category::char_constant:
    case Category::utf8_char_constant:
    case Category::utf16_char_constant:
    case Category::utf32_char_constant:
    case Category::wide_char_constant:
      res = sema.act_on_char_constant(consume());
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
      Token lparen_tok = consume();
      res = parse_expression();

      if (peek().is(Category::r_paren))
      {
        Token rparen_tok = consume();
        res = sema.act_on_paren_expr(res.value(), lparen_tok.location(),
                                     rparen_tok.location());
      }
      else
      {
        diag.report(peek().location(), "expected ')'");
        diag.report(lparen_tok.location(), "to match this '('");
      }

      break;
    }
  }

  return res;
}

auto Parser::parse_string_literal_expression()
  -> std::optional<arena_ptr<StringLiteral>>
{
  cci_expects(is_string_literal(peek().category()));

  small_vector<Token, 4> string_toks{consume()};
  while (is_string_literal(peek().category()))
    string_toks.push_back(consume());

  return sema.act_on_string_literal(string_toks);
}
