#include "cci/syntax/parser.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/sema.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/small_vector.hpp"
#include <algorithm>
#include <string_view>

using cci::ast::Expr;
using cci::ast::StringLiteral;

namespace cci::syntax
{

auto Parser::peek_tok(size_t lookahead) -> Token
{
    cci_expects(peeked_toks.size() >= lookahead);

    if (lookahead < peeked_toks.size())
        return peeked_toks[lookahead];

    Token peeking = scanner.next_token();
    peeked_toks.push_back(peeking);
    cci_ensures(lookahead + 1 == peeked_toks.size());
    return peeking;
}

auto Parser::consume_tok() -> Token
{
    Token consumed = peek_tok();
    std::move(std::next(peeked_toks.begin()), peeked_toks.end(),
              peeked_toks.begin());
    peeked_toks.pop_back();
    return consumed;
}

auto Parser::expect_and_consume_tok(TokenKind token_kind)
    -> std::optional<Token>
{
    if (peek_tok().is(token_kind))
        return consume_tok();

    diag.report(peek_tok().location(), diag::Diag::expected_but_got)
        .args(token_kind, peek_tok().kind);
    return std::nullopt;
}

auto Parser::parse_expression() -> std::optional<arena_ptr<Expr>>
{
    return parse_primary_expression();
}

auto Parser::parse_primary_expression() -> std::optional<arena_ptr<Expr>>
{
    std::optional<arena_ptr<Expr>> res;

    switch (peek_tok().kind)
    {
        default: cci_unreachable();
        case TokenKind::numeric_constant:
            res = sema.act_on_numeric_constant(consume_tok());
            break;

        case TokenKind::char_constant:
        case TokenKind::utf8_char_constant:
        case TokenKind::utf16_char_constant:
        case TokenKind::utf32_char_constant:
        case TokenKind::wide_char_constant:
            res = sema.act_on_char_constant(consume_tok());
            break;

        case TokenKind::string_literal:
        case TokenKind::utf8_string_literal:
        case TokenKind::utf16_string_literal:
        case TokenKind::utf32_string_literal:
        case TokenKind::wide_string_literal:
            res = parse_string_literal_expression();
            break;
        case TokenKind::l_paren: {
            Token lparen_tok = consume_tok();

            if ((res = parse_expression()))
            {
                if (auto rparen_tok =
                        expect_and_consume_tok(TokenKind::r_paren))
                    res = sema.act_on_paren_expr(res.value(),
                                                 lparen_tok.location(),
                                                 rparen_tok->location());
            }

            break;
        }
    }

    return res ? parse_postfix_expression(*res) : res;
}

auto Parser::parse_string_literal_expression()
    -> std::optional<arena_ptr<StringLiteral>>
{
    cci_expects(is_string_literal(peek_tok().kind));

    small_vector<Token, 4> string_toks{consume_tok()};
    while (is_string_literal(peek_tok().kind))
        string_toks.push_back(consume_tok());

    return sema.act_on_string_literal(string_toks);
}

auto Parser::parse_postfix_expression(arena_ptr<Expr> expr)
    -> std::optional<arena_ptr<Expr>>
{
    switch (peek_tok().kind)
    {
        case TokenKind::l_bracket: {
            const Token lbracket_tok = consume_tok();
            if (auto expr_inside_brackets = parse_expression())
            {
                if (auto rbracket_tok =
                        expect_and_consume_tok(TokenKind::r_bracket))
                {
                    return sema.act_on_array_subscript(
                        expr, expr_inside_brackets.value(),
                        lbracket_tok.location(), rbracket_tok->location());
                }
            }
            break;
        }
        default: return expr;
    }

    return std::nullopt;
}

} // namespace cci::syntax
