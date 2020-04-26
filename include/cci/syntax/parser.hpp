#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/sema.hpp"
#include "cci/syntax/source_map.hpp"
#include <optional>

namespace cci {

struct Parser
{
private:
    Scanner &scanner;
    Sema &sema;
    diag::Handler &diag;

public:
    Parser(Scanner &scanner, Sema &sema)
        : scanner(scanner), sema(sema), diag(scanner.diag_handler)
    {}

    auto parse_expression() -> std::optional<arena_ptr<Expr>>;

private:
    auto peek_tok(size_t lookahead = 0) -> Token;
    auto consume_tok() -> Token;
    auto expect_and_consume_tok(TokenKind category) -> std::optional<Token>;

    auto parse_primary_expression() -> std::optional<arena_ptr<Expr>>;
    auto parse_string_literal_expression()
        -> std::optional<arena_ptr<StringLiteral>>;
    auto parse_postfix_expression(arena_ptr<Expr>)
        -> std::optional<arena_ptr<Expr>>;

private:
    small_vector<Token, 8> peeked_toks;
};

} // namespace cci
