#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include <optional>
#include <span>

namespace cci::syntax
{

// Semantic analyses.
struct Sema
{
private:
    Scanner &scanner;
    diag::Handler &diag_handler;
    ast::ASTContext &context;

public:
    Sema(Scanner &scanner, ast::ASTContext &ctx)
        : scanner(scanner), diag_handler(scanner.diag_handler), context(ctx)
    {}

    auto act_on_numeric_constant(const Token &)
        -> std::optional<arena_ptr<ast::Expr>>;

    auto act_on_char_constant(const Token &)
        -> std::optional<arena_ptr<ast::CharacterConstant>>;

    auto act_on_string_literal(std::span<const Token> string_toks)
        -> std::optional<arena_ptr<ast::StringLiteral>>;

    auto act_on_paren_expr(arena_ptr<ast::Expr> expr, ByteLoc left,
                           ByteLoc right)
        -> std::optional<arena_ptr<ast::ParenExpr>>;

    auto act_on_array_subscript(arena_ptr<ast::Expr> base,
                                arena_ptr<ast::Expr> idx, ByteLoc left,
                                ByteLoc right)
        -> std::optional<arena_ptr<ast::ArraySubscriptExpr>>;

    auto function_array_lvalue_conversion(arena_ptr<ast::Expr> expr)
        -> std::optional<arena_ptr<ast::Expr>>;

    auto function_array_conversion(arena_ptr<ast::Expr> expr)
        -> std::optional<arena_ptr<ast::Expr>>;

    auto lvalue_conversion(arena_ptr<ast::Expr> expr)
        -> std::optional<arena_ptr<ast::Expr>>;
};

} // namespace cci::syntax
