#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/memory_resource.hpp"
#include <optional>

namespace cci::syntax {

// Semantic analyses.
struct Sema
{
private:
    Scanner &scanner;
    diag::Handler &diag_handler;
    ASTContext &context;

public:
    Sema(Scanner &scanner, ASTContext &ctx)
        : scanner(scanner), diag_handler(scanner.diag_handler), context(ctx)
    {}

    auto act_on_numeric_constant(const Token &)
        -> std::optional<arena_ptr<Expr>>;
    auto act_on_char_constant(const Token &)
        -> std::optional<arena_ptr<CharacterConstant>>;
    auto act_on_string_literal(span<const Token> string_toks)
        -> std::optional<arena_ptr<StringLiteral>>;
    auto act_on_paren_expr(arena_ptr<Expr> expr, ByteLoc left, ByteLoc right)
        -> std::optional<arena_ptr<ParenExpr>>;
    auto act_on_array_subscript(arena_ptr<Expr> base, arena_ptr<Expr> idx,
                                ByteLoc left, ByteLoc right)
        -> std::optional<arena_ptr<ArraySubscriptExpr>>;
    auto function_array_lvalue_conversion(arena_ptr<Expr> expr)
        -> std::optional<arena_ptr<Expr>>;
    auto function_array_conversion(arena_ptr<Expr> expr)
        -> std::optional<arena_ptr<Expr>>;
    auto lvalue_conversion(arena_ptr<Expr> expr)
        -> std::optional<arena_ptr<Expr>>;
};

} // namespace cci::syntax
