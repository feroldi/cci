#pragma once
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/ast/arena_types.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"

namespace cci {

// Semantic analyses.
struct Sema
{
private:
  Lexer &lex;
  CompilerDiagnostics &diags;
  ASTContext &context;

public:
  Sema(Lexer &lex, ASTContext &ctx)
    : lex(lex), diags(lex.diagnostics()), context(ctx)
  {}

  auto act_on_numeric_constant(const Token &) -> ASTResult<Expr>;
  auto act_on_char_constant(const Token &)
    -> ASTResult<CharacterConstant>;
  auto act_on_string_literal(span<const Token> string_toks)
    -> ASTResult<StringLiteral>;
  auto act_on_paren_expr(arena_ptr<Expr> expr, SourceLocation left,
                         SourceLocation right) -> ASTResult<ParenExpr>;
};

} // namespace cci
