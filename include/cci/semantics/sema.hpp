#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/lexer.hpp"
#include "cci/syntax/source_map.hpp"

namespace cci {

// Semantic analyses.
struct Sema
{
private:
  Lexer &lex;
  diag::Handler &diag;
  ASTContext &context;

public:
  Sema(Lexer &lex, ASTContext &ctx)
    : lex(lex), diag(lex.diagnostics()), context(ctx)
  {}

  auto act_on_numeric_constant(const Token &) -> ASTResult<Expr>;
  auto act_on_char_constant(const Token &) -> ASTResult<CharacterConstant>;
  auto act_on_string_literal(span<const Token> string_toks)
    -> ASTResult<StringLiteral>;
  auto act_on_paren_expr(arena_ptr<Expr> expr, srcmap::ByteLoc left,
                         srcmap::ByteLoc right) -> ASTResult<ParenExpr>;
};

} // namespace cci
