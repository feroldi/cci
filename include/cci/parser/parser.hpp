#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/lex/lexer.hpp"
#include "cci/semantics/sema.hpp"
#include <optional>
#include <string_view>

namespace cci {

struct Parser
{
private:
  Lexer &lex;
  Sema &sema;
  Token tok;
  CompilerDiagnostics &diags;

public:
  Parser(Lexer &lex, Sema &sema)
    : lex(lex), sema(sema), tok(), diags(lex.diagnostics())
  {
    // Sets up the peek token.
    auto new_tok = lex.next_token();
    cci_expects(new_tok.has_value());
    tok = *new_tok;
  }

  // Consumes the current token and peeks the next one. Returns a SourceLocation
  // to the consumed token.
  auto consume_token() -> SourceLocation
  {
    SourceLocation old_loc = tok.location();
    if (auto new_tok = lex.next_token())
      tok = *new_tok;
    return old_loc;
  }

private:
  auto parse_expression() -> ASTResult<Expr>;
  auto parse_string_literal_expression() -> ASTResult<StringLiteral>;
};

} // namespace cci
