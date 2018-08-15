#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/semantics/sema.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/lexer.hpp"
#include "cci/syntax/source_map.hpp"
#include <optional>
#include <string_view>

namespace cci {

struct Parser
{
private:
  Lexer &lex;
  Sema &sema;
  Token tok;
  diag::Handler &diag;

public:
  Parser(Lexer &lex, Sema &sema)
    : lex(lex), sema(sema), diag(lex.diagnostics())
  {
    // Sets up the peek token.
    tok = lex.next_token();
  }

  // Consumes the current token and peeks the next one. Returns a SourceLocation
  // to the consumed token.
  auto consume_token() -> srcmap::ByteLoc
  {
    auto old_loc = tok.location();
    tok = lex.next_token();
    return old_loc;
  }

private:
  auto parse_expression() -> ASTResult<Expr>;
  auto parse_string_literal_expression() -> ASTResult<StringLiteral>;
};

} // namespace cci
