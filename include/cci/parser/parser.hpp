#pragma once
#include "cci/lex/lexer.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/type.hpp"
#include "cci/ast/expr.hpp"
#include "cci/semantics/sema.hpp"

namespace cci {

struct Parser
{
  Lexer &lex;
  Sema &sema;
  Token tok;

  Parser(Lexer &lex, Sema &sema) : lex(lex), sema(sema)
  {
    // Sets up the peek token.
    consume_token();
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

  auto parse_primary_expression() -> std::unique_ptr<Expr>;
  auto parse_string_literal_expression() -> std::unique_ptr<StringLiteral>;
};

} // namespace cci
