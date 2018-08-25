#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/semantics/sema.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include <optional>

namespace cci {

struct Parser
{
private:
  Scanner scanner;
  Sema &sema;
  diag::Handler &diag;

public:
  Parser(Scanner scanner, Sema &sema)
    : scanner(std::move(scanner)), sema(sema), diag(scanner.diagnostics())
  {}

private:
  auto peek(size_t lookahead = 0) -> Token;
  auto consume() -> Token;

  auto expect_and_consume(Category category) -> std::optional<Token>
  {
    if (peek().category() == category)
      return consume();

    diag.report(peek().location(), "expected '{}'").arg(category);
    return std::nullopt;
  }

  auto parse_expression() -> std::optional<arena_ptr<Expr>>;
  auto parse_string_literal_expression()
    -> std::optional<arena_ptr<StringLiteral>>;

private:
  small_vector<Token, 8> peeked_toks;
};

} // namespace cci
