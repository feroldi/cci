#include "cci/lex/literal_parser.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"
#include "cci/util/contracts.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

namespace {

TEST(LiteralParser, integerConstants)
{
  const char *code = R"(
42uL 042 0xDEADc0dellu
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto lexer = cci::Lexer(source);
  std::optional<cci::Token> tok;

  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    ASSERT_FALSE(result.has_error);
    EXPECT_EQ(10, result.radix);
    EXPECT_TRUE(result.is_unsigned);
    EXPECT_TRUE(result.is_long);
    EXPECT_FALSE(result.is_long_long);
    EXPECT_FALSE(result.is_float);
    
    const auto [value, overflowed] = result.eval_to_integer();

    ASSERT_FALSE(overflowed);
    EXPECT_EQ(42ull, value);
  }

  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    ASSERT_FALSE(result.has_error);
    EXPECT_EQ(8, result.radix);
    EXPECT_FALSE(result.is_unsigned);
    EXPECT_FALSE(result.is_long);
    EXPECT_FALSE(result.is_long_long);
    EXPECT_FALSE(result.is_float);
    
    const auto [value, overflowed] = result.eval_to_integer();

    ASSERT_FALSE(overflowed);
    EXPECT_EQ(34ull, value);
  }

  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    ASSERT_FALSE(result.has_error);
    EXPECT_EQ(16, result.radix);
    EXPECT_TRUE(result.is_unsigned);
    EXPECT_FALSE(result.is_long);
    EXPECT_TRUE(result.is_long_long);
    EXPECT_FALSE(result.is_float);
    
    const auto [value, overflowed] = result.eval_to_integer();

    ASSERT_FALSE(overflowed);
    EXPECT_EQ(3735929054ull, value);
  }
}

} // namespace
