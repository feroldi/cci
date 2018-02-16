#include "cci/lex/literal_parser.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"
#include "cci/util/contracts.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

namespace {

TEST(LiteralParser, numericConstants)
{
  const char *code = R"(
42uL // ok, decimal
042 // ok, octal
0xDEADc0dellu // ok, hexadecimal
0uU // error: invalid suffix 'uU'
0LLL // error: invalid suffix 'LLL'
0128 // error: invalid digit '8'
314e10 // ok, double with exp
1.f // ok, float with period and float suffix
1.ef // error: empty exponent
.0 // ok, double
01238. // ok, double
0xabcde.ffP+1 // ok, hexadecimal floating constant
0xep1f // ok, hexadecimal floating constant
0x.f // error: missing exponent
18446744073709551616ull // error: overflow
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto lexer = cci::Lexer(source);
  std::optional<cci::Token> tok;

  // 42ul
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_TRUE(result.is_integer_literal());
    EXPECT_EQ(10, result.radix);
    EXPECT_TRUE(result.is_unsigned);
    EXPECT_TRUE(result.is_long);
    EXPECT_FALSE(result.is_long_long);
    
    const auto [value, overflowed] = result.eval_to_integer();

    ASSERT_FALSE(overflowed);
    EXPECT_EQ(42ull, value);
  }

  // 042
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_TRUE(result.is_integer_literal());
    EXPECT_EQ(8, result.radix);
    EXPECT_FALSE(result.is_unsigned);
    EXPECT_FALSE(result.is_long);
    EXPECT_FALSE(result.is_long_long);
    
    const auto [value, overflowed] = result.eval_to_integer();

    ASSERT_FALSE(overflowed);
    EXPECT_EQ(34ull, value);
  }

  // 0xDEADc0dellu
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(16, result.radix);
    EXPECT_TRUE(result.is_integer_literal());
    EXPECT_TRUE(result.is_unsigned);
    EXPECT_FALSE(result.is_long);
    EXPECT_TRUE(result.is_long_long);
    
    const auto [value, overflowed] = result.eval_to_integer();

    ASSERT_FALSE(overflowed);
    EXPECT_EQ(3735929054ull, value);
  }

  // 0uU // error
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_TRUE(result.has_error);
  }

  // 0LLL // error
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_TRUE(result.has_error);
  }

  // 0128 // error
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_TRUE(result.has_error);
  }

  // 314e10
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(10, result.radix);
    EXPECT_TRUE(result.is_floating_literal());
    EXPECT_FALSE(result.is_long);
    EXPECT_FALSE(result.is_float);
  }

  // 1.f // ok, floating with period and float suffix
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(10, result.radix);
    EXPECT_TRUE(result.is_floating_literal());
    EXPECT_TRUE(result.has_period);
    EXPECT_TRUE(result.is_float);
    EXPECT_FALSE(result.has_exponent);
  }

  // 1.ef // error: empty exponent
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_TRUE(result.has_error);
  }

  // .0 // ok, double
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(10, result.radix);
    EXPECT_TRUE(result.is_floating_literal());
    EXPECT_TRUE(result.has_period);
  }

  // 01238.0 // ok, double
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(10, result.radix);
    EXPECT_TRUE(result.is_floating_literal());
    EXPECT_TRUE(result.has_period);
  }

  // 0xabcde.ffP+1 // ok, hexadecimal floating constant
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(16, result.radix);
    EXPECT_TRUE(result.is_floating_literal());
    EXPECT_TRUE(result.has_period);
    EXPECT_TRUE(result.has_exponent);
  }

  // 0xep1f // ok, hexadecimal floating constant
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    EXPECT_EQ(16, result.radix);
    EXPECT_TRUE(result.is_floating_literal());
    EXPECT_FALSE(result.has_period);
    EXPECT_TRUE(result.is_float);
    EXPECT_TRUE(result.has_exponent);
  }

  // 0x.f // error: missing exponent
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_TRUE(result.has_error);
  }

  // 18446744073709551616 // error: overflow
  {
    tok = lexer.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(cci::TokenKind::numeric_constant, tok->kind);

    cci::NumericConstantParser result(lexer, tok->spelling(source), tok->location());
    EXPECT_FALSE(result.has_error);
    const auto [value, overflowed] = result.eval_to_integer();
    ASSERT_TRUE(overflowed);
  }
}

} // namespace
