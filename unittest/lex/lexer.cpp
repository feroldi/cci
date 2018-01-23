#include "cci/lex/lexer.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

namespace {

TEST(LexerTest, identifiers)
{
  const char *code = R"(
int
_i abc123 this_is_a_long_name_but_we_are_all_okay_with_it
\u00FF \U000000ff
ident\uFFFFwith_UCN\UFFFFFFFF
)";
  cci::CompilerDiagnostics diag(cci::DiagnosticsOptions{}, stderr);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  ASSERT_FALSE(diag.has_errors());

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::kw_int));
  EXPECT_EQ("int", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::raw_identifier));
  EXPECT_EQ("_i", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::raw_identifier));
  EXPECT_EQ("abc123", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::raw_identifier));
  EXPECT_EQ("this_is_a_long_name_but_we_are_all_okay_with_it",
            source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(\u0030)", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(\U00000030)", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(ident\uFFFFwith_UCN\UFFFFFFFF)",
            source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.consume().is(cci::TokenKind::eof));

  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
  EXPECT_TRUE(tstream.empty());
}

TEST(LexerTest, universalCharacterNames)
{
  const char *code = R"(
wrong_\uabc_UCN_\UABCD_\U12345678a9
)";
  cci::CompilerDiagnostics diag(cci::DiagnosticsOptions{}, stderr);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  ASSERT_TRUE(diag.has_errors());

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("wrong_", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("uabc_UCN_", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(UABCD_\U12345678a9)", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.consume().is(cci::TokenKind::eof));
}

#if 0
TEST(LexerTest, integerConstant)
{
  const char *code = R"(
123 456ull 0Ul 1LLu
0123
0xAABBCCFF 0XDE4DC0DEllu
)";
  cci::CompilerDiagnostics diag(cci::DiagnosticsOptions{}, stderr);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  ASSERT_FALSE(diag.has_errors());

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("123", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("456ull", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("0Ul", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("1LLu", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("0123", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("0xAABBCCFF", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::integer_constant));
  EXPECT_EQ("0XDE4DC0DEllu", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.consume().is(cci::TokenKind::eof));
}
#endif

} // namespace
