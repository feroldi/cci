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
\u0030 \U00000030
ident\uFFFFwith_UCN\UFFFFFFFF
)";
  auto source = cci::SourceManager::from_buffer(code);
  cci::CompilerDiagnostics diag(cci::DiagnosticsOptions{}, source, stderr);
  auto text = source.full_text();
  auto tstream = cci::TokenStream::tokenize(diag, text.begin(), text.end());

  ASSERT_EQ(8, tstream.size());

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::kw_int));
  EXPECT_EQ("int", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("_i", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("abc123", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
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
  auto source = cci::SourceManager::from_buffer(code);
  cci::CompilerDiagnostics diag(cci::DiagnosticsOptions{}, source, stderr);
  auto text = source.full_text();
  auto tstream = cci::TokenStream::tokenize(diag, text.begin(), text.end());

  ASSERT_TRUE(diag.has_errors());
  ASSERT_EQ(4, tstream.size());

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("wrong_", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("uabc_UCN_", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(UABCD_\U12345678a9)", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.consume().is(cci::TokenKind::eof));
}

} // namespace
