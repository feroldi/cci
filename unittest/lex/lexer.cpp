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
_abc123 escaped\
newline
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::kw_int));
  EXPECT_EQ("int", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("_abc123", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("escaped\\\nnewline", source.text_slice(tstream.consume().source_range()));

  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
  EXPECT_TRUE(tstream.empty());
}

TEST(LexerTest, universalCharacterNames)
{
  const char *code = R"(
\u1234 \UAABBCCDD \UABCD
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(\u1234)", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ(R"(\UAABBCCDD)", source.text_slice(tstream.consume().source_range()));

  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::unknown));
  EXPECT_EQ("\\", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.peek().is(cci::TokenKind::identifier));
  EXPECT_EQ("UABCD", source.text_slice(tstream.consume().source_range()));

  EXPECT_TRUE(tstream.empty());

  EXPECT_TRUE(diag.has_errors() || diag.has_warnings());
}
} // namespace
