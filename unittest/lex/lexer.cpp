#include "cci/lex/lexer.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

namespace {

TEST(LexerTest, specialCharacters)
{
  const char *code = R"(
// line continues??/
here\
and here!
this_??/
also_\
works
"invalid\??/
"
)";
  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {"this_\?\?/\nalso_\\\nworks", cci::TokenKind::identifier},
    {R"("invalid\??/)", cci::TokenKind::unknown},
    {"\"", cci::TokenKind::unknown},
  };
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  for (const auto [spell, kind] : corrects)
  {
    EXPECT_EQ(kind, tstream.peek().kind);
    EXPECT_EQ(spell, tstream.consume().spelling(source));
  }

  EXPECT_TRUE(tstream.empty());
}

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

TEST(LexerTest, numericConstants)
{
  const char *code = R"(
42ULL 3.14f 161.80e-3 1.9E377P+1 .999
)";
  const std::string_view corrects[] {
    "42ULL",
    "3.14f",
    "161.80e-3",
    "1.9E377P+1",
    ".999",
  };
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  for (const auto correct : corrects)
  {
    EXPECT_TRUE(tstream.peek().is(cci::TokenKind::numeric_constant));
    EXPECT_EQ(correct, tstream.consume().spelling(source));
  }

  EXPECT_TRUE(tstream.empty());
  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
}

TEST(LexerTest, comments)
{
  const char *code = R"(
dont_skip_1 // this should be skipped, \
WE GET SIGNAL!
// skip this \too
/\
/ and this too
dont_skip_2
"a//b"        // string literal
// */         // comment, not syntax error
f = g/**//h   // f = g / h
//\
x             // first two-line comment
/\
/ y           // second two-line comment
/*//*/ z      // z
m = n//**/o
  + p         // m = n + p
)";
  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {"dont_skip_1", cci::TokenKind::identifier},
    {"dont_skip_2", cci::TokenKind::identifier},
    {"\"a//b\"", cci::TokenKind::string_literal},
    {"f", cci::TokenKind::identifier},
    {"=", cci::TokenKind::equal},
    {"g", cci::TokenKind::identifier},
    {"/", cci::TokenKind::slash},
    {"h", cci::TokenKind::identifier}, // f = g / h
    {"z", cci::TokenKind::identifier}, // z
    {"m", cci::TokenKind::identifier},
    {"=", cci::TokenKind::equal},
    {"n", cci::TokenKind::identifier},
    {"+", cci::TokenKind::plus},
    {"p", cci::TokenKind::identifier}, // m = n + p
  };
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  for (const auto [spell, kind] : corrects)
  {
    EXPECT_EQ(kind, tstream.peek().kind);
    EXPECT_EQ(spell, tstream.consume().spelling(source));
  }

  EXPECT_TRUE(tstream.empty());
  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
}

TEST(LexerTest, charConstants)
{
  const char *code = R"(
'xxx' L'\'x\'' u'new\
line' U'\u1234x\777\xffffffff'
'\\'
''
'\\
)";
  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {"'xxx'", cci::TokenKind::char_constant},
    {"L'\\'x\\''", cci::TokenKind::wide_char_constant},
    {"u'new\\\nline'", cci::TokenKind::utf16_char_constant},
    {"U'\\u1234x\\777\\xffffffff'", cci::TokenKind::utf32_char_constant},
    {"'\\\\'", cci::TokenKind::char_constant},
    {"''", cci::TokenKind::unknown},
    {"'\\\\", cci::TokenKind::unknown},
  };
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  for (const auto [spell, kind] : corrects)
  {
    EXPECT_EQ(kind, tstream.peek().kind);
    EXPECT_EQ(spell, tstream.consume().spelling(source));
  }

  EXPECT_TRUE(tstream.empty());
}

TEST(LexerTest, stringLiterals)
{
  const char *code = R"(
"xxx" L"\"abc\"" u"new\
line" U"\u1234x\777\xffffffff"
u8""
"\\
"\\"
)";
  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {R"("xxx")", cci::TokenKind::string_literal},
    {R"(L"\"abc\"")", cci::TokenKind::wide_string_literal},
    {"u\"new\\\nline\"", cci::TokenKind::utf16_string_literal},
    {R"(U"\u1234x\777\xffffffff")", cci::TokenKind::utf32_string_literal},
    {R"(u8"")", cci::TokenKind::utf8_string_literal},
    {R"("\\)", cci::TokenKind::unknown},
    {R"("\\")", cci::TokenKind::string_literal},
  };
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto tstream = cci::TokenStream::tokenize(source);

  for (const auto [spell, kind] : corrects)
  {
    EXPECT_EQ(kind, tstream.peek().kind);
    EXPECT_EQ(spell, tstream.consume().spelling(source));
  }

  EXPECT_TRUE(tstream.empty());
}

} // namespace
