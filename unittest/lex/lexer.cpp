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
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto ctx = cci::LexerContext(source);

  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {"this_\?\?/\nalso_\\\nworks", cci::TokenKind::identifier},
    {R"("invalid\??/)", cci::TokenKind::unknown},
    {"\"", cci::TokenKind::unknown},
  };

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_FALSE(ctx.next_token().has_value());
}

TEST(LexerTest, identifiers)
{
  const char *code = R"(
int
un\
signed
_abc123 escaped\
newline
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto ctx = cci::LexerContext(source);

  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {"int", cci::TokenKind::kw_int},
    {"un\\\nsigned", cci::TokenKind::kw_unsigned},
    {"_abc123", cci::TokenKind::identifier},
    {"escaped\\\nnewline", cci::TokenKind::identifier},
  };

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
  EXPECT_FALSE(ctx.next_token().has_value());
}

TEST(LexerTest, universalCharacterNames)
{
  const char *code = R"(
\u1234 \UAABBCCDD \UABCD
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto ctx = cci::LexerContext(source);

  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {R"(\u1234)", cci::TokenKind::identifier},
    {R"(\UAABBCCDD)", cci::TokenKind::identifier},
    {"\\", cci::TokenKind::unknown},
    {"UABCD", cci::TokenKind::identifier},
  };

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_TRUE(diag.has_errors() || diag.has_warnings());
  EXPECT_FALSE(ctx.next_token().has_value());
}

TEST(LexerTest, numericConstants)
{
  const char *code = R"(
42ULL 3.14f 161.80e-3 1.9E377P+1 .999
)";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto ctx = cci::LexerContext(source);

  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {"42ULL", cci::TokenKind::numeric_constant},
    {"3.14f", cci::TokenKind::numeric_constant},
    {"161.80e-3", cci::TokenKind::numeric_constant},
    {"1.9E377P+1", cci::TokenKind::numeric_constant},
    {".999", cci::TokenKind::numeric_constant},
  };

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
  EXPECT_FALSE(ctx.next_token().has_value());
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
  auto ctx = cci::LexerContext(source);

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_FALSE(diag.has_errors() || diag.has_warnings());
  EXPECT_FALSE(ctx.next_token().has_value());
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
  auto ctx = cci::LexerContext(source);

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_TRUE(diag.has_errors() || diag.has_warnings());
  EXPECT_FALSE(ctx.next_token().has_value());
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
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto source = cci::SourceManager::from_buffer(diag, code);
  auto ctx = cci::LexerContext(source);

  const std::pair<std::string_view, cci::TokenKind> corrects[]{
    {R"("xxx")", cci::TokenKind::string_literal},
    {R"(L"\"abc\"")", cci::TokenKind::wide_string_literal},
    {"u\"new\\\nline\"", cci::TokenKind::utf16_string_literal},
    {R"(U"\u1234x\777\xffffffff")", cci::TokenKind::utf32_string_literal},
    {R"(u8"")", cci::TokenKind::utf8_string_literal},
    {R"("\\)", cci::TokenKind::unknown},
    {R"("\\")", cci::TokenKind::string_literal},
  };

  for (const auto [spell, kind] : corrects)
  {
    auto tok = ctx.next_token();
    ASSERT_TRUE(tok.has_value());
    EXPECT_EQ(kind, tok->kind);
    EXPECT_EQ(spell, tok->raw_spelling(source));
  }

  EXPECT_TRUE(diag.has_errors() || diag.has_warnings());
  EXPECT_FALSE(ctx.next_token().has_value());
}

} // namespace
