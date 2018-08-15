#include "cci/syntax/literal_parser.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/lexer.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/memory_resource.hpp"
#include "cci/util/span.hpp"
#include "cci/util/unicode.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

using namespace cci;

namespace {

struct LiteralParserTest : ::testing::Test
{
protected:
  srcmap::SourceMap source_map;
  diag::Handler diag_handler;
  std::unique_ptr<Lexer> lexer;
  TargetInfo target;
  pmr::monotonic_buffer_resource arena;

  LiteralParserTest()
    : source_map()
    , diag_handler(diag::ignoring_emitter(), source_map)
    , lexer()
    , target()
    , arena()
  {}

  auto lex(std::string source) -> std::vector<Token>
  {
    const auto &file =
      this->source_map.create_owned_filemap("main.c", std::move(source));
    lexer =
      std::make_unique<Lexer>(source_map, file.start_loc, file.src_begin(),
                              file.src_end(), diag_handler);
    std::vector<Token> toks;

    while (true)
    {
      auto tok = lexer->next_token();
      if (tok.is(TokenKind::eof))
        break;
      toks.push_back(tok);
    }

    return toks;
  }

  auto get_lexeme(const Token &tok) -> std::string_view
  {
    char *lexeme_buffer = new (
      this->arena.allocate(tok.size(), alignof(char))) char[tok.size() + 1];
    const size_t lexeme_len =
      Lexer::get_spelling_to_buffer(tok, lexeme_buffer, this->source_map);
    lexeme_buffer[lexeme_len] = '\0';
    return {lexeme_buffer, lexeme_len};
  }

  auto parse_numeric_constants(std::string source)
    -> std::vector<NumericConstantParser>
  {
    auto lexed_toks = lex(std::move(source));
    std::vector<NumericConstantParser> parsers;
    for (const Token &tok : lexed_toks)
      parsers.emplace_back(*this->lexer, get_lexeme(tok), tok.location());
    return parsers;
  }

  auto parse_char_constants(std::string source)
    -> std::vector<CharConstantParser>
  {
    auto lexed_toks = lex(std::move(source));
    std::vector<CharConstantParser> parsers;
    for (const Token &tok : lexed_toks)
      parsers.emplace_back(*this->lexer, get_lexeme(tok), tok.location(),
                           tok.kind, target);
    return parsers;
  }

  auto parse_string_literal(std::string source) -> StringLiteralParser
  {
    auto string_toks = lex(std::move(source));
    StringLiteralParser parser(*this->lexer, string_toks, target);
    return parser;
  }
};

TEST_F(LiteralParserTest, numericConstants)
{
  auto nums = parse_numeric_constants(
    "42uL "
    "042 "
    "0xDEADc0dellu "
    "0uU "
    "0LLL "
    "0128 "
    "314e10 "
    "1.f "
    "1.ef "
    ".0 "
    "01238. "
    "0xabcde.ffP+1 "
    "0xep1f "
    "0x.f \n");

  ASSERT_EQ(14, nums.size());

  // 42uL
  EXPECT_FALSE(nums[0].has_error);
  EXPECT_FALSE(nums[0].has_period);
  EXPECT_FALSE(nums[0].has_exponent);
  EXPECT_TRUE(nums[0].is_unsigned);
  EXPECT_TRUE(nums[0].is_long);
  EXPECT_FALSE(nums[0].is_long_long);
  EXPECT_FALSE(nums[0].is_float);
  EXPECT_TRUE(nums[0].is_integer_literal());
  EXPECT_FALSE(nums[0].is_floating_literal());
  EXPECT_EQ(10, nums[0].radix);
  EXPECT_EQ(std::pair(42ul, false), nums[0].to_integer());

  // 042
  EXPECT_FALSE(nums[1].has_error);
  EXPECT_FALSE(nums[1].has_period);
  EXPECT_FALSE(nums[1].has_exponent);
  EXPECT_FALSE(nums[1].is_unsigned);
  EXPECT_FALSE(nums[1].is_long);
  EXPECT_FALSE(nums[1].is_long_long);
  EXPECT_FALSE(nums[1].is_float);
  EXPECT_TRUE(nums[1].is_integer_literal());
  EXPECT_FALSE(nums[1].is_floating_literal());
  EXPECT_EQ(8, nums[1].radix);
  EXPECT_EQ(std::pair(34ul, false), nums[1].to_integer());

  // 0xDEADc0dellu
  EXPECT_FALSE(nums[2].has_error);
  EXPECT_FALSE(nums[2].has_period);
  EXPECT_FALSE(nums[2].has_exponent);
  EXPECT_TRUE(nums[2].is_unsigned);
  EXPECT_FALSE(nums[2].is_long);
  EXPECT_TRUE(nums[2].is_long_long);
  EXPECT_FALSE(nums[2].is_float);
  EXPECT_TRUE(nums[2].is_integer_literal());
  EXPECT_FALSE(nums[2].is_floating_literal());
  EXPECT_EQ(16, nums[2].radix);
  EXPECT_EQ(std::pair(3735929054ul, false), nums[2].to_integer());

  // 0uU
  EXPECT_TRUE(nums[3].has_error);

  // 0LLL
  EXPECT_TRUE(nums[4].has_error);

  // 0128
  EXPECT_TRUE(nums[5].has_error);

  // 314e10
  EXPECT_FALSE(nums[6].has_error);
  EXPECT_FALSE(nums[6].has_period);
  EXPECT_TRUE(nums[6].has_exponent);
  EXPECT_FALSE(nums[6].is_unsigned);
  EXPECT_FALSE(nums[6].is_long);
  EXPECT_FALSE(nums[6].is_long_long);
  EXPECT_FALSE(nums[6].is_float);
  EXPECT_FALSE(nums[6].is_integer_literal());
  EXPECT_TRUE(nums[6].is_floating_literal());
  EXPECT_EQ(10, nums[6].radix);
  // EXPECT_EQ(std::pair(314e10, false), nums[6].to_floating_point());

  // 1.f
  EXPECT_FALSE(nums[7].has_error);
  EXPECT_TRUE(nums[7].has_period);
  EXPECT_FALSE(nums[7].has_exponent);
  EXPECT_FALSE(nums[7].is_unsigned);
  EXPECT_FALSE(nums[7].is_long);
  EXPECT_FALSE(nums[7].is_long_long);
  EXPECT_TRUE(nums[7].is_float);
  EXPECT_FALSE(nums[7].is_integer_literal());
  EXPECT_TRUE(nums[7].is_floating_literal());
  EXPECT_EQ(10, nums[7].radix);
  // EXPECT_EQ(std::pair(1.0, false), nums[7].to_floating_point());

  // 1.ef
  EXPECT_TRUE(nums[8].has_error);

  // .0
  EXPECT_FALSE(nums[9].has_error);
  EXPECT_TRUE(nums[9].has_period);
  EXPECT_FALSE(nums[9].has_exponent);
  EXPECT_FALSE(nums[9].is_unsigned);
  EXPECT_FALSE(nums[9].is_long);
  EXPECT_FALSE(nums[9].is_long_long);
  EXPECT_FALSE(nums[9].is_float);
  EXPECT_FALSE(nums[9].is_integer_literal());
  EXPECT_TRUE(nums[9].is_floating_literal());
  EXPECT_EQ(10, nums[9].radix);
  // EXPECT_EQ(std::pair(0.0, false), nums[9].to_floating_point());

  // 01238.
  EXPECT_FALSE(nums[10].has_error);
  EXPECT_TRUE(nums[10].has_period);
  EXPECT_FALSE(nums[10].has_exponent);
  EXPECT_FALSE(nums[10].is_unsigned);
  EXPECT_FALSE(nums[10].is_long);
  EXPECT_FALSE(nums[10].is_long_long);
  EXPECT_FALSE(nums[10].is_float);
  EXPECT_FALSE(nums[10].is_integer_literal());
  EXPECT_TRUE(nums[10].is_floating_literal());
  EXPECT_EQ(10, nums[10].radix);
  // EXPECT_EQ(std::pair(1238.0, false), nums[10].to_floating_point());

  // 0xabcde.ffP+1
  EXPECT_FALSE(nums[11].has_error);
  EXPECT_TRUE(nums[11].has_period);
  EXPECT_TRUE(nums[11].has_exponent);
  EXPECT_FALSE(nums[11].is_unsigned);
  EXPECT_FALSE(nums[11].is_long);
  EXPECT_FALSE(nums[11].is_long_long);
  EXPECT_FALSE(nums[11].is_float);
  EXPECT_FALSE(nums[11].is_integer_literal());
  EXPECT_TRUE(nums[11].is_floating_literal());
  EXPECT_EQ(16, nums[11].radix);
  // EXPECT_EQ(std::pair(1.40742e+06, false), nums[11].to_floating_point());

  // 0xep1f
  EXPECT_FALSE(nums[12].has_error);
  EXPECT_FALSE(nums[12].has_period);
  EXPECT_TRUE(nums[12].has_exponent);
  EXPECT_FALSE(nums[12].is_unsigned);
  EXPECT_FALSE(nums[12].is_long);
  EXPECT_FALSE(nums[12].is_long_long);
  EXPECT_TRUE(nums[12].is_float);
  EXPECT_FALSE(nums[12].is_integer_literal());
  EXPECT_TRUE(nums[12].is_floating_literal());
  EXPECT_EQ(16, nums[12].radix);
  // EXPECT_EQ(std::pair(28.0, false), nums[12].to_floating_point());

  // 0x.f
  EXPECT_TRUE(nums[13].has_error);
}

TEST_F(LiteralParserTest, charConstants)
{
  auto chars = parse_char_constants(
    "'A' "
    "'\\x' "
    "u'\\u00A8' "
    "u'\\u00A' "
    "'abcd' "
    "'\\u0080' "
    "'\\123' "
    "'\\777'\n");

  ASSERT_EQ(8, chars.size());

  // 'A'
  EXPECT_FALSE(chars[0].has_error);
  EXPECT_FALSE(chars[0].is_multibyte);
  EXPECT_EQ(TokenKind::char_constant, chars[0].kind);
  EXPECT_EQ(65, chars[0].value);

  // '\x'
  EXPECT_TRUE(chars[1].has_error);

  // u'\u00A8'
  EXPECT_FALSE(chars[2].has_error);
  EXPECT_FALSE(chars[2].is_multibyte);
  EXPECT_EQ(TokenKind::utf16_char_constant, chars[2].kind);
  EXPECT_EQ(168, chars[2].value);

  // u'\u00A'
  EXPECT_TRUE(chars[3].has_error);

  // 'abcd'
  EXPECT_FALSE(chars[4].has_error);
  EXPECT_TRUE(chars[4].is_multibyte);
  EXPECT_EQ(TokenKind::char_constant, chars[4].kind);
  EXPECT_EQ(1633837924u, chars[4].value);

  // '\u0080'
  EXPECT_TRUE(chars[5].has_error);
  EXPECT_EQ(-128u, chars[5].value);

  // '\123'
  EXPECT_FALSE(chars[6].has_error);
  EXPECT_FALSE(chars[6].is_multibyte);
  EXPECT_EQ(TokenKind::char_constant, chars[6].kind);
  EXPECT_EQ(83, chars[6].value);

  // '\777'
  EXPECT_TRUE(chars[7].has_error);
}

TEST_F(LiteralParserTest, stringLiteral)
{
  auto str = parse_string_literal("\"foo bar\"");

  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::string_literal, str.kind);
  EXPECT_EQ(1, str.char_byte_width);
  EXPECT_EQ(7, str.byte_length());
  EXPECT_EQ(7, str.num_string_chars());
  EXPECT_STREQ("foo bar", str.string().data());
}

TEST_F(LiteralParserTest, stringLiteralUTF8)
{
  auto str = parse_string_literal("u8\"foo\"");
  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::utf8_string_literal, str.kind);
  EXPECT_EQ(1, str.char_byte_width);
  EXPECT_EQ(3, str.byte_length());
  EXPECT_EQ(3, str.num_string_chars());
  EXPECT_STREQ("foo", str.string().data());
}

TEST_F(LiteralParserTest, stringLiteralUTF16)
{
  auto str = parse_string_literal("u\"foo\"");
  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::utf16_string_literal, str.kind);
  EXPECT_EQ(2, str.char_byte_width);
  EXPECT_EQ(2*3, str.byte_length());
  EXPECT_EQ(3, str.num_string_chars());

  auto as_utf16 = reinterpret_cast<const uint16_t *>(str.string().data());
  EXPECT_EQ(u'f', as_utf16[0]);
  EXPECT_EQ(u'o', as_utf16[1]);
  EXPECT_EQ(u'o', as_utf16[2]);
}

TEST_F(LiteralParserTest, stringLiteralUTF32)
{
  auto str = parse_string_literal("U\"foo\"");
  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::utf32_string_literal, str.kind);
  EXPECT_EQ(4, str.char_byte_width);
  EXPECT_EQ(4*3, str.byte_length());
  EXPECT_EQ(3, str.num_string_chars());

  auto as_utf32 = reinterpret_cast<const uint32_t *>(str.string().data());
  EXPECT_EQ(U'f', as_utf32[0]);
  EXPECT_EQ(U'o', as_utf32[1]);
  EXPECT_EQ(U'o', as_utf32[2]);
}

TEST_F(LiteralParserTest, stringLiteralContatenation)
{
  auto str = parse_string_literal("\"foo\" \"bar\"");

  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::string_literal, str.kind);
  EXPECT_EQ(1, str.char_byte_width);
  EXPECT_EQ(6, str.byte_length());
  EXPECT_EQ(6, str.num_string_chars());
  EXPECT_STREQ("foobar", str.string().data());
}

TEST_F(LiteralParserTest, stringLiteralEmpty)
{
  auto str = parse_string_literal("\"\"");

  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::string_literal, str.kind);
  EXPECT_EQ(1, str.char_byte_width);
  EXPECT_EQ(0, str.byte_length());
  EXPECT_EQ(0, str.num_string_chars());
  EXPECT_STREQ("", str.string().data());
}

TEST_F(LiteralParserTest, stringLiteralConcatAsciiAndOtherKind)
{
  auto str = parse_string_literal("\"good\" u8\" foo\"");

  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::utf8_string_literal, str.kind);
  EXPECT_EQ(1, str.char_byte_width);
  EXPECT_EQ(8, str.byte_length());
  EXPECT_EQ(8, str.num_string_chars());
  EXPECT_STREQ("good foo", str.string().data());
}

TEST_F(LiteralParserTest, stringLiteralConcatDifferentKinds)
{
  auto str =
    parse_string_literal("u8\"bad\" \" string\" L\" concat\" L\"!\"");
  EXPECT_TRUE(str.has_error);
}

TEST_F(LiteralParserTest, stringLiteralUCNs)
{
  auto utf32_str = parse_string_literal("U\"\U00010437\"");

  EXPECT_FALSE(utf32_str.has_error);
  EXPECT_EQ(TokenKind::utf32_string_literal, utf32_str.kind);
  EXPECT_EQ(4, utf32_str.char_byte_width);
  EXPECT_EQ(4, utf32_str.byte_length());
  EXPECT_EQ(1, utf32_str.num_string_chars());

  uint32_t utf32_code_point{};
  std::memcpy(&utf32_code_point, utf32_str.string().data(),
              sizeof(utf32_code_point));
  EXPECT_EQ(0x10437, utf32_code_point);
}

TEST_F(LiteralParserTest, stringLiteralWithUnicodeChars)
{
  auto str = parse_string_literal("u8\"𐐷\"");

  EXPECT_FALSE(str.has_error);
  EXPECT_EQ(TokenKind::utf8_string_literal, str.kind);
  EXPECT_EQ(1, str.char_byte_width);
  EXPECT_EQ(4, str.byte_length());
  EXPECT_EQ(4, str.num_string_chars());

  EXPECT_EQ('\xF0', str.string()[0]);
  EXPECT_EQ('\x90', str.string()[1]);
  EXPECT_EQ('\x90', str.string()[2]);
  EXPECT_EQ('\xB7', str.string()[3]);
}

} // namespace
