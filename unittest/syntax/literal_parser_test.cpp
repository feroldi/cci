#include "../compiler_fixture.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/literal_parser.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/span.hpp"
#include "cci/util/unicode.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

using namespace cci::diag;
using namespace cci;

namespace {

struct LiteralParserTest : cci::test::CompilerFixture
{
protected:
    std::unique_ptr<Scanner> scanner;
    TargetInfo target;

    LiteralParserTest() : scanner(), target() {}

    auto scan(std::string source) -> std::vector<Token>
    {
        const auto &file =
            this->source_map.create_owned_filemap("main.c", std::move(source));
        scanner = std::make_unique<Scanner>(file, diag_handler);
        std::vector<Token> toks;

        while (true)
        {
            auto tok = scanner->next_token();
            if (tok.is(TokenKind::eof))
                break;
            toks.push_back(tok);
        }

        return toks;
    }

    auto parse_numeric_constant(std::string source) -> NumericConstantParser
    {
        EXPECT_FALSE(source.empty());
        auto lexed_toks = scan(std::move(source));
        EXPECT_EQ(1, lexed_toks.size());
        Token tok = lexed_toks.front();
        NumericConstantParser parser(*this->scanner, get_lexeme_view(tok),
                                     tok.location());
        return parser;
    }

    auto parse_char_constant(std::string source) -> CharConstantParser
    {
        EXPECT_FALSE(source.empty());
        auto lexed_toks = scan(std::move(source));
        EXPECT_EQ(1, lexed_toks.size());
        Token tok = lexed_toks.front();
        CharConstantParser parser(*this->scanner, get_lexeme_view(tok),
                                  tok.location(), tok.category, target);
        return parser;
    }

    auto parse_string_literal(std::string source) -> StringLiteralParser
    {
        auto string_toks = scan(std::move(source));
        StringLiteralParser parser(*this->scanner, string_toks, target);
        return parser;
    }
};

TEST_F(LiteralParserTest, numConstUnsignedLongIntWithSuffix)
{
    auto parsed_num = parse_numeric_constant("42ul");

    EXPECT_TRUE(parsed_num.is_unsigned);
    EXPECT_TRUE(parsed_num.is_long);
    EXPECT_TRUE(parsed_num.is_integer_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_period);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_floating_literal());

    EXPECT_EQ(10, parsed_num.radix);
    EXPECT_EQ(std::pair(42ul, false), parsed_num.to_integer());
}

TEST_F(LiteralParserTest, numConstIntOctal)
{
    auto parsed_num = parse_numeric_constant("042");

    EXPECT_TRUE(parsed_num.is_integer_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_period);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_floating_literal());

    EXPECT_EQ(8, parsed_num.radix);
    EXPECT_EQ(std::pair(34ul, false), parsed_num.to_integer());
}

TEST_F(LiteralParserTest, numConstUnsignedLongLongIntHex)
{
    auto parsed_num = parse_numeric_constant("0xDEADc0dellu");

    EXPECT_TRUE(parsed_num.is_unsigned);
    EXPECT_TRUE(parsed_num.is_long_long);
    EXPECT_TRUE(parsed_num.is_integer_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_period);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_floating_literal());

    EXPECT_EQ(16, parsed_num.radix);
    EXPECT_EQ(std::pair(3735929054ul, false), parsed_num.to_integer());
}

TEST_F(LiteralParserTest, numConstUnsignedSuffixAppearsTwice)
{
    auto parsed_num = parse_numeric_constant("0uU");

    EXPECT_TRUE(parsed_num.has_error);
    EXPECT_EQ(Diag::invalid_suffix, pop_diag().msg);
}

TEST_F(LiteralParserTest, numConstLongAndLongLongSuffixes)
{
    auto parsed_num = parse_numeric_constant("0LLL");

    EXPECT_TRUE(parsed_num.has_error);
    EXPECT_EQ(Diag::invalid_suffix, pop_diag().msg);
}

TEST_F(LiteralParserTest, numConstInvalidOctalDigit)
{
    auto parsed_num = parse_numeric_constant("0128");

    EXPECT_TRUE(parsed_num.has_error);
    EXPECT_EQ(Diag::invalid_digit, pop_diag().msg);
}

TEST_F(LiteralParserTest, numConstDoubleHasExponent)
{
    auto parsed_num = parse_numeric_constant("314e10");

    EXPECT_TRUE(parsed_num.has_exponent);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_period);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(10, parsed_num.radix);

    // TODO: Implement float conversion. Also, use EXPECT_DOUBLE_EQ.
    // EXPECT_EQ(std::pair(314e10, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstDoubleHasPeriodNoRightDigits)
{
    auto parsed_num = parse_numeric_constant("1.");

    EXPECT_TRUE(parsed_num.has_period);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(10, parsed_num.radix);

    // TODO: Implement float conversion. Also, use EXPECT_DOUBLE_EQ.
    // EXPECT_EQ(std::pair(1.0, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstFloatHasPeriodNoRightDigits)
{
    auto parsed_num = parse_numeric_constant("1.f");

    EXPECT_TRUE(parsed_num.has_period);
    EXPECT_TRUE(parsed_num.is_float);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(10, parsed_num.radix);

    // TODO: Implement float conversion. Also, use EXPECT_FLOAT_EQ.
    // EXPECT_EQ(std::pair(1.0f, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstMissingExponentDigits)
{
    auto parsed_num = parse_numeric_constant("1.ef");

    EXPECT_TRUE(parsed_num.has_error);
    EXPECT_EQ(Diag::missing_exponent_digits, pop_diag().msg);
}

TEST_F(LiteralParserTest, numConstDoubleHasPeriodNoLeftDigits)
{
    auto parsed_num = parse_numeric_constant(".0");

    EXPECT_TRUE(parsed_num.has_period);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(10, parsed_num.radix);

    // TODO: Implement float conversion. Also, use EXPECT_DOUBLE_EQ.
    // EXPECT_EQ(std::pair(0.0, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstDoubleStartsWithZeroHasPeriod)
{
    auto parsed_num = parse_numeric_constant("01238.");

    EXPECT_TRUE(parsed_num.has_period);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_exponent);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(10, parsed_num.radix);
    // EXPECT_EQ(std::pair(1238.0, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstDoubleHexHasPeriodExponentPlus)
{
    auto parsed_num = parse_numeric_constant("0xabcde.ffP+1");

    EXPECT_TRUE(parsed_num.has_period);
    EXPECT_TRUE(parsed_num.has_exponent);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_float);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(16, parsed_num.radix);

    // EXPECT_EQ(std::pair(1.40742e+06, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstFloatHexHasExponent)
{
    auto parsed_num = parse_numeric_constant("0xep1f");

    EXPECT_TRUE(parsed_num.has_exponent);
    EXPECT_TRUE(parsed_num.is_float);
    EXPECT_TRUE(parsed_num.is_floating_literal());

    EXPECT_FALSE(parsed_num.has_error);
    EXPECT_FALSE(parsed_num.has_period);
    EXPECT_FALSE(parsed_num.is_unsigned);
    EXPECT_FALSE(parsed_num.is_long);
    EXPECT_FALSE(parsed_num.is_long_long);
    EXPECT_FALSE(parsed_num.is_integer_literal());

    EXPECT_EQ(16, parsed_num.radix);

    // EXPECT_EQ(std::pair(28.0, false), parsed_num.to_floating_point());
}

TEST_F(LiteralParserTest, numConstMissingBinaryExponent)
{
    auto parsed_num = parse_numeric_constant("0x.f");

    EXPECT_TRUE(parsed_num.has_error);
    EXPECT_EQ(Diag::missing_binary_exponent, pop_diag().msg);
}

TEST_F(LiteralParserTest, charConstBasic)
{
    auto parsed_char = parse_char_constant("'A'");

    EXPECT_FALSE(parsed_char.has_error);
    EXPECT_FALSE(parsed_char.is_multibyte);

    EXPECT_EQ(TokenKind::char_constant, parsed_char.category);
    EXPECT_EQ(65, parsed_char.value);
}

TEST_F(LiteralParserTest, charConstMissingEscapeDigits)
{
    auto parsed_char = parse_char_constant(R"('\x')");

    EXPECT_TRUE(parsed_char.has_error);
    EXPECT_EQ(Diag::missing_escape_digits, pop_diag().msg);
}

TEST_F(LiteralParserTest, charConstUtf16)
{
    auto parsed_char = parse_char_constant(R"(u'\u00A8')");

    EXPECT_FALSE(parsed_char.has_error);
    EXPECT_FALSE(parsed_char.is_multibyte);

    EXPECT_EQ(TokenKind::utf16_char_constant, parsed_char.category);
    EXPECT_EQ(168, parsed_char.value);
}

TEST_F(LiteralParserTest, charConstInvalidUcn)
{
    auto parsed_char = parse_char_constant(R"(u'\u00A')");

    EXPECT_TRUE(parsed_char.has_error);
    EXPECT_EQ(Diag::invalid_ucn, pop_diag().msg);
}

TEST_F(LiteralParserTest, charConstMultibyte)
{
    auto parsed_char = parse_char_constant(R"('abcd')");

    EXPECT_FALSE(parsed_char.has_error);
    EXPECT_TRUE(parsed_char.is_multibyte);

    EXPECT_EQ(TokenKind::char_constant, parsed_char.category);
    EXPECT_EQ(1633837924u, parsed_char.value);
}

TEST_F(LiteralParserTest, charConstUnicodeTooLarge)
{
    auto parsed_char = parse_char_constant(R"('\u0080')");

    EXPECT_TRUE(parsed_char.has_error);

    EXPECT_EQ(-128u, parsed_char.value);
    EXPECT_EQ(Diag::unicode_too_large_for_unit, pop_diag().msg);
}

TEST_F(LiteralParserTest, charConstOctalEscapeSequence)
{
    auto parsed_char = parse_char_constant(R"('\123')");

    EXPECT_FALSE(parsed_char.has_error);
    EXPECT_FALSE(parsed_char.is_multibyte);

    EXPECT_EQ(TokenKind::char_constant, parsed_char.category);
    EXPECT_EQ(83, parsed_char.value);
}

TEST_F(LiteralParserTest, charConstOctalEscapeOutOfByteSpan)
{
    auto parsed_char = parse_char_constant(R"('\777')");

    EXPECT_TRUE(parsed_char.has_error);
    EXPECT_EQ(Diag::escape_out_of_range, pop_diag().msg);
}

TEST_F(LiteralParserTest, strLitAsciiChars)
{
    auto parsed_str = parse_string_literal(R"("foo bar")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::string_literal, parsed_str.category);

    EXPECT_EQ(1, parsed_str.char_byte_width);
    EXPECT_EQ(7, parsed_str.byte_length());
    EXPECT_EQ(7, parsed_str.num_string_chars());

    EXPECT_STREQ("foo bar", parsed_str.string().data());
}

TEST_F(LiteralParserTest, strLitUTF8)
{
    auto parsed_str = parse_string_literal(R"(u8"foo")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::utf8_string_literal, parsed_str.category);

    EXPECT_EQ(1, parsed_str.char_byte_width);
    EXPECT_EQ(3, parsed_str.byte_length());
    EXPECT_EQ(3, parsed_str.num_string_chars());

    EXPECT_STREQ("foo", parsed_str.string().data());
}

TEST_F(LiteralParserTest, strLitUTF16)
{
    auto parsed_str = parse_string_literal(R"(u"foo")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::utf16_string_literal, parsed_str.category);

    EXPECT_EQ(2, parsed_str.char_byte_width);
    EXPECT_EQ(2 * 3, parsed_str.byte_length());
    EXPECT_EQ(3, parsed_str.num_string_chars());

    auto as_utf16 = parsed_str.string_as_utf16();

    EXPECT_EQ(u'f', as_utf16[0]);
    EXPECT_EQ(u'o', as_utf16[1]);
    EXPECT_EQ(u'o', as_utf16[2]);
}

TEST_F(LiteralParserTest, strLitUTF32)
{
    auto parsed_str = parse_string_literal(R"(U"foo")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::utf32_string_literal, parsed_str.category);

    EXPECT_EQ(4, parsed_str.char_byte_width);
    EXPECT_EQ(4 * 3, parsed_str.byte_length());
    EXPECT_EQ(3, parsed_str.num_string_chars());

    auto as_utf32 = parsed_str.string_as_utf32();

    EXPECT_EQ(U'f', as_utf32[0]);
    EXPECT_EQ(U'o', as_utf32[1]);
    EXPECT_EQ(U'o', as_utf32[2]);
}

TEST_F(LiteralParserTest, strLitContatenation)
{
    auto parsed_str = parse_string_literal(R"("foo" "bar")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::string_literal, parsed_str.category);

    EXPECT_EQ(1, parsed_str.char_byte_width);
    EXPECT_EQ(6, parsed_str.byte_length());
    EXPECT_EQ(6, parsed_str.num_string_chars());

    EXPECT_STREQ("foobar", parsed_str.string().data());
}

TEST_F(LiteralParserTest, strLitEmpty)
{
    auto parsed_str = parse_string_literal(R"("")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::string_literal, parsed_str.category);

    EXPECT_EQ(1, parsed_str.char_byte_width);
    EXPECT_EQ(0, parsed_str.byte_length());
    EXPECT_EQ(0, parsed_str.num_string_chars());

    EXPECT_STREQ("", parsed_str.string().data());
}

TEST_F(LiteralParserTest, strLitConcatAsciiAndOtherKind)
{
    auto parsed_str = parse_string_literal(R"("good" u8" foo")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::utf8_string_literal, parsed_str.category);

    EXPECT_EQ(1, parsed_str.char_byte_width);
    EXPECT_EQ(8, parsed_str.byte_length());
    EXPECT_EQ(8, parsed_str.num_string_chars());

    EXPECT_STREQ("good foo", parsed_str.string().data());
}

TEST_F(LiteralParserTest, strLitConcatDifferentKinds)
{
    auto parsed_str =
        parse_string_literal(R"(u8"bad" " string" L" concat" L"!")");
    EXPECT_TRUE(parsed_str.has_error);
    EXPECT_EQ(Diag::nonstandard_string_concat, pop_diag().msg);
    EXPECT_EQ(Diag::nonstandard_string_concat, pop_diag().msg);
}

TEST_F(LiteralParserTest, strLitUCNs)
{
    auto parsed_str = parse_string_literal(R"(U"\U00010437")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::utf32_string_literal, parsed_str.category);

    EXPECT_EQ(4, parsed_str.char_byte_width);
    EXPECT_EQ(4, parsed_str.byte_length());
    EXPECT_EQ(1, parsed_str.num_string_chars());

    uint32_t utf32_code_point{};
    std::memcpy(&utf32_code_point, parsed_str.string().data(),
                sizeof(utf32_code_point));
    EXPECT_EQ(0x10437, utf32_code_point);
}

TEST_F(LiteralParserTest, strLitWithUnicodeChars)
{
    auto parsed_str = parse_string_literal(R"(u8"𐐷")");

    EXPECT_FALSE(parsed_str.has_error);
    EXPECT_EQ(TokenKind::utf8_string_literal, parsed_str.category);

    EXPECT_EQ(1, parsed_str.char_byte_width);
    EXPECT_EQ(4, parsed_str.byte_length());
    EXPECT_EQ(4, parsed_str.num_string_chars());

    EXPECT_EQ('\xF0', parsed_str.string()[0]);
    EXPECT_EQ('\x90', parsed_str.string()[1]);
    EXPECT_EQ('\x90', parsed_str.string()[2]);
    EXPECT_EQ('\xB7', parsed_str.string()[3]);
}

} // namespace
