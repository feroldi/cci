#include "../compiler_fixture.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/span.hpp"
#include "gtest/gtest.h"
#include <ostream>
#include <string>
#include <string_view>

using cci::Scanner;
using cci::Token;
using cci::TokenKind;
using cci::diag::Diag;

namespace {

struct ScannerTest : cci::test::CompilerFixture
{
protected:
    auto create_lex(std::string_view source) -> Scanner
    {
        const auto &file = create_filemap("main.c", std::string(source));
        Scanner scanner(file, diag_handler);
        return scanner;
    }

    auto scan(std::string_view source) -> std::vector<Token>
    {
        auto l = create_lex(source);
        std::vector<Token> toks;

        while (true)
        {
            auto tok = l.next_token();
            if (tok.is(TokenKind::eof))
                break;
            toks.push_back(tok);
        }

        return toks;
    }

    auto check_lex(std::string_view source,
                   span<std::pair<TokenKind, std::string>> expected_toks)
        -> std::vector<Token>
    {
        auto toks = this->scan(source);
        EXPECT_EQ(expected_toks.size(), toks.size());
        for (ptrdiff_t i = 0; i != expected_toks.size(); ++i)
        {
            EXPECT_EQ(expected_toks[i].first, toks[i].category)
                << "index: " << i;
            EXPECT_EQ(expected_toks[i].second, this->get_lexeme(toks[i]))
                << "index: " << i;
        }
        return toks;
    }
};

TEST_F(ScannerTest, regressionNewlineAsUnknownToken)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::identifier, "foo"},
    };

    check_lex("\\\n\nfoo\n", expected_toks);
}

TEST_F(ScannerTest, escapedNewLine)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::identifier, "foobar"}, {TokenKind::identifier, "foo"},
        {TokenKind::unknown, "\\"},        {TokenKind::identifier, "bar"},
        {TokenKind::identifier, "foo"},    {TokenKind::unknown, "\\"},
        {TokenKind::identifier, "bar"},    {TokenKind::identifier, "foo"},
        {TokenKind::identifier, "bar"},
    };

    check_lex(
        "foo\\\nbar\n"
        "foo\\ \nbar\n"
        "foo\\bar\n"
        "foo\\\n\nbar\n",
        expected_toks);

    EXPECT_EQ(Diag::unknown_character, pop_diag().msg);
    EXPECT_EQ(Diag::unknown_character, pop_diag().msg);
}

TEST_F(ScannerTest, trigraphEscapedNewline)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::identifier, "foobar"}, {TokenKind::identifier, "foo"},
        {TokenKind::unknown, "\\"},        {TokenKind::identifier, "bar"},
        {TokenKind::identifier, "foo"},    {TokenKind::unknown, "\\"},
        {TokenKind::identifier, "bar"},    {TokenKind::identifier, "foo"},
        {TokenKind::identifier, "bar"},
    };

    check_lex(
        "foo\?\?/\nbar\n"
        "foo\?\?/ \nbar\n"
        "foo\?\?/bar\n"
        "foo\?\?/\n\nbar\n",
        expected_toks);

    EXPECT_EQ(Diag::unknown_character, pop_diag().msg);
    EXPECT_EQ(Diag::unknown_character, pop_diag().msg);
}

TEST_F(ScannerTest, escapedTrigraphEscape)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::unknown, "\"foo\\\?\?/"},
        {TokenKind::unknown, "\""},
    };

    check_lex(
        "\"foo\\\?\?/\n"
        "\"\n",
        expected_toks);

    EXPECT_EQ(Diag::unterminated_string_literal, pop_diag().msg);
    EXPECT_EQ(Diag::unterminated_string_literal, pop_diag().msg);
}

TEST_F(ScannerTest, eofToken)
{
    auto scanner = create_lex("foo\n");
    scanner.next_token();
    EXPECT_EQ(TokenKind::eof, scanner.next_token().category);
    EXPECT_EQ(TokenKind::eof, scanner.next_token().category);
}

TEST_F(ScannerTest, maximallyMunchPunctuation)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::lessless, "<<"},
        {TokenKind::lessequal, "<="},
    };

    check_lex("<<<=\n", expected_toks);
}

TEST_F(ScannerTest, universalCharacterName)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::identifier, "\\u037e"}, // GREEK-QUESTION-MARK (U+037E)
        {TokenKind::identifier, "\\U0000037e"}, // GREEK-QUESTION-MARK (U+037E)
        {TokenKind::identifier, "\\U0001F648"}, // SEE-NO-EVIL MONKEY (U+1F648)
        {TokenKind::identifier, "\\u01234"},
    };

    check_lex(
        "\\u037e\n"
        "\\U0000037e\n"
        "\\U0001F648\n"
        "\\u01234\n", // Note: '4' is not part of the UCN, but the identifier.
        expected_toks);

    EXPECT_EQ(0, diag_handler.err_count());

    // Incomplete UCNs.
    // FIXME: These checks should be removed once regression tests are written.
    scan("\\u012\n");
    EXPECT_EQ(Diag::incomplete_ucn, pop_diag().msg);
    EXPECT_EQ(Diag::unknown_character, pop_diag().msg);

    scan("\\U1F648\n");
    EXPECT_EQ(Diag::incomplete_ucn, pop_diag().msg);
    EXPECT_EQ(Diag::unknown_character, pop_diag().msg);
}

TEST_F(ScannerTest, identifiers)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::identifier, "Alpha"},
        {TokenKind::identifier, "AlphaNum123"},
        {TokenKind::identifier, "_under_line"},
        {TokenKind::identifier, "$$$dol$lar$$$"},
    };

    check_lex("Alpha AlphaNum123 _under_line $$$dol$lar$$$\n", expected_toks);
}

TEST_F(ScannerTest, keywords)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::kw_auto, "auto"},
        {TokenKind::kw_break, "break"},
        {TokenKind::kw_case, "case"},
        {TokenKind::kw_char, "char"},
        {TokenKind::kw_const, "const"},
        {TokenKind::kw_continue, "continue"},
        {TokenKind::kw_default, "default"},
        {TokenKind::kw_do, "do"},
        {TokenKind::kw_double, "double"},
        {TokenKind::kw_else, "else"},
        {TokenKind::kw_enum, "enum"},
        {TokenKind::kw_extern, "extern"},
        {TokenKind::kw_float, "float"},
        {TokenKind::kw_for, "for"},
        {TokenKind::kw_goto, "goto"},
        {TokenKind::kw_if, "if"},
        {TokenKind::kw_inline, "inline"},
        {TokenKind::kw_int, "int"},
        {TokenKind::kw_long, "long"},
        {TokenKind::kw_register, "register"},
        {TokenKind::kw_restrict, "restrict"},
        {TokenKind::kw_return, "return"},
        {TokenKind::kw_short, "short"},
        {TokenKind::kw_signed, "signed"},
        {TokenKind::kw_sizeof, "sizeof"},
        {TokenKind::kw_static, "static"},
        {TokenKind::kw_struct, "struct"},
        {TokenKind::kw_switch, "switch"},
        {TokenKind::kw_typedef, "typedef"},
        {TokenKind::kw_union, "union"},
        {TokenKind::kw_unsigned, "unsigned"},
        {TokenKind::kw_void, "void"},
        {TokenKind::kw_volatile, "volatile"},
        {TokenKind::kw_while, "while"},
        {TokenKind::kw__Alignas, "_Alignas"},
        {TokenKind::kw__Alignof, "_Alignof"},
        {TokenKind::kw__Atomic, "_Atomic"},
        {TokenKind::kw__Bool, "_Bool"},
        {TokenKind::kw__Complex, "_Complex"},
        {TokenKind::kw__Generic, "_Generic"},
        {TokenKind::kw__Imaginary, "_Imaginary"},
        {TokenKind::kw__Noreturn, "_Noreturn"},
        {TokenKind::kw__Static_assert, "_Static_assert"},
        {TokenKind::kw__Thread_local, "_Thread_local"},
    };

    check_lex(
        "auto break case char const continue default do double else enum "
        "extern "
        "float for goto if inline int long register restrict return short "
        "signed "
        "sizeof static struct switch typedef union unsigned void volatile "
        "while "
        "_Alignas _Alignof _Atomic _Bool _Complex _Generic _Imaginary "
        "_Noreturn "
        "_Static_assert _Thread_local\n",
        expected_toks);
}

TEST_F(ScannerTest, numericConstants)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::numeric_constant, "42ULL"},
        {TokenKind::numeric_constant, "3.14f"},
        {TokenKind::numeric_constant, "161.80e-3"},
        {TokenKind::numeric_constant, "1.9E377P+1"},
        {TokenKind::numeric_constant, ".999"},
        {TokenKind::numeric_constant, "0."},
    };

    check_lex("42ULL 3.14f 161.80e-3 1.9E377P+1 .999 0.\n", expected_toks);
}

TEST_F(ScannerTest, comments)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::identifier, "foo"},
        {TokenKind::identifier, "bar"},
        {TokenKind::string_literal, "\"a//b\""},
        {TokenKind::identifier, "f"},
        {TokenKind::equal, "="},
        {TokenKind::identifier, "g"},
        {TokenKind::slash, "/"},
        {TokenKind::identifier, "h"},
        {TokenKind::identifier, "z"},
        {TokenKind::identifier, "m"},
        {TokenKind::equal, "="},
        {TokenKind::identifier, "n"},
        {TokenKind::plus, "+"},
        {TokenKind::identifier, "p"},
    };

    check_lex(
        R"(foo // this comment should be skipped, \
so it is
// skip this too and \ignore\ these \back\slashes
/\
/ and this too
bar
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
)",
        expected_toks);
}

TEST_F(ScannerTest, charConstants)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::char_constant, "'a'"},
        {TokenKind::wide_char_constant, "L'b'"},
        {TokenKind::utf16_char_constant, "u'c'"},
        {TokenKind::utf32_char_constant, "U'd'"},
        {TokenKind::char_constant, "'\\''"},
        {TokenKind::char_constant, "'\"'"},
    };

    check_lex(R"('a' L'b' u'c' U'd' '\'' '"')"
              "\n",
              expected_toks);

    // Empty character constant.
    scan("''\n");
    EXPECT_EQ(Diag::char_const_empty, pop_diag().msg);
}

TEST_F(ScannerTest, stringLiterals)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::string_literal, "\"\""},
        {TokenKind::string_literal, "\"foo\""},
        {TokenKind::wide_string_literal, "L\"bar\""},
        {TokenKind::utf16_string_literal, "u\"baz\""},
        {TokenKind::utf32_string_literal, "U\"fizz\""},
        {TokenKind::utf8_string_literal, "u8\"buzz\""},
        {TokenKind::string_literal, "\"\\\"\""},
        {TokenKind::string_literal, "\"'\""},
    };

    check_lex(R"("" "foo" L"bar" u"baz" U"fizz" u8"buzz" "\"" "'")"
              "\n",
              expected_toks);
}

TEST_F(ScannerTest, punctuators)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::l_bracket, "["},
        {TokenKind::r_bracket, "]"},
        {TokenKind::l_paren, "("},
        {TokenKind::r_paren, ")"},
        {TokenKind::l_brace, "{"},
        {TokenKind::r_brace, "}"},
        {TokenKind::period, "."},
        {TokenKind::arrow, "->"},
        {TokenKind::plusplus, "++"},
        {TokenKind::minusminus, "--"},
        {TokenKind::ampersand, "&"},
        {TokenKind::star, "*"},
        {TokenKind::plus, "+"},
        {TokenKind::minus, "-"},
        {TokenKind::tilde, "~"},
        {TokenKind::exclama, "!"},
        {TokenKind::slash, "/"},
        {TokenKind::percent, "%"},
        {TokenKind::lessless, "<<"},
        {TokenKind::greatergreater, ">>"},
        {TokenKind::less, "<"},
        {TokenKind::greater, ">"},
        {TokenKind::lessequal, "<="},
        {TokenKind::greaterequal, ">="},
        {TokenKind::question, "?"},
        {TokenKind::colon, ":"},
        {TokenKind::semi, ";"},
        {TokenKind::ellipsis, "..."},
        {TokenKind::equal, "="},
        {TokenKind::starequal, "*="},
        {TokenKind::slashequal, "/="},
        {TokenKind::percentequal, "%="},
        {TokenKind::plusequal, "+="},
        {TokenKind::minusequal, "-="},
        {TokenKind::lesslessequal, "<<="},
        {TokenKind::comma, ","},
        {TokenKind::hash, "#"},
        {TokenKind::hashhash, "##"},
        {TokenKind::l_bracket, "<:"},
        {TokenKind::r_bracket, ":>"},
        {TokenKind::l_brace, "<%"},
        {TokenKind::r_brace, "%>"},
        {TokenKind::hash, "%:"},
        {TokenKind::hashhash, "%:%:"},
        {TokenKind::equalequal, "=="},
        {TokenKind::greatergreaterequal, ">>="},
        {TokenKind::exclamaequal, "!="},
        {TokenKind::ampequal, "&="},
        {TokenKind::caret, "^"},
        {TokenKind::pipe, "|"},
        {TokenKind::caretequal, "^="},
        {TokenKind::ampamp, "&&"},
        {TokenKind::pipepipe, "||"},
        {TokenKind::pipeequal, "|="},
    };

    check_lex(
        "[ ] ( ) { } . -> ++ -- & * + - ~ ! / % << >> < > <= >= ? : ; ... = *= "
        "/= "
        "%= += -= <<= , # ## <: :> <% %> %: %:%: == >>= != &= ^ | ^= && || "
        "|=\n",
        expected_toks);
}

TEST_F(ScannerTest, trigraphs)
{
    std::vector<std::pair<TokenKind, std::string>> expected_toks{
        {TokenKind::hash, "#"},      {TokenKind::l_bracket, "["},
        {TokenKind::r_bracket, "]"}, {TokenKind::caret, "^"},
        {TokenKind::l_brace, "{"},   {TokenKind::r_brace, "}"},
        {TokenKind::pipe, "|"},      {TokenKind::tilde, "~"},
    };

    // ??/ becomes \, which escapes the last but one new line.
    check_lex("\?\?= \?\?( \?\?) \?\?' \?\?< \?\?> \?\?! \?\?- \?\?/\n\n",
              expected_toks);
}

} // namespace
