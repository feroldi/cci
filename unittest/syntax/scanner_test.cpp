#include "cci/syntax/scanner.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/span.hpp"
#include "gtest/gtest.h"
#include <ostream>
#include <string>
#include <string_view>

namespace cci {
void PrintTo(const Category category, std::ostream *os) noexcept
{
  *os << to_string(category);
}
} // namespace cci

using namespace cci;

namespace {

struct ScannerTest : ::testing::Test
{
protected:
  srcmap::SourceMap source_map;
  diag::Handler diag_handler;

  ScannerTest() : source_map(), diag_handler(diag::ignoring_emitter(), source_map)
  {}

  auto create_lex(std::string_view source) -> Scanner
  {
    const auto &file =
      source_map.create_owned_filemap("main.c", std::string(source));
    Scanner scan(source_map, file.start_loc, file.src_begin(), file.src_end(),
              diag_handler);
    return scan;
  }

  auto scan(std::string_view source) -> std::vector<Token>
  {
    auto l = create_lex(source);
    std::vector<Token> toks;

    while (true)
    {
      auto tok = l.next_token();
      if (tok.is(Category::eof))
        break;
      toks.push_back(tok);
    }

    return toks;
  }

  auto get_source_text(const Token &tok) const -> std::string_view
  {
    return source_map.range_to_snippet(tok.source_range());
  }

  auto get_lexeme(const Token &tok) const -> std::string
  {
    std::string lexeme;
    lexeme.resize(tok.size());
    const size_t len =
      Scanner::get_spelling_to_buffer(tok, lexeme.data(), this->source_map);
    lexeme.resize(len);
    return lexeme;
  }

  auto check_lex(std::string_view source,
                 span<std::pair<Category, std::string>> expected_toks)
    -> std::vector<Token>
  {
    auto toks = this->scan(source);
    EXPECT_EQ(expected_toks.size(), toks.size());
    for (ptrdiff_t i = 0; i != expected_toks.size(); ++i)
    {
      EXPECT_EQ(expected_toks[i].first, toks[i].category()) << "index: " << i;
      EXPECT_EQ(expected_toks[i].second, this->get_lexeme(toks[i]))
        << "index: " << i;
    }
    return toks;
  }
};

TEST_F(ScannerTest, regressionNewlineAsUnknownToken)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::identifier, "foo"},
  };

  check_lex("\\\n\nfoo\n", expected_toks);
}

TEST_F(ScannerTest, escapedNewLine)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::identifier, "foobar"}, {Category::identifier, "foo"},
    {Category::unknown, "\\"},        {Category::identifier, "bar"},
    {Category::identifier, "foo"},    {Category::unknown, "\\"},
    {Category::identifier, "bar"},    {Category::identifier, "foo"},
    {Category::identifier, "bar"},
  };

  check_lex(
    "foo\\\nbar\n"
    "foo\\ \nbar\n"
    "foo\\bar\n"
    "foo\\\n\nbar\n",
    expected_toks);
}

TEST_F(ScannerTest, trigraphEscapedNewline)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::identifier, "foobar"}, {Category::identifier, "foo"},
    {Category::unknown, "\\"},        {Category::identifier, "bar"},
    {Category::identifier, "foo"},    {Category::unknown, "\\"},
    {Category::identifier, "bar"},    {Category::identifier, "foo"},
    {Category::identifier, "bar"},
  };

  check_lex(
    "foo\?\?/\nbar\n"
    "foo\?\?/ \nbar\n"
    "foo\?\?/bar\n"
    "foo\?\?/\n\nbar\n",
    expected_toks);

  EXPECT_EQ(2, diag_handler.err_count());
}

TEST_F(ScannerTest, escapedTrigraphEscape)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::unknown, "\"foo\\\?\?/"},
    {Category::unknown, "\""},
  };

  check_lex(
    "\"foo\\\?\?/\n"
    "\"\n",
    expected_toks);
}

TEST_F(ScannerTest, eofToken)
{
  auto scanner = create_lex("foo\n");
  scanner.next_token();
  EXPECT_EQ(Category::eof, scanner.next_token().category());
  EXPECT_EQ(Category::eof, scanner.next_token().category());
}

TEST_F(ScannerTest, maximallyMunchPunctuation)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::lessless, "<<"},
    {Category::lessequal, "<="},
  };

  check_lex("<<<=\n", expected_toks);
}

TEST_F(ScannerTest, universalCharacterName)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::identifier, "\\u037e"}, // GREEK-QUESTION-MARK (U+037E)
    {Category::identifier, "\\U0000037e"}, // GREEK-QUESTION-MARK (U+037E)
    {Category::identifier, "\\U0001F648"}, // SEE-NO-EVIL MONKEY (U+1F648)
    {Category::identifier, "\\u01234"},
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
  EXPECT_EQ(2, diag_handler.err_count());

  scan("\\U1F648\n");
  EXPECT_EQ(4, diag_handler.err_count());
}

TEST_F(ScannerTest, identifiers)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::identifier, "Alpha"},
    {Category::identifier, "AlphaNum123"},
    {Category::identifier, "_under_line"},
    {Category::identifier, "$$$dol$lar$$$"},
  };

  check_lex("Alpha AlphaNum123 _under_line $$$dol$lar$$$\n", expected_toks);
}

TEST_F(ScannerTest, keywords)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::kw_auto, "auto"},
    {Category::kw_break, "break"},
    {Category::kw_case, "case"},
    {Category::kw_char, "char"},
    {Category::kw_const, "const"},
    {Category::kw_continue, "continue"},
    {Category::kw_default, "default"},
    {Category::kw_do, "do"},
    {Category::kw_double, "double"},
    {Category::kw_else, "else"},
    {Category::kw_enum, "enum"},
    {Category::kw_extern, "extern"},
    {Category::kw_float, "float"},
    {Category::kw_for, "for"},
    {Category::kw_goto, "goto"},
    {Category::kw_if, "if"},
    {Category::kw_inline, "inline"},
    {Category::kw_int, "int"},
    {Category::kw_long, "long"},
    {Category::kw_register, "register"},
    {Category::kw_restrict, "restrict"},
    {Category::kw_return, "return"},
    {Category::kw_short, "short"},
    {Category::kw_signed, "signed"},
    {Category::kw_sizeof, "sizeof"},
    {Category::kw_static, "static"},
    {Category::kw_struct, "struct"},
    {Category::kw_switch, "switch"},
    {Category::kw_typedef, "typedef"},
    {Category::kw_union, "union"},
    {Category::kw_unsigned, "unsigned"},
    {Category::kw_void, "void"},
    {Category::kw_volatile, "volatile"},
    {Category::kw_while, "while"},
    {Category::kw__Alignas, "_Alignas"},
    {Category::kw__Alignof, "_Alignof"},
    {Category::kw__Atomic, "_Atomic"},
    {Category::kw__Bool, "_Bool"},
    {Category::kw__Complex, "_Complex"},
    {Category::kw__Generic, "_Generic"},
    {Category::kw__Imaginary, "_Imaginary"},
    {Category::kw__Noreturn, "_Noreturn"},
    {Category::kw__Static_assert, "_Static_assert"},
    {Category::kw__Thread_local, "_Thread_local"},
  };

  check_lex(
    "auto break case char const continue default do double else enum extern "
    "float for goto if inline int long register restrict return short signed "
    "sizeof static struct switch typedef union unsigned void volatile while "
    "_Alignas _Alignof _Atomic _Bool _Complex _Generic _Imaginary _Noreturn "
    "_Static_assert _Thread_local\n",
    expected_toks);
}

TEST_F(ScannerTest, numericConstants)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::numeric_constant, "42ULL"},
    {Category::numeric_constant, "3.14f"},
    {Category::numeric_constant, "161.80e-3"},
    {Category::numeric_constant, "1.9E377P+1"},
    {Category::numeric_constant, ".999"},
    {Category::numeric_constant, "0."},
  };

  check_lex("42ULL 3.14f 161.80e-3 1.9E377P+1 .999 0.\n", expected_toks);
}

TEST_F(ScannerTest, comments)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::identifier, "foo"},
    {Category::identifier, "bar"},
    {Category::string_literal, "\"a//b\""},
    {Category::identifier, "f"},
    {Category::equal, "="},
    {Category::identifier, "g"},
    {Category::slash, "/"},
    {Category::identifier, "h"},
    {Category::identifier, "z"},
    {Category::identifier, "m"},
    {Category::equal, "="},
    {Category::identifier, "n"},
    {Category::plus, "+"},
    {Category::identifier, "p"},
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
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::char_constant, "'a'"},
    {Category::wide_char_constant, "L'b'"},
    {Category::utf16_char_constant, "u'c'"},
    {Category::utf32_char_constant, "U'd'"},
    {Category::char_constant, "'\\''"},
    {Category::char_constant, "'\"'"},
  };

  check_lex(R"('a' L'b' u'c' U'd' '\'' '"')"
            "\n",
            expected_toks);

  // Empty character constant.
  scan("''\n");
  EXPECT_EQ(1, diag_handler.err_count());
}

TEST_F(ScannerTest, stringLiterals)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::string_literal, "\"\""},
    {Category::string_literal, "\"foo\""},
    {Category::wide_string_literal, "L\"bar\""},
    {Category::utf16_string_literal, "u\"baz\""},
    {Category::utf32_string_literal, "U\"fizz\""},
    {Category::utf8_string_literal, "u8\"buzz\""},
    {Category::string_literal, "\"\\\"\""},
    {Category::string_literal, "\"'\""},
  };

  check_lex(R"("" "foo" L"bar" u"baz" U"fizz" u8"buzz" "\"" "'")"
            "\n",
            expected_toks);
}

TEST_F(ScannerTest, punctuators)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::l_bracket, "["},
    {Category::r_bracket, "]"},
    {Category::l_paren, "("},
    {Category::r_paren, ")"},
    {Category::l_brace, "{"},
    {Category::r_brace, "}"},
    {Category::period, "."},
    {Category::arrow, "->"},
    {Category::plusplus, "++"},
    {Category::minusminus, "--"},
    {Category::ampersand, "&"},
    {Category::star, "*"},
    {Category::plus, "+"},
    {Category::minus, "-"},
    {Category::tilde, "~"},
    {Category::exclama, "!"},
    {Category::slash, "/"},
    {Category::percent, "%"},
    {Category::lessless, "<<"},
    {Category::greatergreater, ">>"},
    {Category::less, "<"},
    {Category::greater, ">"},
    {Category::lessequal, "<="},
    {Category::greaterequal, ">="},
    {Category::question, "?"},
    {Category::colon, ":"},
    {Category::semi, ";"},
    {Category::ellipsis, "..."},
    {Category::equal, "="},
    {Category::starequal, "*="},
    {Category::slashequal, "/="},
    {Category::percentequal, "%="},
    {Category::plusequal, "+="},
    {Category::minusequal, "-="},
    {Category::lesslessequal, "<<="},
    {Category::comma, ","},
    {Category::hash, "#"},
    {Category::hashhash, "##"},
    {Category::l_bracket, "<:"},
    {Category::r_bracket, ":>"},
    {Category::l_brace, "<%"},
    {Category::r_brace, "%>"},
    {Category::hash, "%:"},
    {Category::hashhash, "%:%:"},
    {Category::equalequal, "=="},
    {Category::greatergreaterequal, ">>="},
    {Category::exclamaequal, "!="},
    {Category::ampequal, "&="},
    {Category::caret, "^"},
    {Category::pipe, "|"},
    {Category::caretequal, "^="},
    {Category::ampamp, "&&"},
    {Category::pipepipe, "||"},
    {Category::pipeequal, "|="},
  };

  check_lex(
    "[ ] ( ) { } . -> ++ -- & * + - ~ ! / % << >> < > <= >= ? : ; ... = *= /= "
    "%= += -= <<= , # ## <: :> <% %> %: %:%: == >>= != &= ^ | ^= && || |=\n",
    expected_toks);
}

TEST_F(ScannerTest, trigraphs)
{
  std::vector<std::pair<Category, std::string>> expected_toks{
    {Category::hash, "#"},      {Category::l_bracket, "["},
    {Category::r_bracket, "]"}, {Category::caret, "^"},
    {Category::l_brace, "{"},   {Category::r_brace, "}"},
    {Category::pipe, "|"},      {Category::tilde, "~"},
  };

  // ??/ becomes \, which escapes the last but one new line.
  check_lex("\?\?= \?\?( \?\?) \?\?' \?\?< \?\?> \?\?! \?\?- \?\?/\n\n",
            expected_toks);
}

} // namespace
