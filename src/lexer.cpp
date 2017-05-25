#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <string>
#include <tuple>
#include "lexer.hpp"
#include "cpp/contracts.hpp"
#include "cpp/optional.hpp"
#include "utils/stream.hpp"
#include "source_manager.hpp"
#include "program.hpp"

namespace
{

static const std::pair<TokenType, string_view> TOKEN_SYMBOLS[] = {
  // Operators
  {TokenType::Increment, "++"},
  {TokenType::Decrement, "--"},
  {TokenType::RightArrow, "->"},
  {TokenType::Assign, "="},
  {TokenType::Plus, "+"},
  {TokenType::Minus, "-"},
  {TokenType::Times, "*"},
  {TokenType::Divide, "/"},
  {TokenType::Percent, "%"},
  {TokenType::PlusAssign, "+="},
  {TokenType::MinusAssign, "-="},
  {TokenType::TimesAssign, "*="},
  {TokenType::DivideAssign, "/="},
  {TokenType::ModuloAssign, "%="},
  {TokenType::GreaterThan, ">"},
  {TokenType::LessThan, "<"},
  {TokenType::GreaterEqual, ">="},
  {TokenType::LessEqual, "<="},
  {TokenType::EqualsTo, "=="},
  {TokenType::NotEqualTo, "!="},
  {TokenType::LogicalNot, "!"},
  {TokenType::LogicalAnd, "&&"},
  {TokenType::LogicalOr, "||"},

  // Bitwise operators
  {TokenType::BitwiseNot, "~"},
  {TokenType::BitwiseAnd, "&"},
  {TokenType::BitwiseOr, "|"},
  {TokenType::BitwiseXor, "^"},
  {TokenType::BitwiseAndAssign, "&="},
  {TokenType::BitwiseOrAssign, "|="},
  {TokenType::BitwiseXorAssign, "^="},
  {TokenType::BitwiseRightShift, ">>"},
  {TokenType::BitwiseLeftShift, "<<"},
  {TokenType::BitwiseRightShiftAssign, ">>="},
  {TokenType::BitwiseLeftShiftAssign, "<<="},

  // Matches
  {TokenType::LeftParen, "("},
  {TokenType::RightParen, ")"},
  {TokenType::LeftBraces, "["},
  {TokenType::RightBraces, "]"},
  {TokenType::LeftCurlyBraces, "{"},
  {TokenType::RightCurlyBraces, "}"},
  {TokenType::StringMark, "\""},
  {TokenType::CharMark, "'"},
  {TokenType::Dot, "."},
  {TokenType::Comma, ","},
  {TokenType::Colon, ":"},
  {TokenType::Semicolon, ";"},
  {TokenType::QuestionMark, "?"},
};

static const std::pair<TokenType, string_view> TOKEN_RESERVED_NAMES[] = {
  // Common keywords.
  {TokenType::If, "if"},
  {TokenType::Else, "else"},
  {TokenType::For, "for"},
  {TokenType::While, "while"},
  {TokenType::Do, "do"},
  {TokenType::Typedef, "typedef"},
  {TokenType::Break, "break"},
  {TokenType::Case, "case"},
  {TokenType::Continue, "continue"},
  {TokenType::Default, "default"},
  {TokenType::Enum, "enum"},
  {TokenType::Extern, "extern"},
  {TokenType::Goto, "goto"},
  {TokenType::Inline, "inline"},
  {TokenType::Register, "register"},
  {TokenType::Restrict, "restrict"},
  {TokenType::Return, "return"},
  {TokenType::Sizeof, "sizeof"},
  {TokenType::Static, "static"},
  {TokenType::Auto, "auto"},
  {TokenType::Struct, "struct"},
  {TokenType::Switch, "switch"},
  {TokenType::Union, "union"},

  // Types.
  {TokenType::CharType, "char"},
  {TokenType::ShortType, "short"},
  {TokenType::IntType, "int"},
  {TokenType::LongType, "long"},
  {TokenType::FloatType, "float"},
  {TokenType::DoubleType, "double"},
  {TokenType::VoidType, "void"},
  {TokenType::Signed, "signed"},
  {TokenType::Unsigned, "unsigned"},
  {TokenType::Volatile, "volatile"},
  {TokenType::Const, "const"},
};

constexpr auto is_operator(char c) -> bool
{
  return c == '=' || c == '+' || c == '-' || c == '*' || c == '/' || c == '%' ||
         c == '>' || c == '<' || c == '!' || c == '&' || c == '|' || c == '~' ||
         c == '^' || c == '(' || c == ')' || c == '[' || c == ']' || c == '{' ||
         c == '}' || c == '.' || c == ',' || c == ':' || c == ';' || c == '?';
}

constexpr auto is_newline(char c) -> bool
{
  return c == '\n' || c == '\r';
}

constexpr auto is_space(char c) -> bool
{
  return c == ' ' || c == '\t' || is_newline(c);
}

constexpr auto is_char_literal_match(char c) -> bool
{
  return c == '\'';
}

constexpr auto is_string_literal_match(char c) -> bool
{
  return c == '"';
}

constexpr auto is_digit(char c) -> bool
{
  return c >= '0' & c <= '9';
}

constexpr auto is_hexdigit(char c) -> bool
{
  return is_digit(c) || (c >= 'A' & c <= 'F') || (c >= 'a' & c <= 'f');
}

constexpr auto is_octdigit(char c) -> bool
{
  return c >= '0' & c <= '7';
}

constexpr auto is_alpha(char c) -> bool
{
  return (c >= 'A' & c <= 'Z') || (c >= 'a' & c <= 'z') || c == '_';
}

constexpr auto is_alphanum(char c) -> bool
{
  return is_alpha(c) || is_digit(c);
}

struct LexerContext
{
  ProgramContext& program;
  const SourceManager& source;
  std::vector<TokenStream::TokenData> tokens;

  explicit LexerContext(ProgramContext& p, const SourceManager& source)
    : program(p), source(source), tokens()
  {}

  void add_token(TokenType type, SourceLocation begin, SourceLocation end)
  {
    tokens.emplace_back(type, SourceRange{begin, end});
  }

  void add_token(TokenType type, SourceRange source)
  {
    tokens.emplace_back(type, source);
  }

  template <typename... Args>
  void warning(const SourceRange& range, Args&&... args)
  {
    const auto pos = this->source.linecol_from_location(range.begin());
    program.warn(TokenStream::TokenDebug{this->source, pos, range}, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void error(const SourceRange& range, Args&&... args)
  {
    const auto pos = this->source.linecol_from_location(range.begin());
    program.error(TokenStream::TokenDebug{this->source, pos, range}, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void pedantic(const SourceRange& range, Args&&... args)
  {
    const auto pos = this->source.linecol_from_location(range.begin());
    program.pedantic(TokenStream::TokenDebug{this->source, pos, range}, std::forward<Args>(args)...);
  }
};

// Escape sequence    Hex value in ASCII    Character represented
// \a                 07                    Alert (Beep, Bell) (added in C89)
// \b                 08                    Backspace
// \f                 0C                    Formfeed
// \n                 0A                    Newline (Line Feed); see notes below
// \r                 0D                    Carriage Return
// \t                 09                    Horizontal Tab
// \v                 0B                    Vertical Tab
// \\                 5C                    Backslash
// \'                 27                    Single quotation mark
// \"                 22                    Double quotation mark
// \?                 3F                    Question mark (used to avoid
// trigraphs)
// \nnn note-1        any                   The byte whose numerical value is
// given by nnn interpreted as an octal number
// \xhh...            any                   The byte whose numerical value is
// given by hh... interpreted as a hexadecimal number
// \e note-2 	        1B                    ESC character
// \Uhhhhhhhh note-3 	none                  Unicode code point where h is a
// hexadecimal digit
// \uhhhh note-4      none                  Unicode code point below 10000
// hexadecimal
//
//  Note 1.^ There may be one, two, or three octal numerals n present; see the
//  Notes section below.
//  Note 2.^ Common non-standard code; see the Notes section below.
//  Note 3.^ \U takes 8 hexadecimal digits h; see the Notes section below.
//  Note 4.^ \u takes 4 hexadecimal digits h; see the Notes section below.

// Putting it here first because I'll need it later for the parser (or because
// I'm just lazy right now).
// FIXME This function needs to be moved to parser.cpp
// TODO Convert escape sequences into their respective values. As of now, it's
// just validating them.
auto check_escape_sequences(LexerContext& lexer, SourceLocation begin,
                            SourceLocation end) -> bool
{
  const static char ESC_SEQ[] = {'n', 'r',  't',  'a', 'b', 'f',
                                 'v', '\\', '\'', '"', '?'};
  const static char ESC_SEQ_HEX[] = {'x', 'U', 'u'};
  bool any_errors = false;
  auto it = begin;

  while (it != end)
  {
    it = std::find_if(it, end, [](char c) { return c == '\\'; });

    if (it == end)
      break;

    if (std::next(it) != end)
    {
      // skip '\\'
      std::advance(it, 1);

      if (is_octdigit(*it))
      {
        // There may exist up to three octal digits.
        it = std::find_if_not(it, it + 3, is_octdigit);
      }
      else if (std::any_of(std::begin(ESC_SEQ_HEX), std::end(ESC_SEQ_HEX),
                           [&](char c) { return *it == c; }))
      {
        const auto it_end = std::find_if_not(std::next(it), end, is_hexdigit);
        const auto distance = std::distance(it + 1, it_end);

        switch (*it)
        {
          case 'x':
            if (distance < 2)
            {
              lexer.error(SourceRange(std::prev(it)), "\\x used with no following hex digits");
              any_errors = true;
            }
            break;

          case 'U':
            if (distance < 8)
            {
              lexer.error({std::prev(it), it_end}, "\\U incomplete universal character name");
              any_errors = true;
            }
            break;

          case 'u':
            if (distance < 4)
            {
              lexer.error({std::prev(it), it_end}, "\\u incomplete universal character name");
              any_errors = true;
            }
            break;

          default:
            Unreachable();
        }

        it = it_end;
      }
      else if (std::any_of(std::begin(ESC_SEQ), std::end(ESC_SEQ),
                           [&](char c) { return *it == c; }))
      {
        // skip already parsed escape sequence.
        std::advance(it, 1);
      }
      else
      {
        lexer.warning(SourceRange(it - 1, it + 1), "unkown escape sequence");
        any_errors = true;
      }
    }
    else
    {
      lexer.error(SourceRange(it), "missing terminating '\"' character");
      any_errors = true;
    }
  }

  return !any_errors;
}

auto lexer_parse_char_literal(LexerContext& lexer, SourceLocation begin,
                              SourceLocation end) -> SourceLocation
{
  Expects(is_char_literal_match(begin[0]));

  const SourceLocation it = std::invoke([&] {
    for (auto it = std::next(begin); it != end; ++it)
    {
      // skip escaping \'
      if (*it == '\\' && std::next(it) != end &&
          is_char_literal_match(*std::next(it)))
      {
        ++it;
        continue;
      }

      if (is_char_literal_match(*it))
      {
        return std::next(it);
      }
    }

    return end;
  });

  if (it == end && !is_char_literal_match(*std::prev(it)))
  {
    lexer.error(SourceRange(begin), "missing terminating ' character");
  }

  // TODO count bytes in a char literal.
  const size_t byte_count = 1;

  // TODO generate warning only when pedantic errors are enabled.
  if (byte_count > 1)
  {
    // TODO report warning /multibyte character literal.
    assert(false);
  }

  check_escape_sequences(lexer, std::next(begin), std::prev(it));
  lexer.add_token(TokenType::CharConstant, begin, it);

  return it;
}

// TODO parse escape characters.
auto lexer_parse_string_literal(LexerContext& lexer, SourceLocation begin,
                                SourceLocation end) -> SourceLocation
{
  Expects(is_string_literal_match(begin[0]));

  const SourceLocation it = std::invoke([&] {
    for (auto it = std::next(begin); it != end; ++it)
    {
      // skip escaping \"
      if (*it == '\\' && std::next(it) != end &&
          is_string_literal_match(*std::next(it)))
      {
        ++it;
        continue;
      }

      if (is_string_literal_match(*it))
      {
        return std::next(it);
      }
    }

    return end;
  });

  if (it == end && !is_string_literal_match(*std::prev(it)))
  {
    lexer.error(SourceRange(begin), "missing terminating '\"' character");
  }

  check_escape_sequences(lexer, std::next(begin), std::prev(it));
  lexer.add_token(TokenType::StringConstant, begin, it);

  return it;
}

auto lexer_parse_operator(LexerContext& lexer, SourceLocation begin,
                          SourceLocation end) -> SourceLocation
{
  Expects(is_operator(begin[0]));

  auto it = std::find_if_not(begin, end, is_operator);
  auto source_sv = string_view(SourceRange(begin, it));
  auto best_match = optional<std::pair<TokenType, string_view>>{};

  // Find the longest symbol that matches a token.
  for (const auto & [ type, str ] : TOKEN_SYMBOLS)
  {
    if (source_sv.substr(0, str.size()) == str)
    {
      if (!best_match.has_value() || best_match->second.size() < str.size())
      {
        best_match = std::make_pair(type, str);
      }
    }
  }

  if (best_match.has_value())
  {
    auto[type, str] = *best_match;
    it = begin + str.size();
    lexer.add_token(type, begin, it);
  }

  return it;
}

auto lexer_parse_integer(LexerContext& lexer, SourceLocation begin,
                         SourceLocation end) -> SourceLocation
{
  Expects(is_digit(begin[0]));

  enum class IntegerBase
  {
    Decimal,
    Octal,
    Hexadecimal,
  };

  const auto[base, token_type] = [&]() -> std::pair<IntegerBase, TokenType> {
    if (std::distance(begin, end) > 2 && begin[0] == '0')
    {
      if (begin[1] == 'x' || begin[1] == 'X')
      {
        return {IntegerBase::Hexadecimal, TokenType::HexIntegerConstant};
      }

      return {IntegerBase::Octal, TokenType::OctIntegerConstant};
    }

    return {IntegerBase::Decimal, TokenType::IntegerConstant};
  }();

  const auto it = [&, base=base] {
    switch (base)
    {
      case IntegerBase::Decimal:
        return std::find_if_not(begin, end, is_digit);

      case IntegerBase::Octal:
        return std::find_if_not(std::next(begin), end, is_octdigit);

      case IntegerBase::Hexadecimal:
        return std::find_if_not(std::next(begin, 2), end, is_hexdigit);

      default:
        Unreachable();
    }
  }();

  if (it != end)
  {
    switch (base)
    {
      case IntegerBase::Decimal:
        if (is_alpha(*it))
        {
          lexer.error(SourceRange(it), "invalid digit '{}' in decimal constant", *it);
        }
        break;

      case IntegerBase::Octal:
        if (is_alphanum(*it) || *it == '.')
        {
          lexer.error(SourceRange(it), "invalid digit '{}' in octal constant", *it);
        }
        break;

      case IntegerBase::Hexadecimal:
        if (is_alphanum(*it))
        {
          lexer.error(SourceRange(it), "invalid digit '{}' in hexadecimal constant", *it);
        }
        break;

      default:
        Unreachable();
    }
  }

  lexer.add_token(token_type, begin, it);

  return it;
}

auto lexer_parse_decimal(LexerContext& lexer, SourceLocation begin,
                         SourceLocation end) -> SourceLocation
{
  Expects(is_digit(begin[0]) || begin[0] == '.');

  auto it = std::find_if_not(begin, end, is_digit);

  if (is_alpha(*it))
  {
    lexer.error(SourceRange(it), "invalid digit '{}' on floating constant", *it);
  }

  if (*it == '.')
  {
    it = std::find_if_not(std::next(it), end, is_digit);
  }

  if (is_alpha(*it))
  {
    // skip suffix.
    ++it;

    if (it != end)
    {
      if ((*it != 'f' && *it != 'F') || (it != end && is_alphanum(*it)))
      {
        auto suffix_end = std::find_if_not(it, end, is_alphanum);
        lexer.error({it - 1, suffix_end}, "invalid suffix '{}' on floating constant", std::string(it - 1, suffix_end));
      }
    }
  }

  lexer.add_token(TokenType::FloatConstant, begin, it);

  return it;
}

auto lexer_parse_constant(LexerContext& lexer, SourceLocation begin,
                          SourceLocation end) -> SourceLocation
{
  Expects(is_char_literal_match(*begin) || is_string_literal_match(*begin) ||
          is_digit(*begin) || *begin == '.');

  if (is_char_literal_match(*begin))
  {
    return lexer_parse_char_literal(lexer, begin, end);
  }

  if (is_string_literal_match(*begin))
  {
    return lexer_parse_string_literal(lexer, begin, end);
  }

  if (is_digit(*begin))
  {
    const auto it = std::find_if_not(begin, end, is_digit);

    if (*it == '.')
    {
      return lexer_parse_decimal(lexer, begin, end);
    }
    else
    {
      return lexer_parse_integer(lexer, begin, end);
    }
  }

  if (*begin == '.')
  {
    return lexer_parse_decimal(lexer, begin, end);
  }

  Unreachable();

  return end;
}

auto lexer_parse_identifier(LexerContext& lexer, SourceLocation begin,
                            SourceLocation end) -> SourceLocation
{
  Expects(is_alpha(begin[0]));

  const auto it = std::find_if_not(std::next(begin), end, is_alphanum);
  const auto token = SourceRange{begin, it};

  for (const auto & [ type, token_str ] :
       TOKEN_RESERVED_NAMES) //< pair<TokenType, string_view>[]
  {
    if (token == token_str)
    {
      lexer.add_token(type, token);
      return token.end();
    }
  }

  lexer.add_token(TokenType::Identifier, token);

  return token.end();
}

// Doesn't generate tokens, only returns an iterator past the comment.
// There is a desire to convert them to tokens for documentation parsing though.
auto lexer_parse_comments(LexerContext& lexer, SourceLocation begin,
                          SourceLocation end) -> SourceLocation
{
  Expects(begin[0] == '/' && (begin[1] == '/' || begin[1] == '*'));

  if (begin[1] == '/')
  {
    auto it = std::find_if(std::next(begin, 2), end, is_newline);

    if (it != end)
    {
      return std::next(it);
    }
    else
    {
      lexer.pedantic(SourceRange(it - 1), "no newline at end of file");
    }
  }
  else if (begin[1] == '*')
  {
    for (auto it = std::next(begin, 2); it != end; ++it)
    {
      if (*it == '*' && std::next(it) != end && *std::next(it) == '/')
      {
        return std::next(it, 2);
      }
    }

    // TODO error message missing matching */ block-comment end
    assert(false && "missing matching '*/' block-comment end");
  }

  Unreachable();
}

} // namespace

auto TokenStream::parse(ProgramContext& program, const SourceManager& source) -> TokenStream
{
  auto context = LexerContext(program, source);
  auto range = source.range();
  auto it = std::find_if_not(range.begin(), range.end(), is_space);

  while (it != range.end())
  {
    if (*it == '/' && std::next(it) != range.end() &&
        (*std::next(it) == '/' || *std::next(it) == '*'))
    {
      it = lexer_parse_comments(context, it, range.end());
    }
    else if (is_char_literal_match(*it) || is_string_literal_match(*it) ||
             is_digit(*it) || *it == '.')
    {
      it = lexer_parse_constant(context, it, range.end());
    }
    else if (is_alpha(*it))
    {
      it = lexer_parse_identifier(context, it, range.end());
    }
    else if (is_operator(*it))
    {
      it = lexer_parse_operator(context, it, range.end());
    }
    else
    {
      // TODO emit unknown symbol.
      ++it;
    }

    it = std::find_if_not(it, range.end(), is_space);
  }

  return TokenStream(std::move(context.tokens));
}

