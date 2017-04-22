#include <string>
#include <tuple>
#include <algorithm>
#include <functional>
#include <cassert>
#include "cpp/contracts.hpp"
#include "cpp/optional.hpp"
#include "lexer.hpp"

namespace
{

static const std::pair<TokenType, string_view> TOKEN_SYMBOLS[] = 
{
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

static const std::pair<TokenType, string_view> TOKEN_RESERVED_NAMES[] =
{
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
  return c == '='  ||
         c == '+'  ||
         c == '-'  ||
         c == '*'  ||
         c == '/'  ||
         c == '%'  ||
         c == '>'  ||
         c == '<'  ||
         c == '!'  ||
         c == '&'  ||
         c == '|'  ||
         c == '~'  ||
         c == '^'  ||
         c == '('  ||
         c == ')'  ||
         c == '['  ||
         c == ']'  ||
         c == '{'  ||
         c == '}'  ||
         c == '.'  ||
         c == ','  ||
         c == ':'  ||
         c == ';'  ||
         c == '?';
}

constexpr auto is_special(char c) -> bool
{
  return is_operator(c)  || c == '"'  || c == '\'';
}

constexpr auto is_space(char c) -> bool
{
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

// Whites are used to tell apart from different tokens.
constexpr auto is_white(char c) -> bool
{
  return is_space(c) || is_special(c);
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

auto is_token(SourceLocation source, string_view tok) -> bool
{
  return string_view(source) == tok;
}

auto is_token(LexerIterator begin, LexerIterator end, string_view tok) -> bool
{
  return is_token(SourceLocation{begin, end}, tok);
}

struct LexerContext
{
  std::vector<TokenData> tokens;

  void add_token(TokenType type, LexerIterator begin, LexerIterator end)
  {
    tokens.emplace_back(type, SourceLocation{begin, end});
  }

  void add_token(TokenType type, SourceLocation source)
  {
    tokens.emplace_back(type, source);
  }
};

// TODO parse escape characters.
auto lexer_parse_char_literal(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_char_literal_match(begin[0]));

  const LexerIterator match_it = std::invoke([&] {
    for (auto it = std::next(begin); it != end; ++it)
    {
      // skip escaping \'
      if (*it == '\\' && std::next(it) != end && is_char_literal_match(*std::next(it)))
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

  // TODO emit error message missing matching char symbol.
  assert(match_it != end);

  const auto token = SourceLocation{begin, match_it};

  // TODO count bytes in a char literal.
  const size_t byte_count = 1;

  // TODO generate warning only when pedantic errors are enabled.
  if (byte_count > 1)
  {
    // TODO report warning /multibyte character literal.
    assert(false);
  }

  lexer.add_token(TokenType::CharConstant, token);

  return token.end;
}

// TODO parse escape characters.
auto lexer_parse_string_literal(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_string_literal_match(begin[0]));

  const LexerIterator match_it = std::invoke([&] {
    for (auto it = std::next(begin); it != end; ++it)
    {
      // skip escaping \"
      if (*it == '\\' && std::next(it) != end && is_string_literal_match(*std::next(it)))
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

  // TODO emit error message missing matching string literal symbol.
  assert(match_it != end);

  const auto token = SourceLocation{begin, match_it};
  lexer.add_token(TokenType::StringConstant, token);

  return token.end;
}

auto lexer_parse_operator(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_operator(begin[0]));

  auto it = std::find_if_not(begin, end, is_operator);

  if (it == end)
  {
    // TODO emit error.
    return end;
  }

  auto token = SourceLocation{begin, it};
  auto best_match = optional<std::pair<TokenType, string_view>>{};

  for (const auto& [type, str] : TOKEN_SYMBOLS)
  {
    if (string_view(token).substr(0, str.size()) == str)
    {
      if (!best_match.has_value() || best_match->second.size() < str.size())
      {
        best_match = std::make_pair(type, str);
      }
    }
  }

  if (best_match.has_value())
  {
    auto [type, str] = *best_match;
    token.end = token.begin + str.size();
    lexer.add_token(type, token);
  }

  return token.end;
}

auto lexer_parse_integer(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_digit(begin[0]));

  const bool has_hex_prefix = std::distance(begin, end) > 2 && begin[0] == '0' && (begin[1] == 'x' || begin[1] == 'X');
  const auto it = has_hex_prefix ?
    std::find_if_not(std::next(begin, 2), end, is_hexdigit) :
    std::find_if_not(begin, end, is_digit);

  if (it == end)
  {
    return end;
  }

  if (is_alpha(*it))
  {
    // TODO emit error message invalid digit.
    assert(false);
  }

  const auto token = SourceLocation{begin, it};
  lexer.add_token(TokenType::IntegerConstant, token);

  return token.end;
}

auto lexer_parse_decimal(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_digit(begin[0]));

  auto it = std::find_if_not(begin, end, is_digit);

  if (is_alpha(*it))
  {
    // TODO emit error message invalid digit.
    assert(false);
  }

  if (*it == '.')
  {
    it = std::find_if_not(std::next(it), end, is_digit);
  }

  if (is_alpha(*it))
  {
    if (*it != 'f' && *it != 'F')
    {
      // TODO emit error message invalid digit.
      assert(false);
    }

    // skip suffix so we get the token's end.
    ++it;

    if (it != end && is_alpha(*it))
    {
      // TODO emit error message invalid digit.
      assert(false);
    }
  }

  const auto token = SourceLocation{begin, it};
  lexer.add_token(TokenType::FloatConstant, token);

  return token.end;
}

auto lexer_parse_constant(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_char_literal_match(*begin) || is_string_literal_match(*begin) || is_digit(*begin));

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

  Unreachable();

  return end;
}

auto lexer_parse_identifier(LexerContext& lexer, LexerIterator begin, LexerIterator end) -> LexerIterator
{
  Expects(is_alpha(begin[0]));

  const auto it = std::find_if_not(std::next(begin), end, is_alphanum);
  const auto token = SourceLocation{begin, it};

  for (const auto& [type, token_str] : TOKEN_RESERVED_NAMES) //< pair<TokenType, string_view>[]
  {
    if (token == token_str)
    {
      lexer.add_token(type, token);
      return token.end;
    }
  }

  lexer.add_token(TokenType::Identifier, token);

  return token.end;
}

} // namespace

auto lexer_tokenize_text(string_view text) -> std::vector<TokenData>
{
  LexerContext context{};
  const auto end = text.end();
  auto it = std::find_if_not(text.begin(), end, is_space);

  while (it != end)
  {
    it = std::find_if_not(it, end, is_space);

    if (is_char_literal_match(*it) || is_string_literal_match(*it) || is_digit(*it))
    {
      it = lexer_parse_constant(context, it, end);
    }
    else if (is_alpha(*it))
    {
      it = lexer_parse_identifier(context, it, end);
    }
    else if (is_operator(*it))
    {
      it = lexer_parse_operator(context, it, end);
    }
    else
    {
      break;
    }
  }

  return context.tokens;
}

