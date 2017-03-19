#include <string>
#include <tuple>
#include <algorithm>
#include <cassert>
#include <fmt/format.h>
#include <fmt/format.cc>
#include "cpp/contracts.hpp"
#include "cpp/optional.hpp"
#include "lexer.hpp"

namespace
{

constexpr const std::pair<TokenType, string_view> TOKEN_SYMBOLS[] = 
{
  // Operators
  {TokenType::Increment,        "++"},
  {TokenType::Decrement,        "--"},
  {TokenType::RightArrow,       "->"},
  {TokenType::Assign,           "="},
  {TokenType::Plus,             "+"},
  {TokenType::Minus,            "-"},
  {TokenType::Times,            "*"},
  {TokenType::Divide,           "/"},
  {TokenType::Percent,          "%"},
  {TokenType::PlusAssign,       "+="},
  {TokenType::MinusAssign,      "-="},
  {TokenType::TimesAssign,      "*="},
  {TokenType::DivideAssign,     "/="},
  {TokenType::ModuloAssign,     "%="},
  {TokenType::GreaterThan,      ">"},
  {TokenType::LessThan,         "<"},
  {TokenType::GreaterEqual,     ">="},
  {TokenType::LessEqual,        "<="},
  {TokenType::EqualsTo,         "=="},
  {TokenType::NotEqualTo,       "!="},
  {TokenType::LogicalNot,       "!"},
  {TokenType::LogicalAnd,       "&&"},
  {TokenType::LogicalOr,        "||"},

  // Bitwise operators
  {TokenType::BitwiseNot,              "~"},
  {TokenType::BitwiseAnd,              "&"},
  {TokenType::BitwiseOr,               "|"},
  {TokenType::BitwiseXor,              "^"},
  {TokenType::BitwiseAndAssign,        "&="},
  {TokenType::BitwiseOrAssign,         "|="},
  {TokenType::BitwiseXorAssign,        "^="},
  {TokenType::BitwiseRightShift,       ">>"},
  {TokenType::BitwiseLeftShift,        "<<"},
  {TokenType::BitwiseRightShiftAssign, ">>="},
  {TokenType::BitwiseLeftShiftAssign,  "<<="},

  // Matches
  {TokenType::LeftParen,        "("},
  {TokenType::RightParen,       ")"},
  {TokenType::LeftBraces,       "["},
  {TokenType::RightBraces,      "]"},
  {TokenType::LeftCurlyBraces,  "{"},
  {TokenType::RightCurlyBraces, "}"},
  {TokenType::StringMark,       "\""},
  {TokenType::CharMark,         "'"},
  {TokenType::Dot,              "."},
  {TokenType::Comma,            ","},
  {TokenType::Colon,            ":"},
  {TokenType::Semicolon,        ";"},
  {TokenType::QuestionMark,     "?"},
};

constexpr const std::pair<TokenType, string_view> TOKEN_RESERVED_NAMES[] =
{
  // Common keywords.
  {TokenType::If,             "if"},
  {TokenType::Else,           "else"},
  {TokenType::For,            "for"},
  {TokenType::While,          "while"},
  {TokenType::Do,             "do"},
  {TokenType::Typedef,        "typedef"},
  {TokenType::Break,          "break"},
  {TokenType::Case,           "case"},
  {TokenType::Continue,       "continue"},
  {TokenType::Default,        "default"},
  {TokenType::Enum,           "enum"},
  {TokenType::Extern,         "extern"},
  {TokenType::Goto,           "goto"},
  {TokenType::Inline,         "inline"},
  {TokenType::Register,       "register"},
  {TokenType::Restrict,       "restrict"},
  {TokenType::Return,         "return"},
  {TokenType::Sizeof,         "sizeof"},
  {TokenType::Static,         "static"},
  {TokenType::Auto,           "auto"},
  {TokenType::Struct,         "struct"},
  {TokenType::Switch,         "switch"},
  {TokenType::Union,          "union"},

  // Types.
  {TokenType::CharType,       "char"},
  {TokenType::ShortType,      "short"},
  {TokenType::IntType,        "int"},
  {TokenType::LongType,       "long"},
  {TokenType::FloatType,      "float"},
  {TokenType::DoubleType,     "double"},
  {TokenType::VoidType,       "void"},
  {TokenType::Signed,         "signed"},
  {TokenType::Unsigned,       "unsigned"},
  {TokenType::Volatile,       "volatile"},
  {TokenType::Const,          "const"},
};

constexpr auto is_special(char c) -> bool
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
         c == '"'  ||
         c == '\'' ||
         c == '.'  ||
         c == ','  ||
         c == ':'  ||
         c == ';'  ||
         c == '?';
}

// Whites are used to tell apart from different tokens.
constexpr auto is_white(char c) -> bool
{
  return c == ' '  ||
         c == '\t' ||
         is_special(c);
}

constexpr auto is_space(char c) -> bool
{
  return c == ' ' || c == '\t';
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

auto is_token(const char* begin, const char* end, string_view tok) -> bool
{
  return string_view(begin, static_cast<size_t>(std::distance(begin, end))) == tok;
}

auto find_next_token(const char* begin, const char* end) -> optional<string_view>
{
  auto first = std::find_if_not(begin, end, is_white);
  auto last = std::find_if(first, end, is_white);

  if (first != end)
  {
    return string_view(first, static_cast<size_t>(std::distance(first, last)));
  }

  return nullopt;
}

struct LexerContext
{
  std::vector<TokenData> tokens;

  void add_token(TokenType type, const char* begin, const char* end)
  {
    tokens.emplace_back(type, begin, end);
  }

  void add_token(TokenType type, string_view source)
  {
    tokens.emplace_back(type, source);
  }

  // TODO
  template <typename... FormatArgs>
  void error(string_view local, const char* msg, FormatArgs&&... args)
  {
    Unreachable();
  }

  // TODO
  template <typename... FormatArgs>
  void error(const char* local, const char* msg, FormatArgs&&... args)
  {
    Unreachable();
  }
};

// TODO parse escape characters.
auto lexer_parse_char_literal(LexerContext& lexer, const char* begin, const char* end) -> const char*
{
  if (begin == end)
  {
    return end;
  }

  const string_view token = [&]
  {
    auto first = std::find_if(it, end, [] (char c) { return c == '\''; });
    auto last = std::find_if(std::next(first), end, [] (char c) { return c == '\''; });

    assert(first != last);

    return string_view(first, static_cast<size_t>(std::distance(first, last)));
  }();

  if (token.end() == end)
  {
    // TODO signal error /missing terminating ' character.
    return std::find_if(token.begin(), end, is_white);
  }

  if (token.size() == 2)
  {
    // TODO signal error /char literal is empty.
    return token.end();
  }

  const size_t char_size_in_bytes = [&]
  {
    size_t count{};

    for (const auto& c : token)
    {
      if (c == '\'' || c == '\\')
      {
        continue;
      }

      ++count;
    }

    return count;
  }();

  // FIXME generate warnings only when pedantic errors are enabled.
  if (char_size_in_bytes > 1)
  {
    // TODO signal warning /multibyte character literal.
    assert(true);
  }

  lexer.add_token(TokenType::CharConstant, token);

  return token.end();
}

// TODO parse escape characters.
auto lexer_parse_string_literal(LexerContext& lexer, const char* begin, const char* end) -> const char*
{
  if (begin == end)
  {
    return end;
  }

  const string_view token = [&]
  {
    auto first = std::find_if(it, end, [] (char c) { return c == '"'; });
    auto last = std::find_if(std::next(first), end, [] (char c) { return c == '"'; });

    assert(first != last);

    return string_view(first, static_cast<size_t>(std::distance(first, last)));
  }();

  if (token.end() == end)
  {
    // TODO signal error /missing terminating " string literal.
    return std::find_if(token.begin(), end, is_white);
  }

  if (token.size() == 2)
  {
    // TODO signal error /string literal is empty.
    return token.end();
  }

  lexer.add_token(TokenType::StringConstant, token);

  return token.end();
}

auto lexer_parse_constant(LexerContext& lexer, const char* begin, const char* end) -> const char*
{
  if (begin == end)
  {
    return end;
  }

  auto is_integer = [] (const char* it, const char* end) -> bool
  {
    const bool has_hex_prefix = std::distance(it, end) > 2 && it[0] == '0' && (it[1] == 'x' || it[1] == 'X');
    
    // skip 0x
    if (has_hex_prefix)
    {
      it = std::next(it, 2);
    }

    for (; it != end; ++it)
    {
      if (has_hex_prefix? is_hexdigit(*it) : is_digit(*it))
      {
        continue;
      }
      
      // TODO signal error message.

      return false;
    }

    return true;
  };

  auto is_float = [] (const char* it, const char* end) -> bool
  {
    const auto dot_it = std::find_if(it, end, [] (char c) { return c == '.'; });
    const auto f_suffix_it = std::find_if(dot_it, end, [] (char c) { return c == 'f' || c == 'F'; });

    if (f_suffix_it != end && std::next(f_suffix_it) != end)
    {
      return false;
    }

    for (; it != end; ++it)
    {
      if (it == dot_it || it == f_suffix_it)
      {
        continue;
      }

      if (is_digit(*it))
      {
        continue;
      }

      // TODO signal error message.

      return false;
    }

    return true;
  };

  auto token = find_next_token(begin, end).value();

  if (token[0] == '\'')
  {
    return lexer_parse_char_literal(lexer, begin, end);
  }

  if (token[0] == '"')
  {
    return lexer_parse_string_literal(lexer, begin, end);
  }

  if (is_integer(token.begin(), token.end()))
  {
    lexer.add_token(TokenType::IntegerConstant, token);
  }
  else if (is_float(token.begin(), token.end()))
  {
    lexer.add_token(TokenType::FloatConstant, token);
  }

  return token.end();
}

auto lexer_parse_identifier(LexerContext& lexer, const char* begin, const char* end) -> const char*
{
  if (begin == end)
  {
    return end;
  }

  auto is_identifier = [] (const char* it, const char* end) -> bool
  {
    if (!is_alpha(it[0]))
    {
      // TODO signal message error
      return false;
    }

    for (it = std::next(it); it != end; ++it)
    {
      if (is_alphanum(*it))
      {
        continue;
      }

      // TODO signal error message.
      return false;
    }

    return true;
  };

  auto token = find_next_token(begin, end).value();

  if (is_identifier(token.begin(), token.end()))
  {
    bool is_reserved = false;

    for (const auto& name : TOKEN_RESERVED_NAMES) //< pair<TokenType, string_view>[]
    {
      if (name.second == token)
      {
        lexer.add_token(name.first, token);
        is_reserved = true;
      }
    }

    if (!is_reserved)
    {
      lexer.add_token(TokenType::Identifier, token);
    }
  }

  return token.end();
}


} // namespace

auto lexer_parse(const std::string& data) -> std::vector<TokenData>
{
  return {};
}

