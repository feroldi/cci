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

const auto TOKEN_SYMBOLS = std::vector<std::pair<TokenType, string_view>>
{
  // Operators
  {TokenType::Plus,             "+"},
  {TokenType::Minus,            "-"},
  {TokenType::Times,            "*"},
  {TokenType::Divide,           "/"},
  {TokenType::Assign,           "="},

  // Matches
  {TokenType::LeftParen,        "("},
  {TokenType::RightParen,       ")"},
  {TokenType::LeftBraces,       "["},
  {TokenType::RightBraces,      "]"},
  {TokenType::LeftCurlyBraces,  "{"},
  {TokenType::RightCurlyBraces, "}"},
  {TokenType::StringMark,       "\""},
  {TokenType::CharMark,         "'"},
  {TokenType::Comma,            ","},
  {TokenType::Colon,            ":"},
  {TokenType::Semicolon,        ";"},
};

const auto TOKEN_RESERVED_NAMES = std::vector<std::pair<TokenType, string_view>>
{
  {TokenType::If,        "if"},
  {TokenType::Else,      "else"},
  {TokenType::For,       "for"},
  {TokenType::While,     "while"},
  {TokenType::Do,        "do"},
  {TokenType::IntType,   "int"},
  {TokenType::FloatType, "float"},
};

// Whites are used to tell apart from different tokens.
constexpr auto is_white(char c) -> bool
{
  return c == ' '  ||
         c == '\t' ||
         c == '('  ||
         c == ')'  ||
         c == '{'  ||
         c == '}'  ||
         c == ','  ||
         c == ':'  ||
         c == '?'  ||
         c == ';';
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

auto find_token_between_whites(const char* begin, const char* end) -> optional<string_view>
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

  auto token = find_token_between_whites(begin, end).value();

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

  auto token = find_token_between_whites(begin, end).value();

  if (is_identifier(token.begin(), token.end()))
  {
    bool is_reserved = false;

    for (const auto& name : TOKEN_RESERVED_NAMES) //< vector<pair<TokenType, string_view>>
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

