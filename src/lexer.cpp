#include <string>
#include <vector>
#include <tuple>
#include <algorithm>
#include <cassert>
#include <experimental/string_view>
#include <fmt/format.h>
#include <fmt/format.cc>
#include "cpp/contracts.hpp"
#include "cpp/optional.hpp"

using std::experimental::string_view;

enum class TokenType
{
  // Symbols
  Plus,
  Minus,
  Times,
  Divide,
  Assign,
  LeftParen,
  RightParen,
  LeftBraces,
  RightBraces,
  LeftCurlyBraces,
  RightCurlyBraces,
  StringMark,
  CharMark,

  // Constants
  IntegerConstant,
  FloatConstant,

  // Qualified ids
  Identifier,

  // Reserved names
  If,
  Else,
  For,
  While,
  Do,

  // Types
  IntType,
  FloatType,
};

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
         c == '?';
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


struct TokenData
{
  TokenType type;
  string_view data;

  constexpr explicit TokenData(TokenType type, const char* begin, const char* end) noexcept :
    type(type),
    data(begin, static_cast<size_t>(end - begin))
  {}

  constexpr explicit TokenData(TokenType type, string_view source) noexcept :
    type(type),
    data(source.begin(), static_cast<size_t>(source.end() - source.begin()))
  {}
};

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

namespace test
{

struct LexerState
{
  string_view input;
  LexerContext context;
  const char* it;

  explicit LexerState(string_view input) noexcept :
    input{input}, context{}, it{input.begin()}
  {}

  template <typename F>
  auto operator() (F&& f) noexcept -> const TokenData&
  {
    it = std::forward<F>(f)(context, it, input.end());
    return context.tokens.back();
  }
};

void test_lexer_parse_constant()
{
  fmt::print(stderr, "Testing lexer_parse_constant ... ");

  // IntegerConstants.
  {
    LexerState lexer{"42 0x22fx"};

    auto t1 = lexer(lexer_parse_constant);

    assert(t1.type == TokenType::IntegerConstant);
    assert(t1.data == "42");

    // Should fail
    auto t2 = lexer(lexer_parse_constant);

    assert(lexer.context.tokens.size() == 1);
    assert(t2.type == TokenType::IntegerConstant);
    assert(t2.data == "42");
  }

  // FloatConstants.
  {
    LexerState lexer{"123.f 0.2 1. (1.f) 0.ff .0f"};

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "123.f");
    }

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "0.2");
    }

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "1.");
    }

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "1.f");
    }

    {
      // Should fail; nothing gets pushed into context.
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "1.f");

      // Should also skip ill-formed constant.
    }

    {
      auto t= lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == ".0f");
    }
  }

  fmt::print(stderr, "Success.\n");
}

void test_lexer_parse_identifier()
{
  fmt::print(stderr, "Testing lexer_parse_identifier ... ");

  LexerState lexer{"abc42() _KeepMoving_Forward 42fail{}success_1 if"};

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "abc42");
  }

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "_KeepMoving_Forward");
  }

  {
    // Should fail; nothing gets pushed into context.
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "_KeepMoving_Forward");

    // Should also skip ill-formed identifier.
  }

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "success_1");
  }

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::If);
    assert(t.data == "if");
  }

  fmt::print(stderr, "Success.\n");
}

}

int main()
{
  test::test_lexer_parse_constant();
  test::test_lexer_parse_identifier();
}

