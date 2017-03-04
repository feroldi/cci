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
  return (c >= 'A' & c <= 'Z') || (c >= 'a' & c <= 'z');
}

constexpr auto is_alphanum(char c) -> bool
{
  return is_alpha(c) || is_digit(c);
}

auto is_token(const char* begin, const char* end, string_view tok) -> bool
{
  return string_view(begin, static_cast<size_t>(std::distance(begin, end))) == tok;
}

// TODO it deserves an elaborated code.
struct SourceLocation
{
  const char* begin;
  const char* end;

  constexpr explicit SourceLocation(const char* begin, const char* end) noexcept :
    begin(begin), end(end)
  {}
};

auto find_token_between_whites(const char* begin, const char* end) -> optional<SourceLocation>
{
  auto first = std::find_if_not(begin, end, is_white);
  auto last = std::find_if(first, end, is_white);

  if (first != end)
  {
    return SourceLocation(first, last);
  }

  return nullopt;
}

enum class TokenType
{
  Identifier,
  Plus,
  Minus,
  Times,
  Divide,
  Integer,
};

struct TokenData
{
  TokenType type;
  string_view data;

  constexpr explicit TokenData(TokenType type, const char* begin, const char* end) noexcept :
    type(type),
    data(begin, static_cast<size_t>(end - begin))
  {}
};

struct LexerContext
{
  std::vector<TokenData> tokens;

  void add_token(TokenType type, const char* begin, const char* end)
  {
    tokens.emplace_back(type, begin, end);
  }

  void add_token(TokenType type, const SourceLocation& local)
  {
    tokens.emplace_back(type, local.begin, local.end);
  }

  // TODO
  template <typename... FormatArgs>
  void error(const SourceLocation& local, const char* msg, FormatArgs&&... args)
  {
    Unreachable();
  }

  // TODO
  template <typename... FormatArgs>
  void error(const char* local, const char* msg, FormatArgs&&... args)
  {
    fmt::print(stderr, msg, std::forward<FormatArgs>(args)...);
  }
};

auto lexer_parse_integer(LexerContext& lexer, const char* begin, const char* end) -> const char*
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

      return false;
    }

    return true;
  };

  auto is_float = [] (const char* it, const char* end) -> bool
  {
    // TODO implement is_float.
    Unreachable();
  };

  auto token = find_token_between_whites(begin, end).value();

  if (is_integer(token.begin, token.end))
  {
    lexer.add_token(TokenType::Integer, token);
  }
  else
  {
    // temporary test.
    lexer.error(token.begin, "invalid token: {}\n", std::string(token.begin, size_t(token.end - token.begin)));
  }

  return token.end;
}

namespace test
{

void test_lexer_parse_digit()
{
  string_view input = "42 314()";
  LexerContext lexer{};

  auto it1 = lexer_parse_integer(lexer, input.begin(), input.end());
  auto it2 = lexer_parse_integer(lexer, it1, input.end());

  auto token1 = lexer.tokens[0];
  auto token2 = lexer.tokens[1];

  assert(token1.type == TokenType::Integer);
  assert(token1.data == "42");

  assert(token2.type == TokenType::Integer);
  assert(token2.data == "314");
}

}

int main()
{
  using namespace test;

  test_lexer_parse_digit();
}

