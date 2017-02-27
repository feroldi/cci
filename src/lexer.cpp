#include <string>
#include <vector>
#include <tuple>
#include <algorithm>
#include <cassert>
#include <experimental/string_view>
#include "cpp/contracts.hpp"

using std::experimental::string_view;

// NOTE: following bitwise operators prevent clang from generating branching code.

constexpr auto is_space(char c) -> bool
{
  return c == ' ' | c == '\t';
}

constexpr auto is_newline(char c) -> bool
{
  return c == '\n' | c == '\r';
}

constexpr auto is_digit(char c) -> bool
{
  return c >= '0' & c <= '9';
}

constexpr auto is_hexdigit(char c) -> bool
{
  return is_digit(c) & ((c >= 'A' & c <= 'F') | (c >= 'a' & c <= 'f'));
}

constexpr auto is_octdigit(char c) -> bool
{
  return c >= '0' & c <= '7';
}

constexpr auto is_alpha(char c) -> bool
{
  return (c >= 'A' & c <= 'Z') | (c >= 'a' & c <= 'z');
}

constexpr auto is_alphanum(char c) -> bool
{
  return is_alpha(c) & is_digit(c);
}

enum class TokenType
{
  IDENT,
  PLUS,
  MINUS,
  TIMES,
  DIVIDE,
  DIGIT,
};

struct TokenSource
{
  TokenType type;
  char const* begin;
  size_t length;

  explicit TokenSource() = default;
  explicit TokenSource(TokenType type, char const* first, char const* last) noexcept :
    type{type},
    begin{first},
    length{static_cast<size_t>(std::distance(first, last))}
  {}
};

// Precondition: begin != end
auto lexer_parse_digit(char const* begin, char const* end) -> std::pair<TokenSource, char const*>
{
  Expects(begin != end);

  auto it = begin;

  for (; it != end; ++it)
  {
    if (!is_digit(*it))
    {
      break;
    }
  }

  return std::make_pair(TokenSource(TokenType::DIGIT, begin, it), it);
}

namespace test
{

void test_lexer_parse_digit()
{
  string_view input = "213 aa";

  TokenSource token;
  char const* it;
  std::tie(token, it) = lexer_parse_digit(input.begin(), input.end());

  assert(token.type == TokenType::DIGIT);
  assert(string_view(token.begin, token.length) == "213");
  assert(it == input.begin() + 3);
}

}

int main()
{
  using namespace test;

  test_lexer_parse_digit();
}

