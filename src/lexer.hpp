#pragma once

#include <cstddef>
#include <vector>
#include <experimental/string_view>

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
  Comma,
  Colon,
  Semicolon,

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

auto lexer_parse(const std::string& data) -> std::vector<TokenData>;


