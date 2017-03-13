#pragma once

#include <cstddef>
#include <vector>
#include <experimental/string_view>

using std::experimental::string_view;

enum class TokenType
{
  // Operators
  Increment,
  Decrement,
  RightArrow,
  Assign,
  Plus,
  Minus,
  Times,
  Divide,
  Percent,
  GreaterThan,
  LessThan,
  GreaterEqual,
  LessEqual,
  EqualsTo,
  NotEqualTo,
  PlusAssign,
  MinusAssign,
  TimesAssign,
  DivideAssign,
  ModuloAssign,

  // Logical operators
  LogicalNot,
  LogicalAnd,
  LogicalOr,

  // Bitwise operators
  BitwiseNot,
  BitwiseAnd,
  BitwiseOr,
  BitwiseXor,
  BitwiseAndAssign,
  BitwiseOrAssign,
  BitwiseXorAssign,
  BitwiseRightShift,
  BitwiseLeftShift,
  BitwiseRightShiftAssign,
  BitwiseLeftShiftAssign,

  // Matches
  LeftParen,
  RightParen,
  LeftBraces,
  RightBraces,
  LeftCurlyBraces,
  RightCurlyBraces,
  StringMark,
  CharMark,

  // Symbols
  Dot,
  Comma,
  Colon,
  Semicolon,
  Backslash,
  QuestionMark,

  // Constants
  IntegerConstant,
  FloatConstant,
  CharConstant,

  // Qualified ids
  Identifier,

  // Reserved identifiers
  If,
  Else,
  For,
  While,
  Do,
  Typedef,
  Break,
  Case,
  Continue,
  Default,
  Enum,
  Extern,
  Goto,
  Inline,
  Register,
  Restrict,
  Return,
  Sizeof,
  Static,
  Auto,
  Struct,
  Switch,
  Union,
  // Alignas,
  // Alignof,
  // Atomic,
  // Bool,
  // Complex,
  // Generic,
  // Imaginary,
  // Noreturn,
  // StaticAssert,
  // ThreadLocal,

  // Types
  CharType,
  ShortType,
  IntType,
  LongType,
  FloatType,
  DoubleType,
  VoidType,
  Signed,
  Unsigned,
  Volatile,
  Const,
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


