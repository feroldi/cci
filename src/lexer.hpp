#pragma once

#include <cstddef>
#include <vector>
#include "cpp/string_view.hpp"

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
  QuestionMark,

  // Constants
  CharConstant,
  IntegerConstant,
  FloatConstant,
  StringConstant,

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

using LexerIterator = const char*;

struct SourceLocation
{
  LexerIterator begin;
  LexerIterator end;

  explicit SourceLocation() = default;
  explicit SourceLocation(LexerIterator begin, LexerIterator end) noexcept :
    begin(begin), end(end)
  {}

  operator string_view() const
  {
    return string_view{begin, static_cast<size_t>(std::distance(begin, end))};
  }
};

struct LineCol
{
  size_t lineno;
  size_t colno;
};

struct TextStream
{
  std::string filename;
  std::string text;
  std::vector<SourceLocation> line_offsets;

  explicit TextStream(string_view filename);

  void calculate_line_offsets();
  auto linecol_from_source_location(const SourceLocation&) const -> LineCol;
};

struct TokenData
{
  TokenType type;
  SourceLocation data;

  explicit TokenData(TokenType type, SourceLocation source) noexcept :
    type{type},
    data{source}
  {}
};

auto lexer_tokenize_text(string_view text) -> std::vector<TokenData>;


