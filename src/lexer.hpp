#pragma once

#include <cstddef>
#include <vector>
#include "cpp/string_view.hpp"
#include "cpp/contracts.hpp"

struct ProgramContext;

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
  OctIntegerConstant,
  HexIntegerConstant,
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
  using value_type = char;
  using const_iterator = LexerIterator;
  using iterator = const_iterator;
  using size_type = std::size_t;

private:
  const_iterator first;
  const_iterator last;

public:
  explicit SourceLocation() = default;

  explicit SourceLocation(const_iterator begin, const_iterator end) :
    first(begin), last(end)
  {
    Expects(begin <= end);
  }

  explicit SourceLocation(const_iterator it) noexcept :
    first(it), last(it + 1)
  {}

  constexpr auto begin() const noexcept -> const_iterator
  {
    return this->first;
  }

  constexpr auto end() const noexcept -> const_iterator
  {
    return this->last;
  }

  constexpr auto size() const noexcept -> size_type
  {
    return static_cast<size_t>(this->end() - this->begin());
  }

  constexpr auto operator[] (size_t index) const noexcept -> value_type
  {
    return *(this->begin() + index);
  }

  constexpr auto is_sub_of(const SourceLocation& sl) const -> bool
  {
    return this->begin() >= sl.begin() && this->end() <= sl.end();
  }

  operator string_view() const
  {
    return string_view{this->begin(), this->size()};
  }
};

struct TextStream
{
  struct LineColumn
  {
    size_t line_no;
    size_t column_no;
  };

  std::string filename;
  std::string data;
  std::vector<SourceLocation> line_offsets;

  explicit TextStream(string_view filename);

  TextStream(const TextStream&) = default;
  TextStream(TextStream&&) noexcept = default;

  auto linecol_from_source(const SourceLocation&) const -> LineColumn;
  auto get_line(const SourceLocation&) const -> SourceLocation;
  auto get_line(size_t line_no) const -> SourceLocation;
};

struct TokenInfo
{
  const TextStream& stream;
  SourceLocation source;

  explicit TokenInfo(const TextStream& stream, const SourceLocation& source) :
    stream(stream), source(source)
  {}
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

// TODO move this to TokenStream
auto lexer_tokenize_text(ProgramContext&, const TextStream&, string_view text) -> std::vector<TokenData>;

