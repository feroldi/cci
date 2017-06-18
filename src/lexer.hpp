#pragma once

#include <cstddef>
#include <vector>
#include "cpp/contracts.hpp"
#include "cpp/string_view.hpp"
#include "source_manager.hpp"

namespace ccompiler
{

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

struct TokenStream
{
  struct TokenData
  {
    TokenType type;
    SourceRange data;

    explicit TokenData(TokenType type, SourceRange source) noexcept
      : type{type}, data{source}
    {}
  };

  struct TokenDebug
  {
    const SourceManager& source;
    SourceManager::LineColumn pos;
    SourceRange range;
  };

  using iterator = std::vector<TokenData>::const_iterator;
  using const_iterator = std::vector<TokenData>::const_iterator;

  explicit TokenStream(std::vector<TokenData> tokens, const SourceManager& source) :
    tokens{std::move(tokens)}, source{source}
  {}

  static auto parse(ProgramContext&, const SourceManager& source) -> TokenStream;

  auto begin() const -> iterator { return this->tokens.begin(); }

  auto end() const -> iterator { return this->tokens.end(); }

  auto source_manager() const noexcept -> const SourceManager&
  {
    return this->source;
  }

private:
  std::vector<TokenData> tokens;
  const SourceManager& source;
};

inline auto make_token_debug(const SourceManager& src_man, SourceRange range)
  -> TokenStream::TokenDebug
{
  const auto pos = src_man.linecol_from_location(range.begin());

  return TokenStream::TokenDebug{src_man, pos, range};
}

inline auto make_token_debug(const TokenStream& tokens, SourceRange range)
  -> TokenStream::TokenDebug
{
  return make_token_debug(tokens.source_manager(), range);
}

} // namespace ccompiler
