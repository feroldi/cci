#pragma once
#include "cci/ast/type.hpp"
#include "cci/basic/source_location.hpp"
#include "cci/langopts.hpp"
#include "cci/util/span.hpp"
#include <cstdint>

namespace cci {

// Expression categories.
enum class ExprCategory
{
  LValue,
  RValue,
};

enum class ExprClass
{
  IntegerLiteral,
  CharacterConstant,
  StringLiteral,
  ParenExpr,
};

// Expression.
struct Expr
{
private:
  ExprClass expr_class_;
  ExprCategory category_;
  QualType type_;

public:
  Expr(ExprClass k, ExprCategory c, QualType ty)
    : expr_class_(k), category_(c), type_(ty)
  {}

  auto expr_class() const -> ExprClass { return expr_class_; }
  auto category() const -> ExprCategory { return category_; }
  auto type() const -> const QualType & { return type_; }
};

// Numeric constant that is an integer literal.
struct IntegerLiteral : Expr
{
private:
  uint64_t value_;
  SourceLocation loc;

public:
  IntegerLiteral(uint64_t val, QualType ty, SourceLocation l)
    : Expr(ExprClass::IntegerLiteral, ExprCategory::RValue, ty)
    , value_(val)
    , loc(l)
  {}

  auto value() const -> uint64_t { return value_; }
  auto location() const -> SourceLocation { return loc; }
};

enum class CharacterConstantKind
{
  Ascii,
  UTF16,
  UTF32,
  Wide,
};

struct CharacterConstant : Expr
{
private:
  uint32_t value_;
  CharacterConstantKind kind_;
  SourceLocation loc;

public:
  CharacterConstant(uint32_t val, CharacterConstantKind k, QualType ty,
                    SourceLocation l)
    : Expr(ExprClass::CharacterConstant, ExprCategory::RValue, ty)
    , value_(val)
    , kind_(k)
    , loc(l)
  {}

  auto value() const -> uint32_t { return value_; }
  auto kind() const -> CharacterConstantKind { return kind_; }
  auto location() const -> SourceLocation { return loc; }
};

enum class StringLiteralKind
{
  Ascii,
  UTF8,
  UTF16,
  UTF32,
  Wide,
};

struct StringLiteral : Expr
{
private:
  span<std::byte> str_data; //< String content.
  StringLiteralKind kind_;
  size_t char_byte_width_; //< Character's size in bytes.
  span<SourceLocation> tok_locs; //< Sequence of each string location

public:
  StringLiteral(QualType ty, span<std::byte> str_data, StringLiteralKind kind,
                size_t char_byte_width, span<SourceLocation> locs)
    : Expr(ExprClass::StringLiteral, ExprCategory::LValue, ty)
    , str_data(str_data)
    , kind_(kind)
    , char_byte_width_(char_byte_width)
    , tok_locs(locs)
  {
    cci_expects(!locs.empty());
  }

  auto string_as_utf8() const -> std::string_view
  {
    cci_expects(char_byte_width() == 1);
    auto ptr = reinterpret_cast<const char *>(str_data.begin());
    size_t len = str_data.size();
    return {ptr, len};
  }

  auto string_as_bytes() const -> span<const std::byte>
  {
    return as_bytes(str_data);
  }

  auto kind() const -> StringLiteralKind { return kind_; }

  auto char_byte_width() const -> size_t { return char_byte_width_; }
  auto byte_length() const -> size_t { return str_data.size(); }
  auto length() const -> size_t { return byte_length() / char_byte_width(); }

  auto location() const -> SourceLocation { return tok_locs[0]; }
  auto loc_begin() const { return tok_locs.begin(); }
  auto loc_end() const { return tok_locs.end(); }
};

struct ParenExpr : Expr
{
private:
  arena_ptr<Expr> inner_expr_;
  SourceLocation lparen_loc;
  SourceLocation rparen_loc;

public:
  ParenExpr(arena_ptr<Expr> expr, SourceLocation lparen, SourceLocation rparen)
    : Expr(ExprClass::ParenExpr, expr->category(), expr->type())
    , inner_expr_(expr)
    , lparen_loc(lparen)
    , rparen_loc(rparen)
  {}

  auto inner_expr() const -> arena_ptr<Expr> { return inner_expr_; }
  auto left_paren_loc() const -> SourceLocation { return lparen_loc; }
  auto right_paren_loc() const -> SourceLocation { return rparen_loc; }
};

} // namespace cci
