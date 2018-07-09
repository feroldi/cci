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
  QualifiedType type_;

public:
  Expr(ExprClass k, ExprCategory c, QualifiedType ty)
    : expr_class_(k), category_(c), type_(std::move(ty))
  {}

  auto expr_class() const -> ExprClass { return expr_class_; }
  auto category() const -> ExprCategory { return category_; }
  auto type() const -> const QualifiedType & { return type_; }
};

// Numeric constant that is an integer literal.
struct IntegerLiteral : Expr
{
private:
  uint64_t value_;
  SourceLocation loc;

public:
  IntegerLiteral(uint64_t val, QualifiedType ty, SourceLocation l)
    : Expr(ExprClass::IntegerLiteral, ExprCategory::RValue, std::move(ty))
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
  CharacterConstant(uint32_t val, CharacterConstantKind k, QualifiedType ty,
                    SourceLocation l)
    : Expr(ExprClass::CharacterConstant, ExprCategory::RValue, std::move(ty))
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
  std::vector<std::byte> str_data;
  StringLiteralKind kind_;
  size_t char_byte_width_;
  small_vector<SourceLocation, 1> tok_locs;

public:
  StringLiteral(QualifiedType ty, std::vector<std::byte> str_data,
                StringLiteralKind kind, size_t char_byte_width,
                span<const SourceLocation> locs)
    : Expr(ExprClass::StringLiteral, ExprCategory::LValue, std::move(ty))
    , str_data(std::move(str_data))
    , kind_(kind)
    , char_byte_width_(char_byte_width)
    , tok_locs(locs)
  {}

  auto string() const -> std::string_view
  {
    cci_expects(char_byte_width() == 1);
    return {reinterpret_cast<const char *>(str_data.data()), str_data.size()};
  }

  auto string_as_bytes() const -> span<const std::byte>
  {
    return span(str_data);
  }

  auto kind() const -> StringLiteralKind { return kind_; }

  auto char_byte_width() const -> size_t { return char_byte_width_; }
  auto byte_length() const -> size_t { return str_data.size(); }
  auto length() const -> size_t { return byte_length() / char_byte_width(); }
};

struct ParenExpr : Expr
{
private:
  std::unique_ptr<Expr> inner_expr_;
  SourceLocation lparen_loc;
  SourceLocation rparen_loc;

public:
  ParenExpr(std::unique_ptr<Expr> expr, SourceLocation lparen,
            SourceLocation rparen)
    : Expr(ExprClass::ParenExpr, expr->category(), expr->type().clone())
    , inner_expr_(std::move(expr))
    , lparen_loc(lparen)
    , rparen_loc(rparen)
  {}

  auto inner_expr() -> Expr * { return inner_expr_.get(); }
  auto inner_expr() const -> const Expr * { return inner_expr_.get(); }
  auto left_paren_loc() const -> SourceLocation { return lparen_loc; }
  auto right_paren_loc() const -> SourceLocation { return rparen_loc; }
};

} // namespace cci
