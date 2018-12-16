#pragma once
#include "cci/ast/type.hpp"
#include "cci/langopts.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/span.hpp"
#include <cstddef>
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
    ExprClass expr_class;
    ExprCategory value_category;
    QualType type;

    Expr(ExprClass ec, ExprCategory c, QualType ty)
        : expr_class(ec), value_category(c), type(ty)
    {}
};

// Numeric constant that is an integer literal.
struct IntegerLiteral : Expr
{
    uint64_t value;
    srcmap::ByteLoc loc;

    IntegerLiteral(uint64_t val, QualType ty, srcmap::ByteLoc l)
        : Expr(ExprClass::IntegerLiteral, ExprCategory::RValue, ty)
        , value(val)
        , loc(l)
    {}
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
    uint32_t value;
    CharacterConstantKind char_kind;
    srcmap::ByteLoc loc;

    CharacterConstant(uint32_t val, CharacterConstantKind k, QualType ty,
                      srcmap::ByteLoc l)
        : Expr(ExprClass::CharacterConstant, ExprCategory::RValue, ty)
        , value(val)
        , char_kind(k)
        , loc(l)
    {}
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
    span<std::byte> str_data; ///< String content.
    StringLiteralKind str_kind;
    size_t char_byte_width; ///< Character's size in bytes.
    span<srcmap::ByteLoc> tok_locs; ///< Sequence of each string location

    StringLiteral(QualType ty, span<std::byte> str_data, StringLiteralKind kind,
                  size_t cbw, span<srcmap::ByteLoc> locs)
        : Expr(ExprClass::StringLiteral, ExprCategory::LValue, ty)
        , str_data(str_data)
        , str_kind(kind)
        , char_byte_width(cbw)
        , tok_locs(locs)
    {
        cci_expects(!locs.empty());
    }

    auto string_as_utf8() const -> std::string_view
    {
        cci_expects(char_byte_width == 1);
        auto ptr = reinterpret_cast<const char *>(str_data.begin());
        size_t len = str_data.size();
        return {ptr, len};
    }

    auto string_as_bytes() const -> span<const std::byte>
    {
        return as_bytes(str_data);
    }

    auto byte_length() const -> size_t { return str_data.size(); }
    auto length() const -> size_t { return byte_length() / char_byte_width; }

    auto loc() const -> srcmap::ByteLoc { return tok_locs[0]; }
    auto loc_begin() const { return tok_locs.begin(); }
    auto loc_end() const { return tok_locs.end(); }
};

struct ParenExpr : Expr
{
    arena_ptr<Expr> inner_expr;
    srcmap::ByteLoc lparen_loc;
    srcmap::ByteLoc rparen_loc;

    ParenExpr(arena_ptr<Expr> expr, srcmap::ByteLoc lparen,
              srcmap::ByteLoc rparen)
        : Expr(ExprClass::ParenExpr, expr->value_category, expr->type)
        , inner_expr(expr)
        , lparen_loc(lparen)
        , rparen_loc(rparen)
    {}
};

} // namespace cci
