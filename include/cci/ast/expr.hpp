#pragma once
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/type.hpp"
#include "cci/langopts.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/span.hpp"
#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace cci {

// Expression categories.
enum class ExprValueKind
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
    ArraySubscript,
    ImplicitCast,
};

// Expression.
struct Expr
{
private:
    ExprClass ec;
    ExprValueKind vk;
    QualType ty;
    srcmap::Range range;

protected:
    Expr(ExprClass ec, ExprValueKind vk, QualType ty, srcmap::Range r)
        : ec(ec), vk(vk), ty(ty), range(r)
    {}

public:
    auto expr_class() const -> ExprClass { return ec; }
    auto value_kind() const -> ExprValueKind { return vk; }
    auto type() const -> QualType { return ty; }
    auto begin_loc() const -> srcmap::ByteLoc { return range.start; }
    auto end_loc() const -> srcmap::ByteLoc { return range.end; }
    auto source_range() const -> srcmap::Range { return range; }
};

// Numeric constant that is an integer literal.
struct IntegerLiteral : Expr
{
private:
    uint64_t val;

    IntegerLiteral(uint64_t val, QualType ty, srcmap::Range r)
        : Expr(ExprClass::IntegerLiteral, ExprValueKind::RValue, ty, r)
        , val(val)
    {}

public:
    static auto create(const ASTContext &ctx, uint64_t value, QualType ty,
                       srcmap::Range source_range) -> arena_ptr<IntegerLiteral>
    {
        return new (ctx) IntegerLiteral(value, ty, source_range);
    }

    auto value() const -> uint64_t { return val; }
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
    uint32_t val;
    CharacterConstantKind cck;

    CharacterConstant(uint32_t val, CharacterConstantKind cck, QualType ty,
                      srcmap::Range r)
        : Expr(ExprClass::CharacterConstant, ExprValueKind::RValue, ty, r)
        , val(val)
        , cck(cck)
    {}

public:
    static auto create(const ASTContext &ctx, uint32_t value,
                       CharacterConstantKind cck, QualType ty,
                       srcmap::Range source_range)
        -> arena_ptr<CharacterConstant>
    {
        return new (ctx) CharacterConstant(value, cck, ty, source_range);
    }

    auto char_value() const -> uint32_t { return val; }
    auto char_kind() const -> CharacterConstantKind { return cck; }
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
    span<std::byte> str_data; ///< String content.
    StringLiteralKind sk;
    size_t char_byte_width; ///< Character's size in bytes.
    span<srcmap::ByteLoc> tok_locs; ///< Sequence of each string location

    StringLiteral(QualType ty, span<std::byte> str_data, StringLiteralKind sk,
                  size_t cbw, span<srcmap::ByteLoc> locs,
                  srcmap::ByteLoc rquote_loc)
        : Expr(ExprClass::StringLiteral, ExprValueKind::LValue, ty,
               srcmap::Range(locs[0], rquote_loc + srcmap::ByteLoc(1)))
        , str_data(str_data)
        , sk(sk)
        , char_byte_width(cbw)
        , tok_locs(locs)
    {}

public:
    static auto create(const ASTContext &ctx, QualType ty,
                       span<std::byte> str_data, StringLiteralKind sk,
                       size_t cbw, span<srcmap::ByteLoc> locs,
                       srcmap::ByteLoc rquote_loc) -> arena_ptr<StringLiteral>
    {
        cci_expects(!locs.empty());
        return new (ctx) StringLiteral(ty, str_data, sk, cbw, locs, rquote_loc);
    }

    auto str_kind() const -> StringLiteralKind { return sk; }

    auto string_as_utf8() const -> std::string_view
    {
        cci_expects(char_byte_width == 1);
        auto ptr = reinterpret_cast<const char *>(str_data.data());
        size_t len = str_data.size();
        return {ptr, len};
    }

    auto string_as_bytes() const -> span<const std::byte>
    {
        return as_bytes(str_data);
    }

    auto byte_length() const -> size_t { return str_data.size_bytes(); }
    auto length() const -> size_t { return byte_length() / char_byte_width; }
};

struct ParenExpr : Expr
{
private:
    arena_ptr<Expr> inner_expr;
    srcmap::ByteLoc lparen_loc;
    srcmap::ByteLoc rparen_loc;

    ParenExpr(arena_ptr<Expr> expr, srcmap::ByteLoc lparen,
              srcmap::ByteLoc rparen)
        : Expr(ExprClass::ParenExpr, expr->value_kind(), expr->type(),
               srcmap::Range(lparen, rparen + srcmap::ByteLoc(1)))
        , inner_expr(expr)
        , lparen_loc(lparen)
        , rparen_loc(rparen)
    {}

public:
    static auto create(const ASTContext &ctx, arena_ptr<Expr> inner_expr,
                       srcmap::ByteLoc lparen, srcmap::ByteLoc rparen)
        -> arena_ptr<ParenExpr>
    {
        return new (ctx) ParenExpr(inner_expr, lparen, rparen);
    }

    auto sub_expr() const -> arena_ptr<Expr> { return inner_expr; }
    auto open_paren_loc() const -> srcmap::ByteLoc { return lparen_loc; }
    auto close_paren_loc() const -> srcmap::ByteLoc { return rparen_loc; }
};

struct ArraySubscriptExpr : Expr
{
private:
    arena_ptr<Expr> base;
    arena_ptr<Expr> idx;
    srcmap::ByteLoc lbracket_loc;

    ArraySubscriptExpr(arena_ptr<Expr> base, arena_ptr<Expr> idx,
                       ExprValueKind vk, QualType ty,
                       srcmap::ByteLoc lbracket_loc,
                       srcmap::ByteLoc rbracket_loc)
        : Expr(ExprClass::ArraySubscript, vk, ty,
               srcmap::Range(base->begin_loc(),
                             rbracket_loc + srcmap::ByteLoc(1)))
        , base(base)
        , idx(idx)
        , lbracket_loc(lbracket_loc)
    {}

public:
    static auto create(const ASTContext &ctx, arena_ptr<Expr> base_expr,
                       arena_ptr<Expr> index_expr, ExprValueKind vk,
                       QualType ty, srcmap::ByteLoc lbracket_loc,
                       srcmap::ByteLoc rbracket_loc)
        -> arena_ptr<ArraySubscriptExpr>
    {
        cci_expects(base_expr->type()->get_as<PointerType>() != nullptr);
        cci_expects(index_expr->type()->get_as<PointerType>() == nullptr);
        return new (ctx) ArraySubscriptExpr(base_expr, index_expr, vk, ty,
                                            lbracket_loc, rbracket_loc);
    }

    auto base_expr() const -> arena_ptr<Expr> { return base; }
    auto index_expr() const -> arena_ptr<Expr> { return idx; }
    auto open_bracket_loc() const -> srcmap::ByteLoc { return lbracket_loc; }
};

enum class CastKind
{
    LValueToRValue,
    ArrayToPointerDecay,
};

struct CastExpr : Expr
{
private:
    CastKind ck;
    arena_ptr<Expr> op;

protected:
    CastExpr(ExprClass ec, ExprValueKind vk, QualType ty, CastKind ck,
             arena_ptr<Expr> op, srcmap::Range r)
        : Expr(ec, vk, ty, r), ck(ck), op(op)
    {}

public:
    auto cast_kind() const -> CastKind { return ck; }
    auto operand_expr() const -> arena_ptr<Expr> { return op; }
};

struct ImplicitCastExpr : CastExpr
{
private:
    ImplicitCastExpr(ExprValueKind vk, QualType ty, CastKind ck,
                     arena_ptr<Expr> op)
        : CastExpr(ExprClass::ImplicitCast, vk, ty, ck, op, op->source_range())
    {}

public:
    static auto create(const ASTContext &ctx, ExprValueKind vk, QualType ty,
                       CastKind ck, arena_ptr<Expr> operand)
        -> arena_ptr<ImplicitCastExpr>
    {
        return new (ctx) ImplicitCastExpr(vk, ty, ck, operand);
    }
};

static_assert(std::is_trivially_destructible_v<Expr>);
static_assert(std::is_trivially_destructible_v<IntegerLiteral>);
static_assert(std::is_trivially_destructible_v<CharacterConstant>);
static_assert(std::is_trivially_destructible_v<StringLiteral>);
static_assert(std::is_trivially_destructible_v<ParenExpr>);
static_assert(std::is_trivially_destructible_v<ArraySubscriptExpr>);
static_assert(std::is_trivially_destructible_v<ImplicitCastExpr>);

} // namespace cci
