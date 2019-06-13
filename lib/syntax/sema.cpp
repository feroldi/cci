#include "cci/syntax/sema.hpp"
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/literal_parser.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/small_vector.hpp"
#include <memory>
#include <string_view>

using namespace cci;

auto Sema::act_on_numeric_constant(const Token &tok)
    -> std::optional<arena_ptr<Expr>>
{
    cci_expects(tok.is(Category::numeric_constant));

    if (tok.size() == 1)
    {
        const char digit =
            scanner.source_map.range_to_snippet(tok.source_range)[0];
        return IntegerLiteral::create(context, digit - '0', context.int_ty,
                                      tok.source_range);
    }

    small_string<64> spell_buffer;
    std::string_view spelling = scanner.get_spelling(tok, spell_buffer);
    spell_buffer.push_back('\0');
    NumericConstantParser literal(scanner, spelling, tok.location());

    if (literal.has_error)
        return nullptr;

    if (literal.is_integer_literal())
    {
        auto [val, overflowed] = literal.to_integer();

        if (overflowed)
        {
            // We could just stop here, but let's be friends with the user and
            // try to parse and diagnose the source code as much as possible.
            diag_handler.report(tok.location(),
                                diag::Diag::integer_literal_overflow);
        }

        bool allow_unsigned = literal.is_unsigned || literal.radix != 10;
        QualType integer_ty;
        size_t width = 0;

        // Apply [C11 6.4.4.1p5]: The type of an integer constant is the first
        // of the corresponding list in which its value can be represented.
        if (!literal.is_long && !literal.is_long_long)
        {
            size_t int_width = context.target_info.int_width;
            if (val >> int_width == 0)
            {
                if (!literal.is_unsigned && (val >> (int_width - 1)) == 0)
                    integer_ty = context.int_ty;
                else if (allow_unsigned)
                    integer_ty = context.uint_ty;
                width = int_width;
            }
        }

        if (!integer_ty && !literal.is_long_long)
        {
            size_t long_width = context.target_info.long_width;
            if (val >> long_width == 0)
            {
                if (!literal.is_unsigned && (val >> (long_width - 1)) == 0)
                    integer_ty = context.long_ty;
                else if (allow_unsigned)
                    integer_ty = context.ulong_ty;
                width = long_width;
            }
        }

        if (!integer_ty)
        {
            size_t long_long_width = context.target_info.long_long_width;
            if (val >> long_long_width == 0)
            {
                if (!literal.is_unsigned && (val >> (long_long_width - 1)) == 0)
                    integer_ty = context.long_long_ty;
                else if (allow_unsigned)
                    integer_ty = context.ulong_long_ty;
                width = long_long_width;
            }
        }

        if (!integer_ty)
        {
            // [C11 6.4.4.1p6] says that "if an integer constant cannot be
            // represented by any type in its list and has no extended integer
            // type, then the integer constant has no type." However, leaving an
            // expression without a type breaks the code generator and whatnot,
            // so we'll just do whatever GCC or Clang does here. GCC attempts
            // to use __int128, an extended integer type (which [C11 6.4.4.1p6]
            // suggests doing), and Clang settles down to unsigned long long.
            // Given we don't support any extended types, unsigned long long
            // will be the chosen one.
            diag_handler.report(tok.location(),
                                diag::Diag::integer_literal_too_large);
            integer_ty = context.ulong_long_ty;
            width = context.target_info.long_long_width;
        }

        // Truncates the result.
        // FIXME: This 64 is hardcoded, get the correct maximum width from the
        // target info.
        val &= -1U >> (64 - width);

        return IntegerLiteral::create(context, val, integer_ty,
                                      tok.source_range);
    }
    else if (literal.is_floating_literal())
    {
        cci_unreachable(); // TODO: Implement me!
    }

    return nullptr;
}

auto Sema::act_on_char_constant(const Token &tok)
    -> std::optional<arena_ptr<CharacterConstant>>
{
    cci_expects(tok.is_one_of(
        Category::char_constant, Category::utf16_char_constant,
        Category::utf32_char_constant, Category::wide_char_constant));

    small_string<8> spell_buffer;
    std::string_view spelling = scanner.get_spelling(tok, spell_buffer);
    spell_buffer.push_back('\0');
    CharConstantParser literal(scanner, spelling, tok.location(), tok.category,
                               context.target_info);

    if (literal.has_error)
        return nullptr;

    // [C11 6.4.4.4p10]: An integer character constant has type int.
    QualType char_type = context.int_ty;
    auto char_kind = CharacterConstantKind::Ascii;

    switch (literal.category)
    {
        case Category::utf16_char_constant:
            char_type = context.char16_t_ty;
            char_kind = CharacterConstantKind::UTF16;
            break;
        case Category::utf32_char_constant:
            char_type = context.char32_t_ty;
            char_kind = CharacterConstantKind::UTF32;
            break;
        case Category::wide_char_constant:
            char_type = context.wchar_ty;
            char_kind = CharacterConstantKind::Wide;
            break;
        default: cci_expects(Category::char_constant == literal.category);
    }

    return CharacterConstant::create(context, literal.value, char_kind,
                                     char_type, tok.source_range);
}

auto Sema::act_on_string_literal(span<const Token> string_toks)
    -> std::optional<arena_ptr<StringLiteral>>
{
    cci_expects(!string_toks.empty());
    StringLiteralParser literal(scanner, string_toks, context.target_info);
    if (literal.has_error)
        return nullptr;

    cci_expects(literal.char_byte_width == 1 || literal.char_byte_width == 2 ||
                literal.char_byte_width == 4);

    QualType elem_type = context.char_ty;
    auto str_kind = StringLiteralKind::Ascii;

    switch (literal.category)
    {
        case Category::utf8_string_literal:
            str_kind = StringLiteralKind::UTF8;
            break;
        case Category::utf16_string_literal:
            elem_type = context.char16_t_ty;
            str_kind = StringLiteralKind::UTF16;
            break;
        case Category::utf32_string_literal:
            elem_type = context.char32_t_ty;
            str_kind = StringLiteralKind::UTF32;
            break;
        case Category::wide_string_literal:
            elem_type = context.wchar_ty;
            str_kind = StringLiteralKind::Wide;
            break;
        default: cci_expects(Category::string_literal == literal.category);
    }

    // Length of string literal including null character.
    const size_t chars_count = literal.num_string_chars() + 1;
    const size_t bytes_count = literal.byte_length();

    // FIXME: Save this type in ASTContext so it can be reused as canonical.
    auto str_ty =
        QualType(ConstantArrayType::create(context, elem_type, chars_count),
                 Qualifiers::None);

    arena_ptr<std::byte> str_data = new (context) std::byte[bytes_count];

    if (literal.char_byte_width == 1)
        new (str_data) char[chars_count];
    else if (literal.char_byte_width == 2)
        new (str_data) char16_t[chars_count];
    else
    {
        cci_expects(literal.char_byte_width == 4);
        new (str_data) char32_t[chars_count];
    }

    std::memcpy(str_data, literal.string().data(), bytes_count);

    const size_t num_concatenated = string_toks.size();
    arena_ptr<srcmap::ByteLoc> tok_locs =
        new (context) srcmap::ByteLoc[num_concatenated];

    auto locs_ptr = tok_locs;
    for (const Token &tok : string_toks)
        *locs_ptr++ = tok.location();
    cci_ensures(locs_ptr == tok_locs + num_concatenated);

    const srcmap::ByteLoc rquote_loc = string_toks.end()[-1].location();

    return StringLiteral::create(context, str_ty, span(str_data, bytes_count),
                                 str_kind, literal.char_byte_width,
                                 span(tok_locs, num_concatenated), rquote_loc);
}

auto Sema::act_on_paren_expr(arena_ptr<Expr> expr, srcmap::ByteLoc left,
                             srcmap::ByteLoc right)
    -> std::optional<arena_ptr<ParenExpr>>
{
    return ParenExpr::create(context, expr, left, right);
}

auto Sema::act_on_array_subscript(arena_ptr<Expr> base, arena_ptr<Expr> idx,
                                  srcmap::ByteLoc left_loc,
                                  srcmap::ByteLoc right_loc)
    -> std::optional<arena_ptr<ArraySubscriptExpr>>
{
    const auto lhs_expr = function_array_lvalue_conversion(base);
    if (!lhs_expr)
        return std::nullopt;

    const auto rhs_expr = function_array_lvalue_conversion(idx);
    if (!rhs_expr)
        return std::nullopt;

    const QualType lhs_ty = lhs_expr->type();
    const QualType rhs_ty = rhs_expr->type();

    arena_ptr<Expr> base_expr;
    arena_ptr<Expr> index_expr;
    QualType result_ty;

    if (auto ptr_ty = lhs_ty->get_as<PointerType>())
    {
        base_expr = lhs_expr;
        index_expr = rhs_expr;
        result_ty = ptr_ty->pointee_type();
    }
    else if (auto ptr_ty = rhs_ty->get_as<PointerType>())
    {
        // Handle the uncommon case of `123[v]`.
        base_expr = rhs_expr;
        index_expr = lhs_expr;
        result_ty = ptr_ty->pointee_type();
    }
    else
    {
        // C17 6.5.2.1p1: One of the expressions shall have type "pointer to
        // complete object type", the other expression shall have integer type,
        // and the result has type "type".
        diag_handler.report(left_loc, diag::Diag::typecheck_subscript_value)
            .ranges({lhs_expr->source_range(), rhs_expr->source_range()});
        return std::nullopt;
    }

    // C17 6.5.2.1p1
    if (!index_expr->type()->is_integer_type())
    {
        diag_handler
            .report(index_expr->begin_loc(),
                    diag::Diag::typecheck_subscript_not_integer)
            .ranges({index_expr->source_range()});
    }

    return ArraySubscriptExpr::create(context, base_expr, index_expr,
                                      ExprValueKind::LValue, result_ty,
                                      left_loc, right_loc);
}

auto Sema::function_array_lvalue_conversion(arena_ptr<Expr> expr)
    -> std::optional<arena_ptr<Expr>>
{
    auto expr_res = function_array_conversion(expr);
    if (!expr_res)
        return std::nullopt;
    expr_res = lvalue_conversion(*expr_res);
    if (!expr_res)
        return std::nullopt;
    return *expr_res;
}

auto Sema::function_array_conversion(arena_ptr<Expr> expr)
    -> std::optional<arena_ptr<Expr>>
{
    // TODO: Implement for function type
    if (expr->type()->is_array_type())
    {
        // TODO: return the so-qualified decayed from array to pointer type
        return expr;
    }
    return expr;
}

auto Sema::lvalue_conversion(arena_ptr<Expr> expr)
    -> std::optional<arena_ptr<Expr>>
{
    if (expr->type()->is_void_type())
        return expr;

    if (!expr->is_lvalue())
        return expr;

    // C17 6.3.2.1p2:
    //   If the lvalue has qualified type, the value has the unqualified
    //   version of the type of the lvalue;
    QualType ty = expr->type()->get_unqualified_type();
    auto res = ImplicitCastExpr::create(context, ExprValueKind::RValue, ty,
                                        CastKind::LValueToRValue, expr);

    // C17 6.3.2.1p2:
    //   additionally, if the lvalue has atomic type, the value has the
    //   non-atomic version of the type of the lvalue;
    if (const auto *atomic = ty->get_as<AtomicType>())
    {
        ty = atomic->value_type()->get_unqualified_type();
        res = ImplicitCastExpr::create(context, ExprValueKind::RValue, ty,
                                       CastKind::AtomicToNonAtomic, res);
    }

    // C17 6.3.2.1p2:
    //   otherwise, the value has the type of the lvalue.
    return res;
}

static_assert(2 == sizeof(char16_t),
              "UTF-16 string literals assume that char16_t is 2 bytes long");
static_assert(4 == sizeof(char32_t),
              "UTF-32 string literals assume that char32_t is 4 bytes long");
