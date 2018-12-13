#pragma once

#include "cci/syntax/source_map.hpp"
#include <string>
#include <string_view>

namespace cci {

struct Scanner;

// Category - This represents the category of a token, e.g. identifier,
// keyword etc.
enum class Category
{
    // Keywords
    kw_auto,
    kw_break,
    kw_case,
    kw_char,
    kw_const,
    kw_continue,
    kw_default,
    kw_do,
    kw_double,
    kw_else,
    kw_enum,
    kw_extern,
    kw_float,
    kw_for,
    kw_goto,
    kw_if,
    kw_inline,
    kw_int,
    kw_long,
    kw_register,
    kw_restrict,
    kw_return,
    kw_short,
    kw_signed,
    kw_sizeof,
    kw_static,
    kw_struct,
    kw_switch,
    kw_typedef,
    kw_union,
    kw_unsigned,
    kw_void,
    kw_volatile,
    kw_while,
    kw__Alignas,
    kw__Alignof,
    kw__Atomic,
    kw__Bool,
    kw__Complex,
    kw__Generic,
    kw__Imaginary,
    kw__Noreturn,
    kw__Static_assert,
    kw__Thread_local,

    // 6.4.2 Identifiers.
    identifier,

    // 6.4.4 Constants.
    numeric_constant,

    // 6.4.4.4 Character constants.
    char_constant,
    utf8_char_constant,
    utf16_char_constant,
    utf32_char_constant,
    wide_char_constant,

    // 6.4.5 String literals.
    string_literal,
    utf8_string_literal,
    utf16_string_literal,
    utf32_string_literal,
    wide_string_literal,

    // 6.4.6 Punctuators.
    l_bracket,
    r_bracket,
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    period,
    arrow,
    plusplus,
    minusminus,
    ampersand,
    star,
    plus,
    minus,
    tilde,
    exclama,
    slash,
    percent,
    lessless,
    greatergreater,
    less,
    greater,
    lesslessequal,
    greatergreaterequal,
    equalequal,
    exclamaequal,
    caret,
    pipe,
    ampamp,
    pipepipe,
    question,
    colon,
    semi,
    ellipsis,
    equal,
    starequal,
    slashequal,
    percentequal,
    plusequal,
    minusequal,
    lessequal,
    greaterequal,
    ampequal,
    caretequal,
    pipeequal,
    comma,
    hash,
    hashhash,

    // Some stray character.
    unknown,

    // End of input.
    eof,
};

// Returns a string representation of a Category.
//
// For instance, the name of Category::kw_auto is "auto",
// Category::identifier's name is "identifier", Category::plusplus's
// name is "++" etc.
auto to_string(Category) -> std::string_view;

// Checks whether the parameter is a variant of a string literal.
constexpr auto is_string_literal(Category k) -> bool
{
    switch (k)
    {
        case Category::string_literal:
        case Category::utf8_string_literal:
        case Category::utf16_string_literal:
        case Category::utf32_string_literal:
        case Category::wide_string_literal: return true;
        default: return false;
    }
}

// Checks whether the parameter is a variant of a char constant.
constexpr auto is_char_constant(Category k) -> bool
{
    switch (k)
    {
        case Category::char_constant:
        case Category::utf8_char_constant:
        case Category::utf16_char_constant:
        case Category::utf32_char_constant:
        case Category::wide_char_constant: return true;
        default: return false;
    }
}

// Token - A representation of a token as described in the C11 standard.
struct Token
{
    enum TokenFlags
    {
        None = 0,
        HasUCN = 1 << 0, ///< Contains universal character names.
        IsDirty = 1 << 1, ///< Contains escaped new lines or trigraphs.
        IsLiteral = 1 << 2, ///< Is a string/char literal, or numeric constant.
    };

    Token() = default;
    Token(Category c, srcmap::Range r) noexcept : category_(c), range(r) {}

    auto category() const -> Category { return category_; }

    // Checks whether this token is of category `k`.
    bool is(Category k) const { return category_ == k; }

    // Checks whether this token is not of category `k`.
    bool is_not(Category k) const { return category_ != k; }

    // Checks wether this token is of any category in `ks`.
    template <typename... Kinds>
    bool is_one_of(const Kinds... ks) const
    {
        static_assert((std::is_same_v<Category, Kinds> && ...));
        return (is(ks) || ...);
    }

    // Returns the source location at which this token starts.
    auto location() const -> srcmap::ByteLoc { return range.start; }

    // Returns a source range for the token's text (spelling).
    auto source_range() const -> srcmap::Range { return range; }

    // Returns the size of the token spelling in source.
    auto size() const { return static_cast<size_t>(range.end - range.start); }

    void set_flags(TokenFlags fs) { flags |= fs; }
    void clear_flags(TokenFlags fs) { flags &= ~fs; }

    bool has_UCN() const { return flags & TokenFlags::HasUCN; }
    bool is_dirty() const { return flags & TokenFlags::IsDirty; }
    bool is_literal() const { return flags & TokenFlags::IsLiteral; }

private:
    friend struct Scanner;

    // Token's syntactic category, e.g. kw_return, identifier etc.
    Category category_ = Category::unknown;

    // Token's start and end locations on the source file (lexeme).
    srcmap::Range range;

    // Token's flags.
    uint8_t flags = TokenFlags::None;
};

} // namespace cci
