#pragma once

#include "cci/syntax/source_map.hpp"
#include <string_view>

namespace cci {

// A token kind represents the category of a token, e.g. identifier,
// keyword etc.
enum class TokenKind
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
auto to_string(TokenKind) -> std::string_view;

// Checks whether the parameter is a variant of a string literal.
constexpr auto is_string_literal(TokenKind k) -> bool
{
    switch (k)
    {
        case TokenKind::string_literal:
        case TokenKind::utf8_string_literal:
        case TokenKind::utf16_string_literal:
        case TokenKind::utf32_string_literal:
        case TokenKind::wide_string_literal: return true;
        default: return false;
    }
}

// Checks whether the parameter is a variant of a char constant.
constexpr auto is_char_constant(TokenKind k) -> bool
{
    switch (k)
    {
        case TokenKind::char_constant:
        case TokenKind::utf8_char_constant:
        case TokenKind::utf16_char_constant:
        case TokenKind::utf32_char_constant:
        case TokenKind::wide_char_constant: return true;
        default: return false;
    }
}

// A representation of a token as described in the C11 standard.
struct Token
{
    // Token's syntactic category, e.g. kw_return, identifier etc.
    TokenKind category = TokenKind::unknown;
    // Token's start and end locations on the source file (lexeme).
    srcmap::ByteSpan source_range;

    enum TokenFlags
    {
        None = 0,
        HasUCN = 1 << 0, ///< Contains universal character names.
        IsDirty = 1 << 1, ///< Contains escaped new lines or trigraphs.
        IsLiteral = 1 << 2, ///< Is a string/char literal, or numeric constant.
    };

    Token() = default;
    Token(TokenKind c, srcmap::ByteSpan r) noexcept
        : category(c), source_range(r)
    {}

    // Checks whether this token is of category `k`.
    bool is(TokenKind k) const { return category == k; }

    // Checks whether this token is not of category `k`.
    bool is_not(TokenKind k) const { return category != k; }

    // Checks wether this token is of any category in `ks`.
    template <typename... Kinds>
    bool is_one_of(const Kinds... ks) const
    {
        static_assert((std::is_same_v<TokenKind, Kinds> && ...));
        return (is(ks) || ...);
    }

    // Returns the source location at which this token starts.
    auto location() const -> srcmap::ByteLoc { return source_range.start; }

    // Returns the size of the token spelling in source.
    auto size() const
    {
        return static_cast<size_t>(source_range.end - source_range.start);
    }

    void set_flags(TokenFlags fs) { flags |= fs; }
    void clear_flags(TokenFlags fs) { flags &= ~fs; }

    bool has_UCN() const { return flags & TokenFlags::HasUCN; }
    bool is_dirty() const { return flags & TokenFlags::IsDirty; }
    bool is_literal() const { return flags & TokenFlags::IsLiteral; }

private:
    // Token's flags.
    uint8_t flags = TokenFlags::None;
};

} // namespace cci
