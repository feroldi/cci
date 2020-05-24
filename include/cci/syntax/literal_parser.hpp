#pragma once

#include "cci/langopts.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/token.hpp"
#include "cci/util/span.hpp"
#include <vector>

namespace cci::syntax {

struct NumericConstantParser
{
private:
    const char *digit_begin; ///< First meaningful digit.
    const char *digit_end; ///< Past the last meaningful digit.

public:
    bool has_error = false;
    bool has_period = false;
    bool has_exponent = false;
    bool is_unsigned = false;
    bool is_long = false;
    bool is_long_long = false;
    bool is_float = false;

    int32_t radix = 0;

    NumericConstantParser(Scanner &, std::string_view tok_spelling,
                          ByteLoc tok_loc);

    // Evaluates and returns the numeric constant to an integer constant value,
    // as well as whether the evaluation overflowed.
    auto to_integer() const -> std::pair<uint64_t, bool>;

    bool is_floating_literal() const { return has_period || has_exponent; }
    bool is_integer_literal() const { return !is_floating_literal(); }
};

struct CharConstantParser
{
    uint32_t value;
    TokenKind char_token_kind;
    bool is_multibyte;
    bool has_error = false;

    CharConstantParser(Scanner &, std::string_view tok_spelling,
                       ByteLoc tok_loc, TokenKind char_token_kind,
                       const TargetInfo &);
};

struct StringLiteralParser
{
private:
    std::vector<char> result_buf;
    char *result_ptr;

public:
    TokenKind token_kind;
    size_t char_byte_width;
    bool has_error = false;

    StringLiteralParser(Scanner &, span<const Token> string_toks,
                        const TargetInfo &);

    // Returns the size in bytes of the string, excluding the null character.
    size_t byte_length() const { return result_ptr - result_buf.data(); }

    // Returns the number of characters in the string, excluding the null
    // character. Note: this doesn't respect Unicode.
    size_t num_string_chars() const { return byte_length() / char_byte_width; }

    auto string() const -> std::string_view
    {
        return {result_buf.data(), byte_length()};
    }

    auto string_as_utf16() const -> std::u16string_view
    {
        cci_expects(char_byte_width == sizeof(char16_t));
        // TODO: This isn't wrong, but isn't the best way either. Should we
        // return a new string?
        auto as_utf16 = reinterpret_cast<const char16_t *>(result_buf.data());
        return {as_utf16, byte_length()};
    }

    auto string_as_utf32() const -> std::u32string_view
    {
        cci_expects(char_byte_width == sizeof(char32_t));
        // TODO: This isn't wrong, but isn't the best way either. Should we
        // return a new string?
        auto as_utf32 = reinterpret_cast<const char32_t *>(result_buf.data());
        return {as_utf32, byte_length()};
    }
};

} // namespace cci::syntax
