#include "cci/syntax/scanner.hpp"
#include "cci/syntax/char_info.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "cci/syntax/unicode_char_set.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/unicode.hpp"
#include <algorithm>
#include <cassert>
#include <memory>
#include <utility>

namespace cci {
namespace {

constexpr auto is_newline(char C) -> bool { return C == '\n' || C == '\r'; }

constexpr auto is_whitespace(char C) -> bool
{
    return C == ' ' || C == '\t' || is_newline(C) || C == '\v' || C == '\f';
}

// The following few helper functions are inspired by Clang's scanner
// implementation.
//
// The scanner works with a few useful functions. Because the C grammar is a
// little complex, it is not possible to implement a scanner that iterates over
// ASCII characters char by char without any special handling. There are things
// like escaped newlines, trigraphs and UCNs which make scanning a bit more
// difficult. With that said, these few functions implement a "peek and consume"
// interface that handles all of those special syntax. The ideia is that in
// order to consume a character, you specify the size of it, i.e. the number of
// characters that theoretically compose only one character.
//
// For example, a trigraph like '??!' would have size 3, where the first '?'
// would signal that it is not a trivial character, and needs special care.
// After peeking it, you get the decoded character ('|'), and the size to be
// skipped over (3). When consuming this peeked character, the buffer pointer
// will end up past the end of the trigraph, i.e. `ptr + 3`, where `ptr` is the
// current buffer pointer.
//
// In case the trigraph '??/' is found, depending on what's next to it, peeking
// it wouldn't return '\', but rather the character after the escaped new-line
// (if there is any), so the size of these special characters would be 3 (the
// trigraph itself) + the size of the escaped new-line (which may be 2 or 3,
// depending on whether there is a carriage return). This goes on and on until
// a trivial character is found, which finally terminates the peeking process.

/// Calculates the size of an escaped newline. Assumes that the slash character
/// is already consumed. Whitespaces between the slash and the newline are
/// considered ill-formed.
//
/// \param after_backslash The position past the backslash ('\') character.
//
/// \return The distance between `ptr` and the first character after the escaped
///         newline.
auto size_for_escaped_newline(const char *after_backslash) -> int64_t
{
    const char *ptr = after_backslash;
    if (is_newline(*ptr))
    {
        ++ptr;
        // Consumes a pair of \r\n or \n\r if there is any.
        if (is_newline(*ptr) && *(ptr - 1) != *ptr)
            ++ptr;
    }
    return ptr - after_backslash;
}

/// Checks whether a character needs any special care.
//
/// Things like trigraphs and escaped newlines are examples of such special
/// characters. They need to be properly consumed, therefore you can't just
/// advance the buffer pointer by 1.
///
/// \param c The character to be checked.
///
/// \return true if the character doesn't need special care.
constexpr bool is_trivial_character(char c) { return c != '?' && c != '\\'; }

// C11 5.2.1.1 Trigraph sequences.
constexpr auto decode_trigraph_letter(char letter)
{
    switch (letter)
    {
        case '=': return '#';
        case '(': return '[';
        case '/': return '\\';
        case ')': return ']';
        case '\'': return '^';
        case '<': return '{';
        case '!': return '|';
        case '>': return '}';
        case '-': return '~';
        default: return '\0';
    }
}

/// Peeks a character from the input stream and returns it, returning the size
/// to how many characters are to be skipped over. This handles special cases
/// like escaped newlines and trigraphs.
//
/// \param ptr Buffer pointer from which to peek a character.
/// \param size Size accumulator. For normal use, set it to 0.
/// \param tok Token being formed, if any.
///
/// \return The character pointed by `ptr`, or a character after escaped
///         new-line, or a decoded trigraph, all along with the character's size.
auto peek_char_and_size_nontrivial(const char *ptr, int64_t size,
                                   Token *tok = nullptr)
    -> std::pair<char, int64_t>
{
    if (*ptr == '\\')
    {
        ++ptr;
        ++size;

    backslash:
        // There's no need to escape anything other than new-lines.
        if (!is_newline(*ptr))
            return {'\\', size};

        if (int64_t nlsize = size_for_escaped_newline(ptr))
        {
            if (tok)
                tok->set_flags(Token::IsDirty);
            ptr += nlsize;
            size += nlsize;
            return peek_char_and_size_nontrivial(ptr, size, tok);
        }

        // Not a newline, just a regular whitespace.
        return {'\\', size};
    }

    // Trigraphs.
    if (ptr[0] == '?' && ptr[1] == '?')
    {
        if (const char c = decode_trigraph_letter(ptr[2]))
        {
            ptr += 3;
            size += 3;
            if (tok)
                tok->set_flags(Token::IsDirty);
            if (c == '\\')
                goto backslash;
            return {c, size};
        }
    }

    // Peek a simple character.
    return {*ptr, size + 1};
}

/// Peeks a character from `ptr` and advances it.
//
/// This is the same as peek_char_and_size, except that the buffer pointer is
/// properly advanced to the next simple character in the buffer.
///
/// \param ptr Buffer pointer from which to peek and advance.
/// \param tok Token being formed.
///
/// \return The character pointed by `ptr` before advancing.
auto peek_char_advance(const char *&ptr, Token &tok) -> char
{
    if (is_trivial_character(*ptr))
        return *ptr++;
    const auto [c, size] = peek_char_and_size_nontrivial(ptr, 0, &tok);
    ptr += size;
    return c;
}

/// Peeks a character and its size from the buffer pointer.
//
/// If the character pointed by `ptr` is simple, then this is the fast path: it
/// returns `*ptr` and a size of 1. Otherwise, this function falls back to
/// `peek_char_and_size_nontrivial`.
///
/// \param ptr Buffer pointer from which to peek a character.
///
/// \return The character pointed by `ptr`, and the size to get to the next
///         consumable character.
auto peek_char_and_size(const char *ptr) -> std::pair<char, int64_t>
{
    if (is_trivial_character(*ptr))
        return {*ptr, 1};
    return peek_char_and_size_nontrivial(ptr, 0, nullptr);
}

/// Consumes the buffer pointer.
//
/// This is meant to be used along with `peek_char_and_size`, as it returns a
/// buffer pointer by repeeking the same character if `size` doesn't correspond
/// to the size of a simple character.  This reiteration is needed in order to
/// set any special flags to the token `tok` being formed.
///
/// \param ptr Buffer pointer from which to consume a peeked character.
/// \param size The size of the character to be consumed.
/// \param tok Token being formed.
///
/// \return A buffer pointer past the peeked (non-)trivial character.
auto consume_char(const char *ptr, int64_t size, Token &tok) -> const char *
{
    // Consumes a simple character.
    if (size == 1)
        return std::next(ptr);
    // Otherwise, reparse it to get the right size.
    size = peek_char_and_size_nontrivial(ptr, 0, &tok).second;
    return ptr + size;
}

} // namespace

// universal-character-name: [C11 6.4.3/1]
//     '\u' hex-quad
//     '\U' hex-quad  hex-quad
//
// hex-quad:
//   hexadecimal-digit hexadecimal-digit
//       hexadecimal-digit hexadecimal-digit
//
/// Reads a universal character name.
//
/// Parses a \u or \U UCN and calculates the code point represented by it. If
/// such code point isn't in a valid range as defined by [C11 6.4.3], reports
/// diagnostics and returns 0, but still consumes the buffer pointer. Ill-formed
/// UCNs prevent the buffer pointer from being consumed, however.
///
/// \param start_ptr Buffer pointer to the UCN kind ('u' or 'U').
/// \param slash_ptr Buffer pointer to the UCN slash ('\') before its kind.
/// \param tok The token being formed, if any.
///
/// \return The code point represented by the UCN.
auto Scanner::try_read_ucn(const char *&start_ptr, const char *slash_ptr,
                           Token *tok) -> uint32_t
{
    const auto [kind, kind_size] = peek_char_and_size(start_ptr);
    const int num_hexdigits = kind == 'u' ? 4 : kind == 'U' ? 8 : 0;

    if (num_hexdigits == 0)
        return 0;

    auto cur_ptr = start_ptr + kind_size;
    uint32_t code_point = 0;

    // Parses the UCN, ignoring any escaped newlines.
    for (int i = 0; i < num_hexdigits; ++i)
    {
        const auto [c, char_size] = peek_char_and_size(cur_ptr);
        const uint32_t value = hexdigit_value(c);
        if (value == -1U)
        {
            report(slash_ptr, diag::Diag::incomplete_ucn);
            return 0;
        }
        code_point <<= 4;
        code_point += value;
        cur_ptr += char_size;
    }

    // Take into account that this token might have escaped newlines,
    // so make any needed changes to tok. If no token is passed, then
    // just set start_ptr, it's good to go.
    if (tok)
    {
        tok->set_flags(Token::HasUCN);
        // Just set start_ptr if the UCN isn't dirty.
        if (std::distance(start_ptr, cur_ptr) == num_hexdigits + 2)
            start_ptr = cur_ptr;
        else
            while (start_ptr != cur_ptr)
                peek_char_advance(start_ptr, *tok);
    }
    else
        start_ptr = cur_ptr;

    // C11 6.4.3/2: A universal character name shall not specify a character
    // whose short identifier is less than 00A0 other than 0024 ($), 0040 (@),
    // or 0060 ('), nor one in the range D800 through DFFF inclusive.
    if (code_point < 0xA0)
    {
        if (code_point != 0x24 && code_point != 0x40 && code_point != 0x60)
        {
            report(slash_ptr, diag::Diag::invalid_ucn);
            return 0;
        }
    }
    // high and low surrogates
    else if (code_point >= 0xD800 && code_point <= 0xDFFF)
    {
        report(slash_ptr, diag::Diag::invalid_ucn);
        return 0;
    }

    return code_point;
}

/// Scans a UCN that is part of an identifier.
//
/// This makes sure that the lexed UCN is a valid character for an identifier.
///
/// \param cur_ptr Buffer pointer that points to the slash ('\'). This pointer
///                is updated to point past the end of the UCN only if the UCN
///                is well-formed in an identifier.
/// \param size Size of the peeked slash character ('\').
/// \param result Token being formed.
///
/// \return true if UCN is well-formed for an identifier.
auto Scanner::try_advance_identifier_ucn(const char *&cur_ptr, int64_t size,
                                         Token &result) -> bool
{
    auto ucn_ptr = cur_ptr + size;
    if (uint32_t code_point = try_read_ucn(ucn_ptr, cur_ptr, nullptr);
        code_point == 0)
        return false;
    const auto ucn_size = std::distance(cur_ptr, ucn_ptr);
    if ((ucn_size == 6 && cur_ptr[1] == 'u') ||
        (ucn_size == 10 && cur_ptr[1] == 'U'))
        cur_ptr = ucn_ptr;
    else
        while (cur_ptr != ucn_ptr)
            peek_char_advance(cur_ptr, result);
    return true;
}

/// Scans a UTF-8 character that is part of an identifier.
//
/// \param cur_ptr Buffer pointer at the start of the UTF-8 sequence.
///
/// \return true if a UTF-8 sequence is parsed, false otherwise.
auto Scanner::try_advance_identifier_utf8(const char *&cur_ptr) -> bool
{
    cci_expects(!is_ascii(cur_ptr[0]));
    const char *uni_ptr = cur_ptr;
    uni::UTF32 code_point = 0;
    uni::ConversionResult res = uni::convert_utf8_sequence(
        reinterpret_cast<const uni::UTF8 **>(&uni_ptr),
        reinterpret_cast<const uni::UTF8 *>(buffer_end), &code_point,
        uni::strictConversion);
    if (res == uni::conversionOK &&
        is_allowed_id_char(static_cast<uint32_t>(code_point)))
    {
        // TODO: Diagnose UTF8 homoglyphs.
        cur_ptr = uni_ptr;
        return true;
    }
    return false;
}

// identifier: [C11 6.4.2]
//   identifier-nondigit
//   identifier  identifier-nondigit
//   identifier  digit
//
// identifier-nondigit:
//   nondigit
//   universal-character-name
//   other implementation-defined characters
//
// nondigit: one of
//   _abcdefghijklm
//   nopqrstuvwxyz
//   ABCDEFGHIJKLM
//   NOPQRSTUVWXYZ
//
// digit: one of
//   0123456789
//
/// Scans an identifier. Assumes that the identifier's head is already consumed.
//
/// \param cur_ptr A pointer into the buffer that is past the first identifier's
///                character.
/// \param result Token being formed.
///
/// \return true if identifier was successfully formed.
auto Scanner::lex_identifier(const char *cur_ptr, Token &result) -> bool
{
    auto is_ident = [](char c) {
        return is_alphanum(c) || c == '_' || c == '$';
    };
    // Most of the heavy work can be avoided if the identifier is
    // formed by ASCII characters only.
    cur_ptr = std::find_if_not(cur_ptr, buffer_end, is_ident);

    // If there's dirt, then lexes the rest of the identifier.
    if (!is_ascii(*cur_ptr) || !is_trivial_character(*cur_ptr))
    {
        auto [c, size] = peek_char_and_size(cur_ptr);
        while (true)
        {
            if (c == '\\' && try_advance_identifier_ucn(cur_ptr, size, result))
            {
                std::tie(c, size) = peek_char_and_size(cur_ptr);
                continue;
            }
            else if (!is_ascii(c) && try_advance_identifier_utf8(cur_ptr))
            {
                std::tie(c, size) = peek_char_and_size(cur_ptr);
                continue;
            }
            else if (!is_ident(c))
                break; // We're done.

            cur_ptr = consume_char(cur_ptr, size, result);
            std::tie(c, size) = peek_char_and_size(cur_ptr);

            // Handles escaped newlines and trigraphs.
            while (is_ident(c))
            {
                cur_ptr = consume_char(cur_ptr, size, result);
                std::tie(c, size) = peek_char_and_size(cur_ptr);
            }
        }
    }

    form_token(result, cur_ptr, TokenKind::identifier);

    // Changes the token's category to a keyword if this happens to be one.
    if (!result.has_UCN())
    {
        small_string<16> ident_buf;
        const auto spelling = this->get_spelling(result, ident_buf);

        static constexpr TokenKind keyword_kinds[]{
            TokenKind::kw_auto,           TokenKind::kw_break,
            TokenKind::kw_case,           TokenKind::kw_char,
            TokenKind::kw_const,          TokenKind::kw_continue,
            TokenKind::kw_default,        TokenKind::kw_do,
            TokenKind::kw_double,         TokenKind::kw_else,
            TokenKind::kw_enum,           TokenKind::kw_extern,
            TokenKind::kw_float,          TokenKind::kw_for,
            TokenKind::kw_goto,           TokenKind::kw_if,
            TokenKind::kw_inline,         TokenKind::kw_int,
            TokenKind::kw_long,           TokenKind::kw_register,
            TokenKind::kw_restrict,       TokenKind::kw_return,
            TokenKind::kw_short,          TokenKind::kw_signed,
            TokenKind::kw_sizeof,         TokenKind::kw_static,
            TokenKind::kw_struct,         TokenKind::kw_switch,
            TokenKind::kw_typedef,        TokenKind::kw_union,
            TokenKind::kw_unsigned,       TokenKind::kw_void,
            TokenKind::kw_volatile,       TokenKind::kw_while,
            TokenKind::kw__Alignas,       TokenKind::kw__Alignof,
            TokenKind::kw__Atomic,        TokenKind::kw__Bool,
            TokenKind::kw__Complex,       TokenKind::kw__Generic,
            TokenKind::kw__Imaginary,     TokenKind::kw__Noreturn,
            TokenKind::kw__Static_assert, TokenKind::kw__Thread_local,
        };

        // FIXME: This is rather slow.
        for (const TokenKind cat : keyword_kinds)
        {
            if (spelling == to_string(cat))
            {
                result.category = cat;
                break;
            }
        }
    }

    return true;
}

/// Scans a numeric literal constant (integer-constant or floating-constant).
/// Assumes that the first digit is already consumed.
//
/// This just matches a regex that validates such constants. Some syntax
/// checking is delayed until semantic analysis.
///
/// \param cur_ptr Pointer past the first digit of the numeric constant.
/// \param result Token being formed.
///
/// \return true if numeric constant was successfully formed.
auto Scanner::lex_numeric_constant(const char *cur_ptr, Token &result) -> bool
{
    auto [c, digit_size] = peek_char_and_size(cur_ptr);
    char prev = c;

    // Matches the regex /[0-9a-zA-Z.]*/.
    while (is_alphanum(c) || c == '.')
    {
        cur_ptr = consume_char(cur_ptr, digit_size, result);
        prev = c;
        std::tie(c, digit_size) = peek_char_and_size(cur_ptr);
    }

    // If we stumbled upon something that doesn't seem to be part of a numeric
    // constant, check whether it's a (possibly binary) exponent of a floating
    // constant. If so, continue scanning, otherwise stop munching.

    // exponent-part: [C11 6.4.4.2]
    //   'e' sign[opt] digit-sequence
    //   'E' sign[opt] digit-sequence
    // binary-exponent-part:
    //    'p' sign[opt] digit-sequence
    //    'P' sign[opt] digit-sequence
    if ((c == '+' || c == '-') ||
        (prev == 'e' || prev == 'E' || prev == 'p' || prev == 'P'))
        return lex_numeric_constant(consume_char(cur_ptr, digit_size, result),
                                    result);

    form_token(result, cur_ptr, TokenKind::numeric_constant);
    result.set_flags(Token::IsLiteral);
    return true;
}

/// Skips a line comment, returning a pointer past the end of the comment.
/// Assumes that the // part is already lexed.
//
/// \param cur_ptr Buffer pointer which points past the second '/' comment
///                character.
///
/// \return A pointer past the end of the comment, i.e. the newline.
auto Scanner::skip_line_comment(const char *cur_ptr) -> const char *
{
    auto [c, c_size] = peek_char_and_size(cur_ptr);

    // C11 6.4.9/2: Except within a character constant, a string literal, or a
    // comment, the characters // introduce a comment that includes all
    // multibyte characters up to, but not including, the next new-line
    // character. The contents of such a comment are examined only to identify
    // multibyte characters and to find the terminating new-line character.
    while (true)
    {
        if (is_newline(c))
        {
            cur_ptr += c_size;
            break; // We're done.
        }

        // End of input; ill-formed program. Even though this is assured to
        // never happen, we still let this check here.
        if (c == '\0')
        {
            report(cur_ptr, diag::Diag::unterminated_comment);
            break;
        }

        cur_ptr += c_size;
        std::tie(c, c_size) = peek_char_and_size(cur_ptr);
    }

    return cur_ptr;
}

/// Skips a block comment, returning a pointer past the end of the comment, i.e.
/// after the '*/' part. Assumes that the // part is already lexed.
//
/// \param cur_ptr Buffer pointer which points past the '*' from /*
///                string.
///
/// \return A pointer past the end of the comment.
auto Scanner::skip_block_comment(const char *cur_ptr) -> const char *
{
    auto [c, c_size] = peek_char_and_size(cur_ptr);
    char prev = c;

    // Could be recursive, but that might not be a good idea.  This also could
    // be improved upon. Right now it handles trigraphs and escaped newlines,
    // which does the job just fine.
    while (true)
    {
        // C11 6.4.9/1: Except within a character constant, a string literal, or
        // a comment, the characters /* introduce a comment. The contents of
        // such a comment are examined only to identify multibyte characters and
        // to find the characters */ that terminate it. 83)
        //
        // 83) Thus, /* ... */ comments do not nest.
        if (c == '/' && prev == '*')
        {
            cur_ptr += c_size;
            break; // We're done.
        }

        // Missing the terminating */ block comment.
        if (c == '\0')
        {
            report(cur_ptr, diag::Diag::unterminated_comment);
            break;
        }

        cur_ptr += c_size;
        prev = c;
        std::tie(c, c_size) = peek_char_and_size(cur_ptr);
    }

    return cur_ptr;
}

// character-constant: [C11 6.4.4.4]
//    ' c-char-sequence '
//    L' c-char-sequence '
//    L' c-char-sequence '
//    U' c-char-sequence '
//
// c-char-sequence:
//    c-char
//    c-char-sequence c-char
//
// c-char:
//    any member of the source character set except
//      the single-quote ', backslash \, or new-line character
//    escape-sequence
//
// escape-sequence:
//    simple-escape-sequence
//    octal-escape-sequence
//    hexadecimal-escape-sequence
//    universal-character-name
//
// simple-escape-sequence: one of
//    \' \" \? \\ \a \b \f \n \r \t \v
//
// octal-escape-sequence:
//    \ octal-digit
//    \ octal-digit octal-digit
//    \ octal-digit octal-digit octal-digit
//
// hexadecimal-escape-sequence:
//    \x hexadecimal-digit
//    hexadecimal-escape-sequence hexadecimal-digit

/// Scans a character constant literal. Assumes that the prefix and ' are
/// already lexed.
//
/// \param cur_ptr Buffer pointer that points past the ' character.
/// \param result Token being formed.
/// \param char_category Token category of this character constant. This is
///                      given by the character constant's prefix (or the lack
///                      thereof).
///
/// \return true if character constant is successfully lexed.
auto Scanner::lex_character_constant(const char *cur_ptr, Token &result,
                                     const TokenKind char_category) -> bool
{
    cci_expects(char_category == TokenKind::char_constant ||
                char_category == TokenKind::utf8_char_constant ||
                char_category == TokenKind::utf16_char_constant ||
                char_category == TokenKind::utf32_char_constant ||
                char_category == TokenKind::wide_char_constant);

    char c = peek_char_advance(cur_ptr, result);

    if (c == '\'')
    {
        report(buffer_ptr, diag::Diag::char_const_empty);
        form_token(result, cur_ptr, TokenKind::unknown);
        return true;
    }

    while (c != '\'')
    {
        // Skips this character for now. Decoding and checking of escape
        // sequences occur later on in semantic analysis.
        if (c == '\\')
            c = *cur_ptr++;

        else if (is_newline(c) || c == '\0')
        {
            report(buffer_ptr, diag::Diag::unterminated_char_const);
            form_token(result, std::prev(cur_ptr), TokenKind::unknown);
            return true;
        }

        c = peek_char_advance(cur_ptr, result);
    }

    form_token(result, cur_ptr, char_category);
    result.set_flags(Token::IsLiteral);
    return true;
}

// string-literal: [C11 6.4.5]
//    encoding-prefix[opt] " s-char-sequence[opt] "
//
// encoding-prefix:
//    u8
//    u
//    U
//    L
//
// s-char-sequence:
//    s-char
//    s-char-sequence s-char
//
// s-char:
//    any member of the source character set except
//      the double-quote ", backslash \, or new-line character.
//    escape-sequence
//
/// Scans a string literal. Assumes that the prefix and " are already scanned.
//
/// \param cur_ptr Buffer pointer that points pat the " character.
/// \param result Token being formed.
/// \param str_category Token category of this string literal. This is given by
///        the string literals's prefix (or the lack thereof).
///
/// \return true if string literal is successfully lexed.
auto Scanner::lex_string_literal(const char *cur_ptr, Token &result,
                                 const TokenKind str_category) -> bool
{
    cci_expects(str_category == TokenKind::string_literal ||
                str_category == TokenKind::utf8_string_literal ||
                str_category == TokenKind::utf16_string_literal ||
                str_category == TokenKind::utf32_string_literal ||
                str_category == TokenKind::wide_string_literal);

    char c = peek_char_advance(cur_ptr, result);

    while (c != '"')
    {
        // Skips this character for now. Decoding and checking of escape
        // sequences occur later on in semantic analysis.
        if (c == '\\')
            c = *cur_ptr++;

        else if (is_newline(c) || c == '\0')
        {
            report(buffer_ptr, diag::Diag::unterminated_string_literal);
            form_token(result, std::prev(cur_ptr), TokenKind::unknown);
            return true;
        }

        c = peek_char_advance(cur_ptr, result);
    }

    form_token(result, cur_ptr, str_category);
    result.set_flags(Token::IsLiteral);
    return true;
}

/// Scans an identifier that starts with a UCN or a UTF-8 character.
//
/// \param cur_ptr Buffer pointer past the lexed code point.
/// \param code_point Identifier's UTF-8 code point head.
/// \param result Token being formed.
///
/// \return true if identifier is lexed.
auto Scanner::lex_unicode(const char *cur_ptr, uint32_t code_point,
                          Token &result) -> bool
{
    if (is_allowed_id_char(code_point) &&
        is_allowed_initially_id_char(code_point))
    {
        // TODO: Diagnose UTF8 homoglyphs.
        return lex_identifier(cur_ptr, result);
    }
    form_token(result, cur_ptr, TokenKind::unknown);
    return true;
}

/// Scans the next token in the source buffer.
//
/// \param cur_ptr Pointer into the source buffer from which to scan a token.
/// \param result Output parameter to which a lexed token is set on success.
///
/// \return true if a token was lexed, and false if end of input is reached.
auto Scanner::lex_token(const char *cur_ptr, Token &result) -> bool
{
scan:
    if (cur_ptr == buffer_end)
        return false;

    // Skips any whitespace before the token.
    cur_ptr = std::find_if_not(cur_ptr, buffer_end, is_whitespace);
    buffer_ptr = cur_ptr;

    auto [ch, ch_size] = peek_char_and_size(cur_ptr);

    if (is_newline(ch))
    {
        cur_ptr += ch_size;
        goto scan;
    }

    cur_ptr = consume_char(cur_ptr, ch_size, result);
    auto category = TokenKind::unknown;

    switch (ch)
    {
        case '\0': return false; // End of input.

        case '\\':
            if (uint32_t code_point =
                    try_read_ucn(cur_ptr, buffer_ptr, nullptr);
                code_point != 0)
                return lex_unicode(cur_ptr, code_point, result);
            else
                break;

        case '[': category = TokenKind::l_bracket; break;
        case ']': category = TokenKind::r_bracket; break;
        case '(': category = TokenKind::l_paren; break;
        case ')': category = TokenKind::r_paren; break;
        case '{': category = TokenKind::l_brace; break;
        case '}': category = TokenKind::r_brace; break;
        case '~': category = TokenKind::tilde; break;
        case ';': category = TokenKind::semi; break;
        case ',': category = TokenKind::comma; break;

        case '.':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (is_digit(ch))
                return lex_numeric_constant(
                    consume_char(cur_ptr, ch_size, result), result);
            else if (ch == '.')
            {
                if (const auto [after, after_size] =
                        peek_char_and_size(cur_ptr + ch_size);
                    after == '.')
                {
                    category = TokenKind::ellipsis;
                    cur_ptr =
                        consume_char(consume_char(cur_ptr, ch_size, result),
                                     after_size, result);
                }
            }
            else
                category = TokenKind::period;
            break;

        case '-':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '>')
            {
                category = TokenKind::arrow;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '-')
            {
                category = TokenKind::minusminus;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '=')
            {
                category = TokenKind::minusequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::minus;
            break;

        case '+':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '+')
            {
                category = TokenKind::plusplus;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '=')
            {
                category = TokenKind::plusequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::plus;
            break;

        case '&':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '&')
            {
                category = TokenKind::ampamp;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '=')
            {
                category = TokenKind::ampequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::ampersand;
            break;

        case '*':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '=')
            {
                category = TokenKind::starequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::star;
            break;

        case '/':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '/')
            {
                // NOTE: Don't handle line comments that are actually an
                // operator and a block comment in C89. E.g. `a //**/ b`, which
                // should be `a / b` in C89, but is currently parsed as `a`,
                // because of C11's line comments.
                buffer_ptr = skip_line_comment(cur_ptr + ch_size);
                return lex_token(buffer_ptr, result);
            }
            else if (ch == '*')
            {
                buffer_ptr = skip_block_comment(cur_ptr + ch_size);
                return lex_token(buffer_ptr, result);
            }
            else if (ch == '=')
            {
                category = TokenKind::slashequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::slash;
            break;

        case '%':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '=')
            {
                category = TokenKind::percentequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '>') // %> digraph.
            {
                category = TokenKind::r_brace;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == ':') // %: digraph.
            {
                cur_ptr = consume_char(cur_ptr, ch_size, result);
                std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
                const auto [after, after_size] =
                    peek_char_and_size(cur_ptr + ch_size);
                if (ch == '%' && after == ':')
                {
                    // %:%: digraph
                    category = TokenKind::hashhash;
                    cur_ptr =
                        consume_char(consume_char(cur_ptr, ch_size, result),
                                     after_size, result);
                }
                else
                    category = TokenKind::hash;
            }
            else
                category = TokenKind::percent;
            break;

        case '<':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '<')
            {
                if (const auto [after, after_size] =
                        peek_char_and_size(cur_ptr + ch_size);
                    after == '=')
                {
                    category = TokenKind::lesslessequal;
                    cur_ptr =
                        consume_char(consume_char(cur_ptr, ch_size, result),
                                     after_size, result);
                }
                else
                {
                    category = TokenKind::lessless;
                    cur_ptr = consume_char(cur_ptr, ch_size, result);
                }
            }
            else if (ch == '=')
            {
                category = TokenKind::lessequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == ':') // <: digraph.
            {
                category = TokenKind::l_bracket;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '%') // <% digraph.
            {
                category = TokenKind::l_brace;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::less;
            break;

        case '>':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '>')
            {
                if (const auto [after, after_size] =
                        peek_char_and_size(cur_ptr + ch_size);
                    after == '=')
                {
                    category = TokenKind::greatergreaterequal;
                    cur_ptr =
                        consume_char(consume_char(cur_ptr, ch_size, result),
                                     after_size, result);
                }
                else
                {
                    category = TokenKind::greatergreater;
                    cur_ptr = consume_char(cur_ptr, ch_size, result);
                }
            }
            else if (ch == '=')
            {
                category = TokenKind::greaterequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::greater;
            break;

        case '=':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '=')
            {
                category = TokenKind::equalequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::equal;
            break;

        case '!':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '=')
            {
                category = TokenKind::exclamaequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::exclama;
            break;

        case '^':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '=')
            {
                category = TokenKind::caretequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::caret;
            break;

        case '|':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '|')
            {
                category = TokenKind::pipepipe;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else if (ch == '=')
            {
                category = TokenKind::pipeequal;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::pipe;
            break;

        case '?': category = TokenKind::question; break;

        case ':':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '>') // :> digraph.
            {
                category = TokenKind::r_bracket;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::colon;
            break;

        case '#':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '#')
            {
                category = TokenKind::hashhash;
                cur_ptr = consume_char(cur_ptr, ch_size, result);
            }
            else
                category = TokenKind::hash;
            break;

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': return lex_numeric_constant(cur_ptr, result);

        case 'L':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '\'')
                return lex_character_constant(
                    consume_char(cur_ptr, ch_size, result), result,
                    TokenKind::wide_char_constant);
            if (ch == '"')
                return lex_string_literal(
                    consume_char(cur_ptr, ch_size, result), result,
                    TokenKind::wide_string_literal);
            return lex_identifier(cur_ptr, result);

        case 'u':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '\'')
                return lex_character_constant(
                    consume_char(cur_ptr, ch_size, result), result,
                    TokenKind::utf16_char_constant);
            if (ch == '8')
            {
                if (const auto [after, after_size] =
                        peek_char_and_size(cur_ptr + ch_size);
                    after == '"')
                {
                    cur_ptr = consume_char(cur_ptr, ch_size, result);
                    return lex_string_literal(
                        consume_char(cur_ptr, after_size, result), result,
                        TokenKind::utf8_string_literal);
                }
            }
            if (ch == '"')
                return lex_string_literal(
                    consume_char(cur_ptr, ch_size, result), result,
                    TokenKind::utf16_string_literal);
            return lex_identifier(cur_ptr, result);

        case 'U':
            std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
            if (ch == '\'')
                return lex_character_constant(
                    consume_char(cur_ptr, ch_size, result), result,
                    TokenKind::utf32_char_constant);
            if (ch == '"')
                return lex_string_literal(
                    consume_char(cur_ptr, ch_size, result), result,
                    TokenKind::utf32_string_literal);
            [[fallthrough]];

            // clang-format off
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': /*   'u'*/
        case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
        case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
        case 'J': case 'K': /*   'L'*/case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': /*   'U'*/case 'V': case 'W':
        case 'X': case 'Y': case 'Z': case '_': case '$':
            // clang-format on
            return lex_identifier(cur_ptr, result);

        case '\'':
            return lex_character_constant(cur_ptr, result,
                                          TokenKind::char_constant);

        case '"':
            return lex_string_literal(cur_ptr, result,
                                      TokenKind::string_literal);

        default: {
            if (is_ascii(ch))
                break;
            --cur_ptr;
            uni::UTF32 code_point = 0;
            uni::ConversionResult res = uni::convert_utf8_sequence(
                reinterpret_cast<const uni::UTF8 **>(&cur_ptr),
                reinterpret_cast<const uni::UTF8 *>(buffer_end), &code_point,
                uni::strictConversion);
            if (res == uni::conversionOK)
                return lex_unicode(cur_ptr, static_cast<uint32_t>(code_point),
                                   result);
        }
    }

    if (category == TokenKind::unknown)
    {
        if (is_ascii(ch))
            report(buffer_ptr, diag::Diag::unknown_character).args(ch);
        else
            report(buffer_ptr, diag::Diag::invalid_unicode_char);
    }

    form_token(result, cur_ptr, category);
    return true;
}

auto Scanner::next_token() -> Token
{
    if (Token result; lex_token(buffer_ptr, result))
        return result;
    return Token(TokenKind::eof, srcmap::ByteSpan{});
}

auto Scanner::character_location(srcmap::ByteLoc tok_loc,
                                 const char *spelling_begin,
                                 const char *char_pos) const -> srcmap::ByteLoc
{
    const auto [file, offset] = this->source_map.lookup_byte_offset(tok_loc);
    const char *cur_ptr = this->buffer_begin + static_cast<size_t>(offset);
    for (auto it = spelling_begin; it != char_pos; ++it)
    {
        const auto [c, size] =
            peek_char_and_size_nontrivial(cur_ptr, 0, nullptr);
        cci_expects(c == *it);
        cur_ptr += size;
    }
    return this->source_map.ptr_to_byteloc(this->file_loc, cur_ptr);
}

auto Scanner::get_spelling_to_buffer(const Token &tok, char *spelling_buf,
                                     const srcmap::SourceMap &map) -> size_t
{
    std::string_view spelling = map.span_to_snippet(tok.source_range);
    const auto spell_start = spelling.begin();
    const auto spell_end = spelling.end();

    // No newline escapes, UCNs, nor trigraphs. We're good to go.
    if (!tok.is_dirty())
    {
        const char *const buf_start = spelling_buf;
        const char *const buf_end =
            std::copy(spell_start, spell_end, spelling_buf);
        return static_cast<size_t>(buf_end - buf_start);
    }
    auto it = spell_start;
    size_t length = 0;
    while (it != spell_end)
    {
        const auto [c, size] = peek_char_and_size_nontrivial(it, 0, nullptr);
        *spelling_buf++ = c;
        it += size;
        ++length;
    }
    // Dirty tokens have to shrink in size.
    cci_ensures(length < tok.size());
    return length;
}

auto to_string(TokenKind k) -> std::string_view
{
    switch (k)
    {
        case TokenKind::kw_auto: return "auto";
        case TokenKind::kw_break: return "break";
        case TokenKind::kw_case: return "case";
        case TokenKind::kw_char: return "char";
        case TokenKind::kw_const: return "const";
        case TokenKind::kw_continue: return "continue";
        case TokenKind::kw_default: return "default";
        case TokenKind::kw_do: return "do";
        case TokenKind::kw_double: return "double";
        case TokenKind::kw_else: return "else";
        case TokenKind::kw_enum: return "enum";
        case TokenKind::kw_extern: return "extern";
        case TokenKind::kw_float: return "float";
        case TokenKind::kw_for: return "for";
        case TokenKind::kw_goto: return "goto";
        case TokenKind::kw_if: return "if";
        case TokenKind::kw_inline: return "inline";
        case TokenKind::kw_int: return "int";
        case TokenKind::kw_long: return "long";
        case TokenKind::kw_register: return "register";
        case TokenKind::kw_restrict: return "restrict";
        case TokenKind::kw_return: return "return";
        case TokenKind::kw_short: return "short";
        case TokenKind::kw_signed: return "signed";
        case TokenKind::kw_sizeof: return "sizeof";
        case TokenKind::kw_static: return "static";
        case TokenKind::kw_struct: return "struct";
        case TokenKind::kw_switch: return "switch";
        case TokenKind::kw_typedef: return "typedef";
        case TokenKind::kw_union: return "union";
        case TokenKind::kw_unsigned: return "unsigned";
        case TokenKind::kw_void: return "void";
        case TokenKind::kw_volatile: return "volatile";
        case TokenKind::kw_while: return "while";
        case TokenKind::kw__Alignas: return "_Alignas";
        case TokenKind::kw__Alignof: return "_Alignof";
        case TokenKind::kw__Atomic: return "_Atomic";
        case TokenKind::kw__Bool: return "_Bool";
        case TokenKind::kw__Complex: return "_Complex";
        case TokenKind::kw__Generic: return "_Generic";
        case TokenKind::kw__Imaginary: return "_Imaginary";
        case TokenKind::kw__Noreturn: return "_Noreturn";
        case TokenKind::kw__Static_assert: return "_Static_assert";
        case TokenKind::kw__Thread_local: return "_Thread_local";
        case TokenKind::identifier: return "identifier";
        case TokenKind::numeric_constant: return "numeric constant";
        case TokenKind::char_constant: return "character constant";
        case TokenKind::utf8_char_constant: return "utf-8 character constant";
        case TokenKind::utf16_char_constant: return "utf-16 character constant";
        case TokenKind::utf32_char_constant: return "utf-32 character constant";
        case TokenKind::wide_char_constant: return "wide character constant";
        case TokenKind::string_literal: return "string literal";
        case TokenKind::utf8_string_literal: return "utf-8 string literal";
        case TokenKind::utf16_string_literal: return "utf-16 string literal";
        case TokenKind::utf32_string_literal: return "utf-32 string literal";
        case TokenKind::wide_string_literal: return "wide string literal";
        case TokenKind::l_bracket: return "[";
        case TokenKind::r_bracket: return "]";
        case TokenKind::l_paren: return "(";
        case TokenKind::r_paren: return ")";
        case TokenKind::l_brace: return "{";
        case TokenKind::r_brace: return "}";
        case TokenKind::period: return ".";
        case TokenKind::arrow: return "->";
        case TokenKind::plusplus: return "++";
        case TokenKind::minusminus: return "--";
        case TokenKind::ampersand: return "&";
        case TokenKind::star: return "*";
        case TokenKind::plus: return "+";
        case TokenKind::minus: return "-";
        case TokenKind::tilde: return "~";
        case TokenKind::exclama: return "!";
        case TokenKind::slash: return "/";
        case TokenKind::percent: return "%";
        case TokenKind::lessless: return "<<";
        case TokenKind::greatergreater: return ">>";
        case TokenKind::less: return "<";
        case TokenKind::greater: return ">";
        case TokenKind::lesslessequal: return "<<=";
        case TokenKind::greatergreaterequal: return ">>=";
        case TokenKind::equalequal: return "==";
        case TokenKind::exclamaequal: return "!=";
        case TokenKind::caret: return "^";
        case TokenKind::pipe: return "|";
        case TokenKind::ampamp: return "&&";
        case TokenKind::pipepipe: return "||";
        case TokenKind::question: return "?";
        case TokenKind::colon: return ":";
        case TokenKind::semi: return ";";
        case TokenKind::ellipsis: return "...";
        case TokenKind::equal: return "=";
        case TokenKind::starequal: return "*=";
        case TokenKind::slashequal: return "/=";
        case TokenKind::percentequal: return "%=";
        case TokenKind::plusequal: return "+=";
        case TokenKind::minusequal: return "-=";
        case TokenKind::lessequal: return "<=";
        case TokenKind::greaterequal: return ">=";
        case TokenKind::ampequal: return "&=";
        case TokenKind::caretequal: return "^=";
        case TokenKind::pipeequal: return "|=";
        case TokenKind::comma: return ",";
        case TokenKind::hash: return "#";
        case TokenKind::hashhash: return "##";
        case TokenKind::unknown: return "<unknown>";
        case TokenKind::eof: return "<end of input>";
    }

    cci_unreachable();
}

} // namespace cci
