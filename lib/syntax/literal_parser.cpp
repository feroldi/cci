#include "cci/syntax/literal_parser.hpp"
#include "cci/langopts.hpp"
#include "cci/syntax/char_info.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/small_vector.hpp"
#include "cci/util/span.hpp"
#include "cci/util/unicode.hpp"
#include <algorithm>
#include <climits>
#include <utility>
#include <vector>

using namespace cci;

// Returns the correct selector (i.e. if it's a decimal, octal, or hexadecimal)
// for a given radix. This is used only for diagnostics to select the
// appropriate message for a given number base.
static auto select_radix(uint32_t radix) -> diag::select
{
    return diag::select(
        radix == 10 ? 0 : radix == 8 ? 1 : radix == 16 ? 2 : cci_unreachable());
}

static void report(Scanner &scan, const char *char_ptr, srcmap::ByteLoc tok_loc,
                   const char *tok_begin, std::string message)
{
    auto &diag = scan.diagnostics();
    diag.report(scan.character_location(tok_loc, tok_begin, char_ptr),
                std::move(message));
}

// Returns the respective character type width for a given string literal or
// character constant token.
static auto map_char_width(Category category, const TargetInfo &target)
    -> size_t
{
    cci_expects(is_char_constant(category) || is_string_literal(category));
    switch (category)
    {
        case Category::char_constant:
        case Category::string_literal:
        case Category::utf8_char_constant:
        case Category::utf8_string_literal: return target.char_width;
        case Category::wide_char_constant:
        case Category::wide_string_literal: return target.wchar_width;
        case Category::utf16_char_constant:
        case Category::utf16_string_literal: return target.char16_t_width;
        case Category::utf32_char_constant:
        case Category::utf32_string_literal: return target.char32_t_width;
        default: cci_unreachable();
    }
}

NumericConstantParser::NumericConstantParser(Scanner &scanner,
                                             std::string_view tok_lexeme,
                                             srcmap::ByteLoc tok_loc)
{
    cci_expects(tok_lexeme.end()[0] == '\0');
    const char *const tok_begin = tok_lexeme.begin();
    const char *const tok_end = tok_lexeme.end();
    const char *s = tok_begin;
    digit_begin = tok_begin;

    auto parse_possible_period_or_exponent = [&, this] {
        cci_expects(radix == 10 || radix == 8);

        if (is_hexdigit(s[0]) && *s != 'e' && *s != 'E')
        {
            report(scanner, s, tok_loc, tok_begin,
                   fmt::format("invalid digit '{0}' for "
                               "{1:decimal|octal|hexadecimal} integer constant",
                               *s, select_radix(radix)));
            has_error = true;
            return;
        }

        if (*s == '.')
        {
            has_period = true;
            radix = 10;
            s = std::find_if_not(std::next(s), tok_end, is_digit);
        }

        if (*s == 'e' || *s == 'E')
        {
            const auto exponent = s;
            ++s;
            has_exponent = true;
            radix = 10;
            if (*s == '+' || *s == '-')
                ++s; // optional sign
            const auto digs_start = s;
            s = std::find_if_not(s, tok_end, is_digit);
            if (digs_start == s)
            {
                report(scanner, exponent, tok_loc, tok_begin,
                       "exponent needs a sequence of digits");
                has_error = true;
                return;
            }
        }
    };

    if (*s != '0')
    {
        radix = 10;
        s = std::find_if_not(s, tok_end, is_digit);
        if (s != tok_end)
            parse_possible_period_or_exponent();
    }
    else
    {
        ++s;
        if (*s == 'x' || *s == 'X')
        {
            radix = 16;
            std::advance(digit_begin, 2);
            s = std::find_if_not(std::next(s), tok_end, is_hexdigit);

            if (s[0] == '.')
            {
                has_period = true;
                s = std::find_if_not(std::next(s), tok_end, is_hexdigit);
            }

            if (s[0] == 'p' || s[0] == 'P')
            {
                const auto exponent = s;
                ++s;
                has_exponent = true;
                if (s[0] == '+' || s[0] == '-')
                    ++s; // optional sign
                const auto digs_start = s;
                s = std::find_if_not(s, tok_end, is_digit);
                if (digs_start == s)
                {
                    report(scanner, exponent, tok_loc, tok_begin,
                           "exponent needs a sequence of digits");
                    has_error = true;
                }
            }
            else if (has_period)
            {
                report(scanner, tok_begin, tok_loc, tok_begin,
                       "hexadecimal floating constant is missing the binary "
                       "exponent");
                has_error = true;
            }
        }
        else
        {
            radix = 8;
            ++digit_begin; // Skips leading zero for later parsing.
            s = std::find_if_not(s, tok_end, is_octdigit);
            if (s != tok_end)
            {
                if (is_digit(*s))
                {
                    const char *dec_end =
                        std::find_if_not(s, tok_end, is_digit);
                    if (*dec_end == '.' || *dec_end == 'e' || *dec_end == 'E')
                    {
                        s = dec_end;
                        radix = 10;
                    }
                }
                parse_possible_period_or_exponent();
            }
        }
    }

    if (has_error)
        return;

    digit_end = s;
    const bool is_fp = is_floating_literal();

    if (radix == 8 && digit_end - digit_begin == 0)
        radix = 10; // This is just a 0 literal.

    // Parses suffix.
    for (; s != tok_end; ++s)
    {
        switch (*s)
        {
            case 'u':
            case 'U':
                if (is_fp || is_unsigned)
                    break; // Repeated u or U suffix.
                is_unsigned = true;
                continue;
            case 'l':
            case 'L':
                if (is_long || is_long_long)
                    break; // Repeated l or L suffix.
                if (s[1] == s[0])
                {
                    if (is_fp)
                        break; // Invalid ll or LL suffix for floating constants.
                    ++s;
                    is_long_long = true;
                }
                else
                    is_long = true;
                continue;
            case 'f':
            case 'F':
                if (!is_fp || is_float)
                    break; // Repeated f or F suffix.
                is_float = true;
                continue;
            default: break; // Unknown suffix.
        }

        // Getting here means the suffix is invalid. Breaks the loop so the
        // error can be reported.
        break;
    }

    // If it hasn't maximal-munched, then the suffix is invalid.
    if (s != tok_end)
    {
        report(scanner, s, tok_loc, tok_begin,
               fmt::format("invalid suffix '{0}' for "
                           "{1:decimal|octal|hexadecimal} "
                           "{2:floating point|integer} constant",
                           std::string_view(digit_end, tok_end - digit_end),
                           select_radix(radix), diag::select(is_fp ? 0 : 1)));
        has_error = true;
    }
}

// In order to know how many digits are enough to guarantee that overflow won't
// happen to an integer literal, we need to know the number base and bits of
// the storage.  Take for instance an integer of 64 bits represented in binary
// (i.e. number base 2): it needs at least more than 64 digits to overflow,
// because the maximum number represented in binary that is limited to 64 bits
// has all bits turned on. One more bit would overflow it. The same principle
// applies to other number bases. Using the same example, but in hexadecimal,
// it takes more than 64/4 digits to overflow, because one digit is a group of
// 4 bits, so it is natural to divide the bits by 4. But how is it done for
// integer literals with number base 10?
//
// The math behind this idea is rather simple: given the maximum number
// representable (which is given by 2^N, where N is number of bits), and the
// number base, we need to find some other number that relates to these two.
// Assuming a function f(N, B), where N is number of bits, and B is the number
// base, should return the number of digits that can fit the maximum number
// representable. If f(2^64, 2) results in 64, and f(2^64, 4) results in 16,
// then it makes sense that f should be `log`. So, log(2^64, 10) should return
// something like 19.26, which we can round to 19. So, 19 digits are enough to
// represent the maximum number in base 10 in 64 bits. More than that and we
// are able to overflow it.
static auto integer_fits_into_64bits(int32_t num_of_digits, int32_t radix)
    -> bool
{
    switch (radix)
    {
        // Base 2 doesn't exist, as C11 doesn't support binary notation.
        case 8: return num_of_digits <= 64 / 3; // log(2^64, 8)
        case 10: return num_of_digits <= 19; // log(2^64, 10)
        case 16: return num_of_digits <= 64 / 4; // log(2^64, 16)
        default: cci_unreachable();
    }
}

auto NumericConstantParser::to_integer() const -> std::pair<uint64_t, bool>
{
    cci_expects(is_integer_literal());
    cci_expects(radix == 8 || radix == 10 || radix == 16);

    uint64_t value = 0;

    bool overflowed = false;
    ptrdiff_t num_of_digits = digit_end - digit_begin;

    if (integer_fits_into_64bits(num_of_digits, radix))
    {
        for (auto it = digit_begin; it != digit_end; ++it)
            value = value * radix + hexdigit_value(*it);
    }
    else
    {
        for (auto it = digit_begin; it != digit_end; ++it)
        {
            uint64_t old_val = value;
            value *= radix;
            overflowed |= (value / radix) != old_val;

            old_val = value;
            value += hexdigit_value(*it);
            overflowed |= value < old_val;
        }
    }

    return {value, overflowed};
}

// Reads a UCN escape value and sets it to `*code_point`. Returns true
// on success.
//
// This function reads similar to the one used in the main scanning phase. So
// here's some homework:
// TODO: Merge this function with scanner's, and put it in a dedicated API.
static auto parse_ucn_escape(Scanner &scan, srcmap::ByteLoc tok_loc,
                             const char *tok_begin, const char *&tok_ptr,
                             const char *tok_end, uint32_t *code_point) -> bool
{
    cci_expects(tok_ptr[0] == '\\' && (tok_ptr[1] == 'u' || tok_ptr[1] == 'U'));
    int num_hexdigits = tok_ptr[1] == 'u' ? 4 : 8;
    const char *escape_begin = tok_ptr;
    tok_ptr += 2; // Skips \u or \U.
    *code_point = 0;

    if (tok_ptr == tok_end || !is_hexdigit(*tok_ptr))
    {
        report(scan, escape_begin, tok_loc, tok_begin,
               "universal character name escape has no hexadecimal digits");
        return false;
    }

    int num_countdown = num_hexdigits;
    for (; tok_ptr != tok_end && num_countdown != 0; --num_countdown)
    {
        uint32_t val = hexdigit_value(*tok_ptr);
        if (val == -1U)
            break;
        *code_point <<= 4;
        *code_point += val;
        ++tok_ptr;
    }

    // If we haven't consumed either 4 or 8 digits, then we assume that this UCN
    // is missing digits, therefore is invalid.
    if (num_countdown)
    {
        report(scan, escape_begin, tok_loc, tok_begin,
               "invalid universal character name");
        return false;
    }

    else if ((*code_point >= 0xD800 &&
              *code_point <= 0xDFFF) || // high and low surrogates
             *code_point > 0x10FFFF) // maximum UTF-32 code point
    {
        report(scan, escape_begin, tok_loc, tok_begin,
               "invalid universal character name");
        return false;
    }

    return true;
}

// Encodes the code point of a UCN into the character or string literal buffer
// `*result_buf`.
static void encode_ucn_to_buffer(uint32_t ucn_val, char **result_buf,
                                 size_t char_byte_width)
{
    cci_expects(char_byte_width == 4 || char_byte_width == 2 ||
                char_byte_width == 1);

    const auto code_point = static_cast<uni::UTF32>(ucn_val);

    if (char_byte_width == 4)
    {
        std::memcpy(*result_buf, &code_point, sizeof(code_point));
        *result_buf += sizeof(code_point);
        return;
    }

    const uni::UTF32 *ucn_start = &code_point;
    const uni::UTF32 *ucn_end = ucn_start + 1;

    if (char_byte_width == 2)
    {
        // Copies UTF-16 code point directly if it fits in 2 bytes.
        if (code_point <= 0xFFFF)
        {
            std::memcpy(*result_buf, &code_point, sizeof(uni::UTF16));
            *result_buf += sizeof(uni::UTF16);
            return;
        }

        // Encodes high and low UTF-16 surrogates.
        uni::UTF32 cp = code_point - 0x10000;
        uni::UTF16 high = 0xD800 + (cp >> 10); // division
        uni::UTF16 low = 0xDC00 + (cp & 0x3FF); // remainder
        std::memcpy(*result_buf, &high, sizeof(high));
        *result_buf += sizeof(high);
        std::memcpy(*result_buf, &low, sizeof(low));
        *result_buf += sizeof(low);
    }
    else
    {
        cci_expects(char_byte_width == 1);
        [[maybe_unused]] uni::ConversionResult res = uni::convert_utf32_to_utf8(
            &ucn_start, ucn_end, reinterpret_cast<uni::UTF8 **>(result_buf),
            reinterpret_cast<uni::UTF8 *>(*result_buf + sizeof(code_point)),
            uni::strictConversion);
        cci_expects(res == uni::conversionOK);
    }
}

// Returns the value representation of an escape sequence.
static auto parse_escape_sequence(Scanner &scan, srcmap::ByteLoc tok_loc,
                                  const char *tok_begin, const char *&tok_ptr,
                                  const char *tok_end, size_t char_width,
                                  bool *has_error) -> uint32_t
{
    auto escape_begin = tok_ptr;

    // Skips '\' slash.
    ++tok_ptr;

    uint32_t result = *tok_ptr++;

    // http://en.cppreference.com/w/c/language/escape
    switch (result)
    {
        case '\'':
        case '"':
        case '?':
        case '\\': break;
        case 'a': // audible bell
            result = 0x07;
            break;
        case 'b': // backspace
            result = 0x08;
            break;
        case 'f': // form feed
            result = 0x0c;
            break;
        case 'n': // line feed
            result = 0x0a;
            break;
        case 'r': // carriage return
            result = 0x0d;
            break;
        case 't': // horizontal tab
            result = 0x09;
            break;
        case 'v': // vertical tab
            result = 0x0b;
            break;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        {
            result -= '0';
            int num_digits = 1;
            while (tok_ptr != tok_end && num_digits < 3 &&
                   is_octdigit(*tok_ptr))
            {
                result <<= 3;
                result += *tok_ptr++ - '0';
                ++num_digits;
            }

            // Checks whether this octal escape fits in a character literal
            // whose width is less than 32 bits. L'\777' should fit, whereas
            // '\777' should not.
            if (char_width < 32 && (result >> char_width) != 0)
            {
                report(scan, escape_begin, tok_loc, tok_begin,
                       "octal escape sequence out of range");
                *has_error = true;
                // Truncate the result.
                result &= ~0U >> (32 - char_width);
            }

            break;
        }
        case 'x':
        {
            result = 0;
            if (tok_ptr == tok_end || !is_hexdigit(*tok_ptr))
            {
                report(scan, escape_begin, tok_loc, tok_begin,
                       "hexadecimal escape sequence has no digits");
                *has_error = true;
                break;
            }

            bool overflowed = false;

            for (; tok_ptr != tok_end; ++tok_ptr)
            {
                auto val = hexdigit_value(*tok_ptr);
                if (val == -1U)
                    break;
                // Shifting out some existing bits from the result means it is
                // about to overflow.
                if (result & 0xF0000000)
                    overflowed = true;
                result <<= 4;
                result += val;
            }

            // Checks whether this hex escape fits in a character literal whose
            // width is less than 32 bits. L'\xFFFF' should fit, whereas
            // '\xFFFF' should not.
            if (char_width < 32 && (result >> char_width) != 0)
            {
                overflowed = true;
                // Truncate the result.
                result &= ~0U >> (32 - char_width);
            }

            if (overflowed)
            {
                report(scan, escape_begin, tok_loc, tok_begin,
                       "hex escape sequence out of range");
                *has_error = true;
            }

            break;
        }
        default:
            report(scan, escape_begin, tok_loc, tok_begin,
                   "unknown escape sequence");
            *has_error = true;
    }

    return result;
}

CharConstantParser::CharConstantParser(Scanner &scanner,
                                       std::string_view tok_lexeme,
                                       srcmap::ByteLoc tok_loc,
                                       Category char_category,
                                       const TargetInfo &target)
    : value(0), category(char_category)
{
    cci_expects(is_char_constant(char_category));
    cci_expects(tok_lexeme.end()[0] == '\0');
    const char *const tok_begin = tok_lexeme.begin();
    const char *tok_end = tok_lexeme.end();
    const char *tok_ptr = tok_begin;
    auto &diag = scanner.diagnostics();

    // Skips either L, u or U.
    if (char_category != Category::char_constant)
    {
        cci_expects(*tok_ptr == 'L' || *tok_ptr == 'u' || *tok_ptr == 'U');
        ++tok_ptr;
    }

    cci_expects(*tok_ptr == '\'');
    ++tok_ptr; // Skips starting quote.

    --tok_end; // Trims ending quote.
    cci_expects(*tok_end == '\'');

    size_t char_byte_width = map_char_width(category, target);
    // Assumes that the char width for this target is a multiple of 8.
    cci_expects((target.char_width & 0b0111) == 0);
    char_byte_width /= 8;

    // Sometimes Unicode characters can't be represented in a single code unit,
    // so this constant represents the maximum code point a character constant
    // may hold, depending on its category. Code points bigger than
    // `largest_value_for_category` results in an error.
    const uint32_t largest_value_for_category = [&] {
        if (char_category == Category::char_constant ||
            char_category == Category::utf8_char_constant)
            return 0x7F;
        if (char_category == Category::utf16_char_constant)
            return 0xFFFF;
        cci_expects(char_category == Category::utf32_char_constant);
        return 0x10FFFFFF;
    }();

    small_vector<uint32_t, 4> codepoint_buffer;
    codepoint_buffer.resize(static_cast<size_t>(tok_end - tok_ptr));
    uint32_t *buf_begin = &codepoint_buffer.front();
    uint32_t *buf_end = buf_begin + codepoint_buffer.size();

    while (tok_ptr != tok_end)
    {
        if (*tok_ptr != '\\')
        {
            const char *chunk_start = tok_ptr;
            const char *chunk_end = std::find_if(
                tok_ptr, tok_end, [](char c) { return c == '\\'; });

            uint32_t *save_buf_begin = buf_begin;
            uni::ConversionResult res = uni::convert_utf8_to_utf32(
                reinterpret_cast<const uni::UTF8 **>(&tok_ptr),
                reinterpret_cast<const uni::UTF8 *>(chunk_end), &buf_begin,
                buf_end, uni::strictConversion);

            if (res == uni::conversionOK)
            {
                for (; save_buf_begin != buf_begin; ++save_buf_begin)
                {
                    if (*save_buf_begin > largest_value_for_category)
                    {
                        diag.report(
                            tok_loc,
                            "unicode character is too large, and can't be "
                            "represented in a single code unit");
                        has_error = true;
                        break;
                    }
                }
            }
            else
            {
                // Both Clang and GCC just copy the char literal's raw source in
                // case of bad unicode encoding, so do the same to remain
                // compatible with them.
                std::memcpy(buf_begin, chunk_start,
                            static_cast<size_t>(chunk_end - chunk_start));
                tok_ptr = chunk_end;
                ++buf_begin;
            }
        }
        else if (tok_ptr[1] == 'u' || tok_ptr[1] == 'U')
        {
            if (!parse_ucn_escape(scanner, tok_loc, tok_begin, tok_ptr, tok_end,
                                  buf_begin))
                has_error = true;
            else if (*buf_begin > largest_value_for_category)
            {
                diag.report(
                    tok_loc,
                    "unicode character is too large, and can't be represented "
                    "in a single code unit");
                has_error = true;
            }
            ++buf_begin;
        }
        else
        {
            *buf_begin++ =
                parse_escape_sequence(scanner, tok_loc, tok_begin, tok_ptr,
                                      tok_end, char_byte_width * 8, &has_error);
        }
    }

    cci_expects(*tok_ptr == '\'');

    codepoint_buffer.resize(
        static_cast<size_t>(buf_begin - &codepoint_buffer.front()));

    if (codepoint_buffer.empty())
    {
        diag.report(tok_loc, "character constant is empty");
        has_error = true;
        return;
    }

    const size_t num_of_chars = codepoint_buffer.size();
    is_multibyte = num_of_chars > 1;
    uint32_t result_value = 0;

    if (category == Category::char_constant && is_multibyte)
    {
        cci_expects(char_byte_width == 1);
        bool overflowed = false;
        for (uint32_t cp : codepoint_buffer)
        {
            if (result_value & 0xFF000000)
                overflowed = true;
            result_value <<= 8;
            result_value |= cp & 0xFF;
        }

        if (overflowed)
        {
            diag.report(tok_loc, "character constant is too large");
            has_error = true;
        }
    }
    else
    {
        // In case of multiple multibyte characters for non ASCII character
        // literals, use the last multibyte character to remain compatible with
        // GCC's and Clang's behavior.
        result_value = codepoint_buffer.back();
    }

    // [C11 6.4.4.4p13 EXAMPLE 2]
    // In an implementation in which type char has the same range of values as
    // signed char, the integer character constant '\xFF' has the value -1; if
    // type char has the same range of values as unsigned char, the character
    // constant
    // '\xFF' has the value +255.
    if (category == Category::char_constant && num_of_chars == 1 &&
        target.is_char_signed)
        result_value = static_cast<signed char>(result_value);

    this->value = result_value;
}

StringLiteralParser::StringLiteralParser(Scanner &scanner,
                                         span<const Token> string_toks,
                                         const TargetInfo &target)
{
    auto &diag = scanner.diagnostics();

    // The following code calculates a size bound that is the sum of all
    // strings' size for the result buffer, which is a bit over enough, given
    // that trigraphs and escape sequences, after processed, shrink the string
    // size.

    cci_expects(!string_toks.empty());
    cci_expects(is_string_literal(string_toks[0].category()));
    cci_expects(string_toks[0].size() >= 2);
    size_t size_bound = string_toks[0].size() - 2; // removes ""

    // Holds the size of the biggest string literal. This is used to allocate
    // memory for a temporary buffer to hold the processed strings.
    size_t max_token_size = size_bound;

    cci_expects(is_string_literal(string_toks[0].category()));
    category = string_toks[0].category();

    // Performs [C11 5.1.1.2p6]: Adjacent string literal tokens are concatenated.
    for (ptrdiff_t i = 1; i != string_toks.size(); ++i)
    {
        cci_expects(is_string_literal(string_toks[i].category()));
        if (string_toks[i].is_not(category) &&
            string_toks[i].is_not(Category::string_literal))
        {
            if (category == Category::string_literal)
                category = string_toks[i].category();
            else
            {
                // Concatenation of different categories of strings could be
                // supported, but that would not be standard compliant, except
                // if we take into account [C11 6.4.5p5]:
                //
                //    […] Whether differently-prefixed wide string literal
                //    tokens can be concatenated and, if so, the treatment of
                //    the resulting multibyte character sequence are
                //    implementation-defined.
                //
                // Which means wide string literals (ones prefixed with L, u or
                // U) could be concatenated.
                diag.report(
                    string_toks[i].location(),
                    "concatenation of different categories of strings is not "
                    "standard compliant");
                has_error = true;
            }
        }

        cci_expects(string_toks[i].size() >= 2);
        size_bound += string_toks[i].size() - 2; // removes ""
        max_token_size = std::max(max_token_size, string_toks[i].size() - 2);
    }

    // Allows an space for the null terminator.
    ++size_bound;

    char_byte_width = map_char_width(category, target);
    // Assumes that the char width for this target is a multiple of 8.
    cci_expects((target.char_width & 0b0111) == 0);
    char_byte_width /= 8;

    // More space is needed if we have wide/unicode strings literals.
    size_bound *= char_byte_width;

    // Token spellings are written to this buffer.
    small_string<256> token_buf;

    this->result_buf.resize(size_bound);
    token_buf.resize(max_token_size);

    // In case we get an empty string literal, there's nothing left to be done.
    if (size_bound == 0)
        return;

    this->result_ptr = this->result_buf.data();
    [[maybe_unused]] const char *result_buf_start = this->result_buf.data();

    for (const auto &string_tok : string_toks)
    {
        // Gets the token spelling, and writes it to `token_buf`. There's no
        // need to clear the buffer, since we know the amount of bytes that is
        // written to the buffer, which is enough to form a range of iterators.
        const char *tokbuf_ptr = token_buf.data();
        const size_t tok_length = Scanner::get_spelling_to_buffer(
            string_tok, token_buf.data(), scanner.source_map());

        const char *tokbuf_begin = tokbuf_ptr;
        const char *tokbuf_end = tokbuf_begin + tok_length;

        // Skips u, U, or L.
        if (string_tok.is_not(Category::string_literal))
        {
            ++tokbuf_ptr;
            // Skips 8 from u8.
            if (string_tok.is(Category::utf8_string_literal))
                ++tokbuf_ptr;
        }

        cci_expects(*tokbuf_ptr == '"');
        ++tokbuf_ptr;

        --tokbuf_end;
        cci_expects(*tokbuf_end == '"');

        while (tokbuf_ptr != tokbuf_end)
        {
            if (*tokbuf_ptr != '\\')
            {
                // This is possibly a sequence of ASCII or UTF-8 characters.
                const char *chunk_start = tokbuf_ptr;
                const char *chunk_end = std::find_if(
                    tokbuf_ptr, tokbuf_end, [](char c) { return c == '\\'; });

                uni::ConversionResult res = [&] {
                    if (char_byte_width == 4)
                    {
                        return uni::convert_utf8_to_utf32(
                            reinterpret_cast<const uni::UTF8 **>(&chunk_start),
                            reinterpret_cast<const uni::UTF8 *>(chunk_end),
                            reinterpret_cast<uni::UTF32 **>(&this->result_ptr),
                            reinterpret_cast<uni::UTF32 *>(
                                this->result_buf.data() +
                                this->result_buf.size()),
                            uni::strictConversion);
                    }
                    else if (char_byte_width == 2)
                    {
                        return uni::convert_utf8_to_utf16(
                            reinterpret_cast<const uni::UTF8 **>(&chunk_start),
                            reinterpret_cast<const uni::UTF8 *>(chunk_end),
                            reinterpret_cast<uni::UTF16 **>(&this->result_ptr),
                            reinterpret_cast<uni::UTF16 *>(
                                this->result_buf.data() +
                                this->result_buf.size()),
                            uni::strictConversion);
                    }

                    cci_expects(char_byte_width == 1);
                    this->result_ptr =
                        std::copy(chunk_start, chunk_end, this->result_ptr);
                    chunk_start = chunk_end;
                    return uni::conversionOK;
                }();

                if (res == uni::conversionOK)
                {
                    cci_ensures(chunk_start == chunk_end);
                    tokbuf_ptr = chunk_start;
                }
                else
                {
                    // If we're parsing an ASCII string literal, then copy the
                    // string chunk regardless of bad encoding.
                    if (string_tok.is(Category::string_literal))
                    {
                        this->result_ptr =
                            std::copy(tokbuf_ptr, chunk_end, this->result_ptr);
                        tokbuf_ptr = chunk_start;
                    }
                    has_error = true;
                }
            }
            else if (tokbuf_ptr[1] == 'u' || tokbuf_ptr[1] == 'U')
            {
                uint32_t code_point = 0;
                if (parse_ucn_escape(scanner, string_tok.location(),
                                     tokbuf_begin, tokbuf_ptr, tokbuf_end,
                                     &code_point))
                    encode_ucn_to_buffer(code_point, &this->result_ptr,
                                         char_byte_width);
                else
                    has_error = true;
            }
            else
            {
                // If it's not a UCN, then it's a normal escape sequence.
                uint32_t result_char = parse_escape_sequence(
                    scanner, string_tok.location(), tokbuf_begin, tokbuf_ptr,
                    tokbuf_end, char_byte_width * 8, &has_error);

                if (char_byte_width == 4)
                {
                    const auto result_wide =
                        static_cast<uni::UTF32>(result_char);
                    std::memcpy(this->result_ptr, &result_wide,
                                sizeof(result_wide));
                    this->result_ptr += sizeof(result_wide);
                }
                else if (char_byte_width == 2)
                {
                    const auto result_wide =
                        static_cast<uni::UTF16>(result_char & 0xFFFF);
                    std::memcpy(this->result_ptr, &result_wide,
                                sizeof(result_wide));
                    this->result_ptr += sizeof(result_wide);
                }
                else
                {
                    cci_expects(char_byte_width == 1);
                    *this->result_ptr++ = result_char & 0xFF;
                }
            }
        }
    }

    cci_ensures(result_buf_start == this->result_buf.data());
    cci_ensures(this->result_ptr >= this->result_buf.data() &&
                this->result_ptr <=
                    this->result_buf.data() + this->result_buf.size());
}
