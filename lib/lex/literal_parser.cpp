#include "cci/lex/literal_parser.hpp"
#include "./lex_diagnostics.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/langopts.hpp"
#include "cci/lex/char_info.hpp"
#include "cci/lex/lexer.hpp"
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
static auto select_radix(uint32_t radix) -> selector
{
  return selector{
    radix == 10 ? 0 : radix == 8 ? 1 : radix == 16 ? 2 : cci_unreachable()};
}

template <typename... Args>
static void report(Lexer &lex, const char *char_ptr, SourceLocation tok_loc,
                   const char *tok_begin, diag::Lex err_code, Args &&... args)
{
  auto &diag = lex.diagnostics();
  diag.report(lex.character_location(tok_loc, tok_begin, char_ptr), err_code,
              std::forward<Args>(args)...);
}

// Returns the respective character type width for a given string literal or
// character constant token.
static auto map_char_width(TokenKind kind, const TargetInfo &target)
  -> size_t
{
  cci_expects(is_char_constant(kind) || is_string_literal(kind));
  switch (kind)
  {
    case TokenKind::char_constant:
    case TokenKind::string_literal:
    case TokenKind::utf8_char_constant:
    case TokenKind::utf8_string_literal: return target.char_width;
    case TokenKind::wide_char_constant:
    case TokenKind::wide_string_literal: return target.wchar_width;
    case TokenKind::utf16_char_constant:
    case TokenKind::utf16_string_literal: return target.char16_t_width;
    case TokenKind::utf32_char_constant:
    case TokenKind::utf32_string_literal: return target.char32_t_width;
    default: cci_unreachable();
  }
}

NumericConstantParser::NumericConstantParser(Lexer &lexer,
                                             std::string_view tok_spelling,
                                             SourceLocation tok_loc)
{
  cci_expects(tok_spelling.end()[0] == '\0');
  const char *const tok_begin = tok_spelling.begin();
  const char *const tok_end = tok_spelling.end();
  const char *s = tok_begin;
  digit_begin = tok_begin;

  auto parse_possible_period_or_exponent = [&, this] {
    cci_expects(radix == 10 || radix == 8);

    if (is_hexdigit(s[0]) && *s != 'e' && *s != 'E')
    {
      report(lexer, s, tok_loc, tok_begin, diag::err_invalid_digit, *s,
             select_radix(radix), selector{1});
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
        report(lexer, exponent, tok_loc, tok_begin, diag::err_empty_exponent);
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
          report(lexer, exponent, tok_loc, tok_begin, diag::err_empty_exponent);
          has_error = true;
        }
      }
      else if (has_period)
      {
        report(lexer, tok_begin, tok_loc, tok_begin,
               diag::err_missing_exponent);
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
          const char *dec_end = std::find_if_not(s, tok_end, is_digit);
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

    // Getting here means the suffix is invalid. Breaks the loop so the error
    // can be reported.
    break;
  }

  // If it hasn't maximal-munched, then the suffix is invalid.
  if (s != tok_end)
  {
    report(lexer, s, tok_loc, tok_begin, diag::err_invalid_suffix,
           std::string_view(digit_end, tok_end - digit_end),
           select_radix(radix), selector{is_fp ? 0 : 1});
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

auto NumericConstantParser::to_integer() const
  -> std::pair<uint64_t, bool>
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
// This function reads similar to the one used in the main lexing phase. So
// here's some homework:
// TODO: Merge this function with lexer's, and put it in a dedicated API.
static auto parse_ucn_escape(Lexer &lex, SourceLocation tok_loc,
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
    report(lex, escape_begin, tok_loc, tok_begin, diag::err_ucn_no_hexdigits);
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
    report(lex, escape_begin, tok_loc, tok_begin, diag::err_ucn_invalid);
    return false;
  }

  else if ((*code_point >= 0xD800 &&
            *code_point <= 0xDFFF) || // high and low surrogates
           *code_point > 0x10FFFF) // maximum UTF-32 code point
  {
    report(lex, escape_begin, tok_loc, tok_begin, diag::err_ucn_invalid);
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
static auto parse_escape_sequence(Lexer &lex, SourceLocation tok_loc,
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
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    {
      result -= '0';
      int num_digits = 1;
      while (tok_ptr != tok_end && num_digits < 3 && is_octdigit(*tok_ptr))
      {
        result <<= 3;
        result += *tok_ptr++ - '0';
        ++num_digits;
      }

      // Checks whether this octal escape fits in a character literal whose
      // width is less than 32 bits. L'\777' should fit, whereas '\777' should
      // not.
      if (char_width < 32 && (result >> char_width) != 0)
      {
        report(lex, escape_begin, tok_loc, tok_begin,
               diag::err_escape_sequence_too_large, selector{0});
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
        report(lex, escape_begin, tok_loc, tok_begin,
               diag::err_hex_escape_is_empty);
        *has_error = true;
        break;
      }

      bool overflowed = false;

      for (; tok_ptr != tok_end; ++tok_ptr)
      {
        auto val = hexdigit_value(*tok_ptr);
        if (val == -1U)
          break;
        // Shifting out some existing bits from the result means it is about to
        // overflow.
        if (result & 0xF0000000)
          overflowed = true;
        result <<= 4;
        result += val;
      }

      // Checks whether this hex escape fits in a character literal whose width
      // is less than 32 bits. L'\xFFFF' should fit, whereas '\xFFFF' should not.
      if (char_width < 32 && (result >> char_width) != 0)
      {
        overflowed = true;
        // Truncate the result.
        result &= ~0U >> (32 - char_width);
      }

      if (overflowed)
      {
        report(lex, escape_begin, tok_loc, tok_begin,
               diag::err_escape_sequence_too_large, selector{1});
        *has_error = true;
      }

      break;
    }
    default:
      report(lex, escape_begin, tok_loc, tok_begin,
             diag::err_unknown_escape_sequence);
      *has_error = true;
  }

  return result;
}

CharConstantParser::CharConstantParser(Lexer &lexer,
                                       std::string_view tok_spelling,
                                       SourceLocation tok_loc,
                                       TokenKind char_kind,
                                       const TargetInfo &target)
  : value(0), kind(char_kind)
{
  cci_expects(is_char_constant(char_kind));
  cci_expects(tok_spelling.end()[0] == '\0');
  const char *const tok_begin = tok_spelling.begin();
  const char *tok_end = tok_spelling.end();
  const char *tok_ptr = tok_begin;
  auto &diag = lexer.diagnostics();

  // Skips either L, u or U.
  if (char_kind != TokenKind::char_constant)
  {
    cci_expects(*tok_ptr == 'L' || *tok_ptr == 'u' || *tok_ptr == 'U');
    ++tok_ptr;
  }

  cci_expects(*tok_ptr == '\'');
  ++tok_ptr; // Skips starting quote.

  --tok_end; // Trims ending quote.
  cci_expects(*tok_end == '\'');

  size_t char_byte_width = map_char_width(kind, target);
  // Assumes that the char width for this target is a multiple of 8.
  cci_expects((target.char_width & 0b0111) == 0);
  char_byte_width /= 8;

  // Sometimes Unicode characters can't be represented in a single code unit, so
  // this constant represents the maximum code point a character constant may
  // hold, depending on its kind. Code points bigger than
  // `largest_value_for_kind` results in an error.
  const uint32_t largest_value_for_kind = [&] {
    if (char_kind == TokenKind::char_constant ||
        char_kind == TokenKind::utf8_char_constant)
      return 0x7F;
    if (char_kind == TokenKind::utf16_char_constant)
      return 0xFFFF;
    cci_expects(char_kind == TokenKind::utf32_char_constant);
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
      const char *chunk_end =
        std::find_if(tok_ptr, tok_end, [](char c) { return c == '\\'; });

      uint32_t *save_buf_begin = buf_begin;
      uni::ConversionResult res = uni::convert_utf8_to_utf32(
        reinterpret_cast<const uni::UTF8 **>(&tok_ptr),
        reinterpret_cast<const uni::UTF8 *>(chunk_end), &buf_begin, buf_end,
        uni::strictConversion);

      if (res == uni::conversionOK)
      {
        for (; save_buf_begin != buf_begin; ++save_buf_begin)
        {
          if (*save_buf_begin > largest_value_for_kind)
          {
            diag.report(tok_loc, diag::err_unicode_character_too_large);
            has_error = true;
            break;
          }
        }
      }
      else
      {
        // Both Clang and GCC just copy the char literal's raw source in case of
        // bad unicode encoding, so do the same to remain compatible with them.
        std::memcpy(buf_begin, chunk_start,
                    static_cast<size_t>(chunk_end - chunk_start));
        tok_ptr = chunk_end;
        ++buf_begin;
      }
    }
    else if (tok_ptr[1] == 'u' || tok_ptr[1] == 'U')
    {
      if (!parse_ucn_escape(lexer, tok_loc, tok_begin, tok_ptr, tok_end,
                            buf_begin))
        has_error = true;
      else if (*buf_begin > largest_value_for_kind)
      {
        diag.report(tok_loc, diag::err_unicode_character_too_large);
        has_error = true;
      }
      ++buf_begin;
    }
    else
    {
      *buf_begin++ =
        parse_escape_sequence(lexer, tok_loc, tok_begin, tok_ptr, tok_end,
                              char_byte_width * 8, &has_error);
    }
  }

  cci_expects(*tok_ptr == '\'');

  codepoint_buffer.resize(
    static_cast<size_t>(buf_begin - &codepoint_buffer.front()));

  if (codepoint_buffer.empty())
  {
    diag.report(tok_loc, diag::err_empty_character);
    has_error = true;
    return;
  }

  const size_t num_of_chars = codepoint_buffer.size();
  is_multibyte = num_of_chars > 1;
  uint32_t result_value = 0;

  if (kind == TokenKind::char_constant && is_multibyte)
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
      diag.report(tok_loc, diag::err_char_constant_too_large);
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
  if (kind == TokenKind::char_constant && num_of_chars == 1 &&
      target.is_char_signed)
    result_value = static_cast<signed char>(result_value);

  this->value = result_value;
}

StringLiteralParser::StringLiteralParser(Lexer &lexer,
                                         span<const Token> string_toks,
                                         const TargetInfo &target)
{
  auto &diag = lexer.diagnostics();

  // The following code calculates a size bound that is the sum of all strings'
  // size for the result buffer, which is a bit over enough, given that
  // trigraphs and escape sequences, after processed, shrink the string size.

  cci_expects(!string_toks.empty());
  cci_expects(is_string_literal(string_toks[0].kind));
  cci_expects(string_toks[0].size() >= 2);
  size_t size_bound = string_toks[0].size() - 2; // removes ""

  // Holds the size of the biggest string literal. This is used to allocate
  // memory for a temporary buffer to hold the processed strings.
  size_t max_token_size = size_bound;

  cci_expects(is_string_literal(string_toks[0].kind));
  kind = string_toks[0].kind;

  // Performs [C11 5.1.1.2p6]: Adjacent string literal tokens are concatenated.
  for (ptrdiff_t i = 1; i != string_toks.size(); ++i)
  {
    cci_expects(is_string_literal(string_toks[i].kind));
    if (string_toks[i].is_not(kind) &&
        string_toks[i].is_not(TokenKind::string_literal))
    {
      if (kind == TokenKind::string_literal)
        kind = string_toks[i].kind;
      else
      {
        // Concatenation of different kinds of strings could be supported, but
        // that would not be standard compliant, except if we take into account
        // [C11 6.4.5p5]:
        //
        //    […] Whether differently-prefixed wide string literal tokens can
        //    be concatenated and, if so, the treatment of the resulting
        //    multibyte character sequence are implementation-defined.
        //
        // Which means wide string literals (ones prefixed with L, u or U)
        // could be concatenated.
        diag.report(string_toks[i].location(),
                    diag::err_nonstd_string_concatenation);
        has_error = true;
      }
    }

    cci_expects(string_toks[i].size() >= 2);
    size_bound += string_toks[i].size() - 2; // removes ""
    max_token_size = std::max(max_token_size, string_toks[i].size() - 2);
  }

  // Allows an space for the null terminator.
  ++size_bound;

  char_byte_width = map_char_width(kind, target);
  // Assumes that the char width for this target is a multiple of 8.
  cci_expects((target.char_width & 0b0111) == 0);
  char_byte_width /= 8;

  // More space is needed if we have wide/unicode strings literals.
  size_bound *= char_byte_width;

  // Token spellings are written to this buffer.
  small_string<256> token_buf;

  result_buf.resize(size_bound);
  token_buf.resize(max_token_size);

  // In case we get an empty string literal, there's nothing left to be done.
  if (size_bound == 0)
    return;

  this->result_ptr = result_buf.data();

  for (const auto &string_tok : string_toks)
  {
    // Gets the token spelling, and writes it to `token_buf`. There's no need
    // to clear the buffer, since we know the amount of bytes that is written
    // to the buffer, which is enough to form a range of iterators.
    const char *tokbuf_ptr = token_buf.data();
    const size_t tok_length = Lexer::get_spelling_to_buffer(
      string_tok, token_buf.data(), lexer.source_mgr);

    const char *tokbuf_begin = tokbuf_ptr;
    const char *tokbuf_end = tokbuf_begin + tok_length;

    // Skips u, U, or L.
    if (string_tok.is_not(TokenKind::string_literal))
    {
      ++tokbuf_ptr;
      // Skips 8 from u8.
      if (string_tok.is(TokenKind::utf8_string_literal))
        ++tokbuf_ptr;
    }

    // Removes the first quote.
    cci_expects(*tokbuf_ptr == '"');
    ++tokbuf_ptr;

    // Removes the trailing quote.
    --tokbuf_end;
    cci_expects(*tokbuf_end == '"');

    while (tokbuf_ptr != tokbuf_end)
    {
      if (*tokbuf_ptr != '\\')
      {
        // This is possibly a sequence of ASCII or UTF-8 characters.
        const char *chunk_start = tokbuf_ptr;
        const char *chunk_end = std::find_if(tokbuf_ptr, tokbuf_end,
                                             [](char c) { return c == '\\'; });

        uni::ConversionResult res = [&] {
          if (char_byte_width == 4)
          {
            return uni::convert_utf8_to_utf32(
              reinterpret_cast<const uni::UTF8 **>(&chunk_start),
              reinterpret_cast<const uni::UTF8 *>(chunk_end),
              reinterpret_cast<uni::UTF32 **>(&result_ptr),
              reinterpret_cast<uni::UTF32 *>(result_buf.end()),
              uni::strictConversion);
          }
          else if (char_byte_width == 2)
          {
            return uni::convert_utf8_to_utf16(
              reinterpret_cast<const uni::UTF8 **>(&chunk_start),
              reinterpret_cast<const uni::UTF8 *>(chunk_end),
              reinterpret_cast<uni::UTF16 **>(&result_ptr),
              reinterpret_cast<uni::UTF16 *>(result_buf.end()),
              uni::strictConversion);
          }

          cci_expects(char_byte_width == 1);
          result_ptr = std::copy(chunk_start, chunk_end, result_ptr);
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
          // If we're parsing an ASCII string literal, then copy the string
          // chunk regardless of bad encoding.
          if (string_tok.is(TokenKind::string_literal))
          {
            result_ptr = std::copy(tokbuf_ptr, chunk_end, result_ptr);
            tokbuf_ptr = chunk_start;
          }
          has_error = true;
        }
      }
      else if (tokbuf_ptr[1] == 'u' || tokbuf_ptr[1] == 'U')
      {
        uint32_t code_point = 0;
        if (parse_ucn_escape(lexer, string_tok.location(), tokbuf_begin,
                             tokbuf_ptr, tokbuf_end, &code_point))
          encode_ucn_to_buffer(code_point, &result_ptr, char_byte_width);
        else
          has_error = true;
      }
      else
      {
        // If it's not a UCN, then it's a normal escape sequence.
        uint32_t result_char = parse_escape_sequence(
          lexer, string_tok.location(), tokbuf_begin, tokbuf_ptr, tokbuf_end,
          char_byte_width * 8, &has_error);

        if (char_byte_width == 4)
        {
          const auto result_wide = static_cast<uni::UTF32>(result_char);
          std::memcpy(result_ptr, &result_wide, sizeof(result_wide));
          result_ptr += sizeof(result_wide);
        }
        else if (char_byte_width == 2)
        {
          const auto result_wide =
            static_cast<uni::UTF16>(result_char & 0xFFFF);
          std::memcpy(result_ptr, &result_wide, sizeof(result_wide));
          result_ptr += sizeof(result_wide);
        }
        else
        {
          cci_expects(char_byte_width == 1);
          *result_ptr++ = result_char & 0xFF;
        }
      }
    }
  }
}
