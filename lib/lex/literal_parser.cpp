#include "cci/lex/literal_parser.hpp"
#include "./lex_diagnostics.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/langopts.hpp"
#include "cci/lex/lexer.hpp"
#include <algorithm>
#include <climits>
#include <utility>
#include <vector>

using namespace cci;

static auto numeric_constant_name_for_radix(uint32_t radix,
                                            bool is_floating_point)
  -> std::string_view
{
  if (is_floating_point)
    return radix == 10
             ? "floating point"
             : radix == 16 ? "hexadecimal floating point" : cci_unreachable();
  else
    return radix == 10 ? "decimal integer"
                       : radix == 8 ? "octal integer"
                                    : radix == 16 ? "hexadecimal integer"
                                                  : cci_unreachable();
}

template <typename... Args>
static void report(Lexer &lex, const char *char_ptr, SourceLocation tok_loc,
                   const char *tok_begin, diag::Lex err_code, Args &&... args)
{
  auto &diag = lex.diagnostics();
  diag.report(lex.character_location(tok_loc, tok_begin, char_ptr), err_code,
              std::forward<Args>(args)...);
}

static auto resolve_char_width(TokenKind kind, const TargetInfo &target)
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
             numeric_constant_name_for_radix(radix, false));
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
        report(lexer, tok_begin, tok_loc, tok_begin, diag::err_missing_exponent);
        has_error = true;
      }
    }
    else
    {
      radix = 8;
      ++digit_begin;
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

  // Parses suffix.
  for (; s != tok_end; ++s)
  {
    switch (*s)
    {
      case 'u':
      case 'U':
        if (is_fp || is_unsigned)
          break;
        is_unsigned = true;
        continue;
      case 'l':
      case 'L':
        if (is_long || is_long_long)
          break;
        if (s[1] == s[0])
        {
          if (is_fp)
            break;
          ++s;
          is_long_long = true;
        }
        else
          is_long = true;
        continue;
      case 'f':
      case 'F':
        if (!is_fp || is_float)
          break;
        is_float = true;
        continue;
      default: break;
    }

    // Getting here means the suffix is invalid. Breaks the loop so the error
    // can be reported.
    break;
  }

  if (s != tok_end)
  {
    report(lexer, s, tok_loc, tok_begin, diag::err_invalid_suffix,
           std::string_view(digit_end, tok_end - digit_end),
           numeric_constant_name_for_radix(radix, is_fp));
    has_error = true;
  }
}

auto NumericConstantParser::eval_to_integer() -> std::pair<uint64_t, bool>
{
  cci_expects(is_integer_literal());
  cci_expects(radix != 0);

  uint64_t value = 0;
  bool overflowed = false;

  for (auto it = digit_begin; it != digit_end; ++it)
  {
    // FIXME: This overflow check doesn't take smaller units into account, such
    // as signed or unsigned 8, 16, and 32 bits.
    uint64_t old_val = value;
    value *= radix;
    overflowed |= (value / radix) != old_val;

    old_val = value;
    value += hexdigit_value(*it);
    overflowed |= value < old_val;
  }

  return {value, overflowed};
}

static auto evaluate_ucn(Lexer &lex, SourceLocation tok_loc,
                         const char *tok_begin, const char *&tok_ptr,
                         const char *tok_end, bool &has_error) -> uint32_t
{
  cci_expects(tok_ptr[0] == '\\' && (tok_ptr[1] == 'u' || tok_ptr[1] == 'U'));
  int num_hexdigits = tok_ptr[1] == 'u' ? 4 : 8;
  const auto escape_begin = tok_ptr;
  tok_ptr += 2; // Skips \u or \U.
  uint64_t code_point = 0;

  int num_countdown = num_hexdigits;
  for (; tok_ptr != tok_end && num_countdown != 0; --num_countdown)
  {
    uint32_t val = hexdigit_value(*tok_ptr);
    if (val == -1U)
      break;
    code_point <<= 4;
    code_point += val;
    ++tok_ptr;
  }

  // If we haven't consumed either 4 or 8 digits, then we assume that this UCN
  // is missing digits, therefore is invalid.
  if (num_countdown)
  {
    report(lex, escape_begin, tok_loc, tok_begin, diag::err_ucn_invalid);
    has_error = true;
  }

  return code_point;
}

// Returns the value representation of an escape sequence.
static auto evaluate_escape_sequence(Lexer &lex, SourceLocation tok_loc,
                                     const char *tok_begin,
                                     const char *&tok_ptr, const char *tok_end,
                                     bool &has_error) -> uint32_t
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
    case '\\':
      break;
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
      --tok_ptr; // Backs up one digit.
      result = 0;
      int num_digits = 0;
      do
      {
        result <<= 3;
        result += *tok_ptr++ - '0';
        ++num_digits;
      } while (tok_ptr != tok_end && num_digits < 3 && is_octdigit(*tok_ptr));
      break;
    }
    case 'x':
    {
      result = 0;
      if (tok_ptr == tok_end || !is_hexdigit(*tok_ptr))
      {
        report(lex, escape_begin, tok_loc, tok_begin,
               diag::err_hex_escape_is_empty);
        has_error = true;
        break;
      }

      // TODO: Check for overflow.
      for (; tok_ptr != tok_end; ++tok_ptr)
      {
        auto val = hexdigit_value(*tok_ptr);
        if (val == -1U)
          break;
        result <<= 4;
        result += val;
      }
      break;
    }
    default:
      report(lex, escape_begin, tok_loc, tok_begin,
             diag::err_unknown_escape_sequence);
      has_error = true;
  }

  return result;
}

CharConstantParser::CharConstantParser(Lexer &lexer,
                                       std::string_view tok_spelling,
                                       SourceLocation tok_loc,
                                       TokenKind char_kind)
  : kind(char_kind)
{
  cci_expects(is_char_constant(char_kind));
  cci_expects(tok_spelling.end()[0] == '\0');
  const char *const tok_begin = tok_spelling.begin();
  const char *const tok_end = tok_spelling.end();
  const char *tok_ptr = tok_begin;

  // Skips either L, u or U.
  // TODO: Implement different char widths.
  if (char_kind != TokenKind::char_constant)
    ++tok_ptr;

  ++tok_ptr; // Skips '\'' character.

  if (*tok_ptr != '\\')
  {
    value = *tok_ptr++;
  }
  else if (tok_ptr[1] == 'u' || tok_ptr[1] == 'U')
  {
    value =
      evaluate_ucn(lexer, tok_loc, tok_begin, tok_ptr, tok_end, has_error);
  }
  else
  {
    value = evaluate_escape_sequence(lexer, tok_loc, tok_begin, tok_ptr,
                                     tok_end, has_error);
  }

  cci_expects(*tok_ptr == '\'');
}

StringLiteralParser::StringLiteralParser(Lexer &lexer,
                                         const std::vector<Token> &string_toks,
                                         const TargetInfo &target)
{
  auto &diag = lexer.diagnostics();

  // The following code calculates a size bound that is the sum of all strings'
  // size for the result buffer, which is a bit over enough, given that
  // trigraphs and escape sequences, after processed, shrink the string size.

  cci_expects(!string_toks.empty());
  cci_expects(string_toks[0].size() >= 2);
  size_t size_bound = string_toks[0].size() - 2; // removes ""

  // Holds the size of the biggest string literal. This is used to allocate memory
  // for a temporary buffer to hold the processed strings.
  size_t max_token_size = size_bound;

  cci_expects(is_string_literal(string_toks[0].kind));
  kind = string_toks[0].kind;

  // Performs C11 5.1.1.2/6: Adjacent string literal tokens are concatenated.
  // Unfortunately, `i` has type `size_t` because otherwise size comparison gets
  // messy with casts.
  for (size_t i = 1; i != string_toks.size(); ++i)
  {
    cci_expects(is_string_literal(string_toks[i].kind));
    if (string_toks[i].is_not(kind) && string_toks[i].is_not(TokenKind::string_literal))
    {
      if (kind == TokenKind::string_literal)
        kind = string_toks[i].kind;
      else
      {
        // Concatenation of different kinds of strings could be supported, but
        // that would not be standard compliant.
        diag.report(string_toks[i].location(),
                    diag::err_nonstd_string_concatenation);
        has_error = true;
      }
    }

    cci_expects(string_toks[i].size() >= 2);
    size_bound += string_toks[i].size() - 2; // removes ""
    max_token_size = std::max(max_token_size, string_toks[i].size() - 2);
  }

  // In case we get an empty string literal, there's nothing left to be done.
  if (size_bound == 0)
  {
    // Includes the null terminator.
    result_buf.push_back('\0');
    return;
  }

  // Allows an space for the null terminator.
  ++size_bound;

  char_byte_width = resolve_char_width(kind, target);
  // Assumes that the char width for this target is a multiple of 8.
  cci_expects((target.char_width & 0b0111) == 0);
  char_byte_width /= 8;

  // More space is needed if we have wide/unicode strings literals.
  size_bound *= char_byte_width;

  // Token spellings are written to this buffer.
  small_vector<char, 256> token_buf;

  result_buf.resize(size_bound);
  token_buf.resize(max_token_size);

  char *result_ptr = result_buf.data();

  for (const auto &string_tok : string_toks)
  {
    cci_expects(is_string_literal(string_tok.kind));

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
        const char *tokbuf_start = tokbuf_ptr;
        tokbuf_ptr = std::find_if(tokbuf_ptr, tokbuf_end,
                                  [](char c) { return c == '\\'; });

        // FIXME: Parse UTF-8 too.
        result_ptr = std::copy(tokbuf_start, tokbuf_ptr, result_ptr);
      }
      else if (tokbuf_ptr[1] == 'u' || tokbuf_ptr[1] == 'U')
      {
        uint32_t code_point =
          evaluate_ucn(lexer, string_tok.location(), tokbuf_begin, tokbuf_ptr,
                       tokbuf_end, has_error);

        // FIXME: Convert to the appropriate encoding. This is very wrong as it
        // is right now.
        result_ptr = std::copy(reinterpret_cast<const char *>(code_point),
                               reinterpret_cast<const char *>(code_point) +
                                 sizeof(code_point),
                               result_ptr);
      }
      else
      {
        // If it's not a UCN, then it's a normal escape sequence.
        int32_t result_char =
          evaluate_escape_sequence(lexer, string_tok.location(), tokbuf_begin,
                                   tokbuf_ptr, tokbuf_end, has_error);

        // TODO: Handle char width of 2 and 4 bytes.
        if (char_byte_width == 1)
          *result_ptr++ = result_char & 0xFF;
        else
          cci_unreachable();
      }
    }
  }
}
