#include "cci/lex/literal_parser.hpp"
#include "./lex_diagnostics.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"
#include <algorithm>
#include <climits>
#include <utility>

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
void report(Lexer &lex, const char *char_ptr, SourceLocation tok_loc,
            const char *tok_begin, diag::Lex err_code, Args &&... args)
{
  auto &diag = lex.source_mgr.get_diagnostics();
  diag.report(lex.character_location(tok_loc, tok_begin, char_ptr),
              err_code, std::forward<Args>(args)...);
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
    // TODO: Report diag::err_ucn_invalid.
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
