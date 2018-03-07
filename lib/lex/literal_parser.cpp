#include "cci/lex/literal_parser.hpp"
#include "./lex_diagnostics.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"
#include <algorithm>
#include <utility>
#include <climits>

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

NumericConstantParser::NumericConstantParser(Lexer &lexer,
                                             std::string_view tok_spelling,
                                             SourceLocation tok_loc)
{
  cci_expects(tok_spelling.end()[0] == '\0');
  const char *const tok_begin = tok_spelling.begin();
  const char *const tok_end = tok_spelling.end();
  const char *s = tok_begin;
  digit_begin = tok_begin;

  auto report = [&, this](const char *char_ptr, diag::Lex err_code,
                          auto &&... args) {
    auto &diag = lexer.source_mgr.get_diagnostics();
    has_error = true;
    diag.report(lexer.character_location(tok_loc, tok_spelling, char_ptr),
                err_code, decltype(args)(args)...);
  };

  auto parse_possible_period_or_exponent = [&, this] {
    cci_expects(radix == 10 || radix == 8);

    if (is_hexdigit(s[0]) && *s != 'e' && *s != 'E')
    {
      report(s, diag::err_invalid_digit, *s,
             numeric_constant_name_for_radix(radix, false));
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
        report(exponent, diag::err_empty_exponent);
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
          report(exponent, diag::err_empty_exponent);
      }
      else if (has_period)
        report(tok_begin, diag::err_missing_exponent);
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
    report(s, diag::err_invalid_suffix,
           std::string_view(digit_end, tok_end - digit_end),
           numeric_constant_name_for_radix(radix, is_fp));
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

static auto decode_ucn(const char *s) -> std::pair<uint64_t, const char *>
{
  cci_expects(*s == 'u' || *s == 'U');
  int num_hexdigits = *s == 'u' ? 4 : 8;
  ++s;
  uint64_t code_point = 0;

  for (int i = 0; i < num_hexdigits; ++i)
  {
    // TODO: Report diag::err_ucn_invalid.
    if (uint32_t value = hexdigit_value(s[i]); value != -1U)
    {
      code_point += value;
      code_point <<= 4;
    }
    else
      break;
  }

  return std::pair(code_point, s + num_hexdigits);
}

// Returns the value representation of an escape sequence, and the final
// position of the cursor.
static auto decode_escape_sequence(const char *sequence)
  -> std::pair<uint64_t, const char *>
{
  auto s = sequence;
  uint64_t value = 0;
  // http://en.cppreference.com/w/c/language/escape
  switch (s[0])
  {
    case '\'':
    case '"':
    case '?':
    case '\\':
      value = s[0];
      break;
    case 'a': // audible bell
      value = 0x07;
      break;
    case 'b': // backspace
      value = 0x08;
      break;
    case 'f': // form feed
      value = 0x0c;
      break;
    case 'n': // line feed
      value = 0x0a;
      break;
    case 'r': // carriage return
      value = 0x0d;
      break;
    case 't': // horizontal tab
      value = 0x09;
      break;
    case 'v': // vertical tab
      value = 0x0b;
      break;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': // arbitrary octal value
      for (int i = 0; is_octdigit(s[i]) && i < 3; ++i, ++s)
      {
        value += hexdigit_value(*s);
        value <<= 3;
      }
      break;
    case 'x': // arbitrary hexadecimal value
      ++s;
      // FIXME: This doesn't check for empty or single digit hexs.
      for (; is_hexdigit(*s); ++s)
      {
        value += hexdigit_value(*s);
        value <<= 4;
      }
      break;
    case 'u':
    case 'U':
      std::tie(value, s) = decode_ucn(s);
      break;
  }

  return std::pair(value, s);
}

CharConstantParser::CharConstantParser(Lexer &lexer,
                                       std::string_view tok_spelling,
                                       SourceLocation tok_loc,
                                       TokenKind char_kind)
{
  (void)lexer;
  (void)tok_loc;
  cci_expects(is_char_constant(char_kind));
  cci_expects(tok_spelling.end()[0] == '\0');
  const char *const tok_begin = tok_spelling.begin();
  const char *s = tok_begin;

  // Skips either L, u or U.
  if (char_kind != TokenKind::char_constant)
    ++s;

  ++s; // Skips ' character.

  if (*s == '\\')
    std::tie(this->value, s) = decode_escape_sequence(s + 1);
  else
  {
    this->value = static_cast<uint64_t>(*s);
    ++s;
  }

  cci_expects(*s == '\'');
}
