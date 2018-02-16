#include "cci/lex/literal_parser.hpp"
#include "./lex_diagnostics.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"
#include <utility>

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

namespace cci {

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
        if (is_fp || is_unsigned) break;
        is_unsigned = true;
        continue;
      case 'l':
      case 'L':
        if (is_long || is_long_long) break;
        if (s[1] == s[0])
        {
          if (is_fp) break;
          ++s;
          is_long_long = true;
        }
        else
          is_long = true;
        continue;
      case 'f':
      case 'F':
        if (!is_fp || is_float) break;
        is_float = true;
        continue;
      default:
        break;
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

} // namespace cci
