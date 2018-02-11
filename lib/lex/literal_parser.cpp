#include "cci/lex/literal_parser.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/lex/lexer.hpp"
#include <utility>

namespace cci {
namespace diag {
enum LiteralParser
{
#define DIAG(CODE, LEVEL, FORMAT) CODE,
#include "cci/basic/diagnostics_lex.inc"
#undef DIAG
};
} // namespace diag

template <>
struct diagnostics_error_code<diag::LiteralParser>
{
  constexpr static auto info(diag::LiteralParser code) -> ErrorCodeInfo
  {
#define DIAG(CODE, LEVEL, FORMAT)                                              \
  case diag::CODE: return {LEVEL, FORMAT};
    switch (code)
    {
#include "cci/basic/diagnostics_lex.inc"
    }
#undef DIAG
  }
};

template <>
struct is_diagnostics_error_code<diag::LiteralParser> : std::true_type
{
};

NumericConstantParser::NumericConstantParser(Lexer &lexer,
                                             std::string_view tok_spelling,
                                             SourceLocation tok_loc)
{
  cci_expects(tok_spelling.end()[0] == '\0');
  const char *const tok_begin = tok_spelling.begin();
  const char *const tok_end = tok_spelling.end();
  auto &diag = lexer.source_mgr.get_diagnostics();
  const char *s = tok_begin;
  digit_begin = tok_begin;

  if (s[0] >= '1' && s[0] <= '9')
  {
    radix = 10;
    s = std::find_if_not(std::next(s), tok_end, is_digit);
  }
  else if (s[0] == '0')
  {
    ++s;
    if (s[0] >= '0' && s[0] <= '7')
    {
      radix = 8;
      ++digit_begin;
      s = std::find_if_not(std::next(s), tok_end, is_octdigit);
    }
    else if (s[0] == 'x' || s[0] == 'X')
    {
      radix = 16;
      std::advance(digit_begin, 2);
      s = std::find_if_not(std::next(s), tok_end, is_hexdigit);
    }
    else
      radix = 10;
  }

  digit_end = s;

  for (; s != tok_end; ++s)
  {
    switch (s[0])
    {
      case 'u':
      case 'U':
        if (!is_unsigned)
          is_unsigned = true;
        else
        {
          has_error = true;
          diag.report(tok_loc, diag::err_invalid_suffix);
          return;
        }
        break;
      case 'l':
      case 'L':
        if (s[1] == s[0])
        {
          ++s;
          if (!is_long_long)
            is_long_long = true;
          else
          {
            has_error = true;
            diag.report(tok_loc, diag::err_invalid_suffix);
            return;
          }
        }
        else if (!is_long)
          is_long = true;
        else
        {
          has_error = true;
          diag.report(tok_loc, diag::err_invalid_suffix);
          return;
        }
        break;
      default:
        has_error = true;
        diag.report(tok_loc, diag::err_invalid_digit);
        return;
    }
  }
}

auto NumericConstantParser::eval_to_integer() -> std::pair<uint64_t, bool>
{
  cci_expects(!(has_period || has_exponent));
  cci_expects(radix != 0);

  uint64_t value = 0;
  for (auto it = digit_begin; it != digit_end; ++it)
  {
    // FIXME: Check for overflow.
    value *= radix;
    value += hexdigit_value(*it);
  }

  return {value, false};
}

} // namespace cci
