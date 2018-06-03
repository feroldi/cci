#pragma once

#include "cci/basic/source_manager.hpp"
#include "cci/langopts.hpp"
#include "cci/lex/lexer.hpp"
#include "cci/util/small_vector.hpp"
#include "cci/util/span.hpp"
#include <vector>

namespace cci {

struct NumericConstantParser
{
  const char *digit_begin; //< First meaningful digit.
  const char *digit_end; //< Past the last meaningful digit.

  bool has_error = false;
  bool has_period = false;
  bool has_exponent = false;
  bool is_unsigned = false;
  bool is_long = false;
  bool is_long_long = false;
  bool is_float = false;

  int32_t radix = 0;

  NumericConstantParser(Lexer &, std::string_view tok_spelling,
                        SourceLocation tok_loc);

  // Evaluates and returns the numeric constant to an integer constant value, as
  // well as whether the evaluation overflowed.
  auto to_integer(size_t int_width) const -> std::pair<uint64_t, bool>;

  bool is_floating_literal() const { return has_period || has_exponent; }
  bool is_integer_literal() const { return !is_floating_literal(); }
};

struct CharConstantParser
{
  uint32_t value = 0;
  TokenKind kind;
  size_t char_byte_width;
  bool is_multibyte;
  bool has_error = false;

  CharConstantParser(Lexer &, std::string_view tok_spelling,
                     SourceLocation tok_loc, TokenKind char_kind,
                     const TargetInfo &);
};

struct StringLiteralParser
{
  small_string<256> result_buf;
  TokenKind kind;
  size_t char_byte_width;
  bool has_error = false;

  StringLiteralParser(Lexer &, span<const Token> string_toks,
                      const TargetInfo &);
};

} // namespace cci
