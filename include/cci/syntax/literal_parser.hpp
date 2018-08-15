#pragma once

#include "cci/langopts.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/util/span.hpp"
#include <vector>

namespace cci {

struct NumericConstantParser
{
private:
  const char *digit_begin; //< First meaningful digit.
  const char *digit_end; //< Past the last meaningful digit.

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
                        srcmap::ByteLoc tok_loc);

  // Evaluates and returns the numeric constant to an integer constant value, as
  // well as whether the evaluation overflowed.
  auto to_integer() const -> std::pair<uint64_t, bool>;

  bool is_floating_literal() const { return has_period || has_exponent; }
  bool is_integer_literal() const { return !is_floating_literal(); }
};

struct CharConstantParser
{
  uint32_t value;
  Category category;
  bool is_multibyte;
  bool has_error = false;

  CharConstantParser(Scanner &, std::string_view tok_spelling,
                     srcmap::ByteLoc tok_loc, Category char_category,
                     const TargetInfo &);
};

struct StringLiteralParser
{
private:
  std::vector<char> result_buf;
  char *result_ptr;

public:
  Category category;
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
};

} // namespace cci
