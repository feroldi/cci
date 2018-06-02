#pragma once
#include <cstdint>

namespace cci {
namespace charinfo {
extern const uint8_t ASCIITable[256];

enum : uint8_t
{
  CHAR_ALPHA = 0x1, // Alphabetic
  CHAR_CONTROL = 0x2, // Control character
  CHAR_DIGIT = 0x4, // Numeric digit
  CHAR_OCTDIG = 0x8, // Octal numeric digit
  CHAR_HEXDIG = 0x10, // Hexadecimal numeric digit
  CHAR_PUNCT = 0x20, // Punctuation
};
} // namespace charinfo

inline bool is_digit(unsigned char c)
{
  using namespace charinfo;
  return ASCIITable[c] & CHAR_DIGIT;
}

inline bool is_hexdigit(unsigned char c)
{
  using namespace charinfo;
  return ASCIITable[c] & CHAR_HEXDIG;
}

inline bool is_octdigit(unsigned char c)
{
  using namespace charinfo;
  return ASCIITable[c] & CHAR_OCTDIG;
}

inline bool is_control(unsigned char c)
{
  using namespace charinfo;
  return ASCIITable[c] & CHAR_CONTROL;
}

inline bool is_alpha(unsigned char c)
{
  using namespace charinfo;
  return ASCIITable[c] & CHAR_ALPHA;
}

inline bool is_alphanum(unsigned char c)
{
  using namespace charinfo;
  return ASCIITable[c] & (CHAR_ALPHA | CHAR_DIGIT);
}

inline bool is_printable(unsigned char c) { return !is_control(c); }

constexpr inline bool is_ascii(char c)
{
  return static_cast<unsigned char>(c) <= 127;
}

} // namespace cci
