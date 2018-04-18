#include "cci/lex/char_info.hpp"
#include <cstdint>

namespace cci {
enum CharSetProperty : uint8_t
{
  ALPHA = 0x1, // Alphabetic
  CONTROL = 0x2, // Control character
  DIGIT = 0x4, // Numeric digit
  OCTDIG = 0x8, // Octal numeric digit
  HEXDIG = 0x10, // Hexadecimal numeric digit
  PUNCT = 0x20, // Punctuation
};

// ASCII table based off on http://en.cppreference.com/w/cpp/language/ascii
static uint8_t ASCII_TABLE[128] = {
  CONTROL, // U+0000 (NUL)
  CONTROL, // U+0001 (SOH)
  CONTROL, // U+0002 (STX)
  CONTROL, // U+0003 (ETX)
  CONTROL, // U+0004 (EOT)
  CONTROL, // U+0005 (ENQ)
  CONTROL, // U+0006 (ACK)
  CONTROL, // U+0007 (BEL)
  CONTROL, // U+0008 (BS)
  CONTROL, // U+0009 (HT)
  CONTROL, // U+000A (LF)
  CONTROL, // U+000B (VT)
  CONTROL, // U+000C (FF)
  CONTROL, // U+000D (CR)
  CONTROL, // U+000E (SO)
  CONTROL, // U+000F (SI)
  CONTROL, // U+0010 (DLE)
  CONTROL, // U+0011 (DC1)
  CONTROL, // U+0012 (DC2)
  CONTROL, // U+0013 (DC3)
  CONTROL, // U+0014 (DC4)
  CONTROL, // U+0015 (NAK)
  CONTROL, // U+0016 (SYN)
  CONTROL, // U+0017 (ETB)
  CONTROL, // U+0018 (CAN)
  CONTROL, // U+0019 (EM)
  CONTROL, // U+001A (SUB)
  CONTROL, // U+001B (ESC)
  CONTROL, // U+001C (FS)
  CONTROL, // U+001D (GS)
  CONTROL, // U+001E (RS)
  CONTROL, // U+001F (US)
  PUNCT, // U+0020 (SP)
  PUNCT, // U+0021 (!)
  PUNCT, // U+0022 (")
  PUNCT, // U+0023 (#)
  PUNCT, // U+0024 ($)
  PUNCT, // U+0025 (%)
  PUNCT, // U+0026 ($)
  PUNCT, // U+0027 (')
  PUNCT, // U+0028 (()
  PUNCT, // U+0029 (()
  PUNCT, // U+002A (*)
  PUNCT, // U+002B (+)
  PUNCT, // U+002C (,)
  PUNCT, // U+002D (-)
  PUNCT, // U+002E (.)
  PUNCT, // U+002F (/)
  DIGIT | OCTDIG | HEXDIG, // U+0030 (0)
  DIGIT | OCTDIG | HEXDIG, // U+0031 (1)
  DIGIT | OCTDIG | HEXDIG, // U+0032 (2)
  DIGIT | OCTDIG | HEXDIG, // U+0033 (3)
  DIGIT | OCTDIG | HEXDIG, // U+0034 (4)
  DIGIT | OCTDIG | HEXDIG, // U+0035 (5)
  DIGIT | OCTDIG | HEXDIG, // U+0036 (6)
  DIGIT | OCTDIG | HEXDIG, // U+0037 (7)
  DIGIT | HEXDIG, // U+0038 (8)
  DIGIT | HEXDIG, // U+0039 (9)
  PUNCT, // U+003A (:)
  PUNCT, // U+003B (;)
  PUNCT, // U+003C (<)
  PUNCT, // U+003D (=)
  PUNCT, // U+003E (>)
  PUNCT, // U+003F (?)
  PUNCT, // U+0040 (@)
  ALPHA | HEXDIG, // U+0041 (A)
  ALPHA | HEXDIG, // U+0042 (B)
  ALPHA | HEXDIG, // U+0043 (C)
  ALPHA | HEXDIG, // U+0044 (D)
  ALPHA | HEXDIG, // U+0045 (E)
  ALPHA | HEXDIG, // U+0046 (F)
  ALPHA, // U+0047 (G)
  ALPHA, // U+0048 (H)
  ALPHA, // U+0049 (I)
  ALPHA, // U+004A (J)
  ALPHA, // U+004B (K)
  ALPHA, // U+004C (L)
  ALPHA, // U+004D (M)
  ALPHA, // U+004E (N)
  ALPHA, // U+004F (O)
  ALPHA, // U+0050 (P)
  ALPHA, // U+0051 (Q)
  ALPHA, // U+0052 (R)
  ALPHA, // U+0053 (S)
  ALPHA, // U+0054 (T)
  ALPHA, // U+0055 (U)
  ALPHA, // U+0056 (V)
  ALPHA, // U+0057 (W)
  ALPHA, // U+0058 (X)
  ALPHA, // U+0059 (Y)
  ALPHA, // U+005A (Z)
  PUNCT, // U+005B ([)
  PUNCT, // U+005C (\)
  PUNCT, // U+005D (])
  PUNCT, // U+005E (^)
  PUNCT, // U+005F (_)
  PUNCT, // U+0060 (`)
  ALPHA | HEXDIG, // U+0061 (a)
  ALPHA | HEXDIG, // U+0062 (b)
  ALPHA | HEXDIG, // U+0063 (c)
  ALPHA | HEXDIG, // U+0064 (d)
  ALPHA | HEXDIG, // U+0065 (e)
  ALPHA | HEXDIG, // U+0066 (f)
  ALPHA, // U+0067 (g)
  ALPHA, // U+0068 (h)
  ALPHA, // U+0069 (i)
  ALPHA, // U+006A (j)
  ALPHA, // U+006B (k)
  ALPHA, // U+006C (l)
  ALPHA, // U+006D (m)
  ALPHA, // U+006E (n)
  ALPHA, // U+006F (o)
  ALPHA, // U+0070 (p)
  ALPHA, // U+0071 (q)
  ALPHA, // U+0072 (r)
  ALPHA, // U+0073 (s)
  ALPHA, // U+0074 (t)
  ALPHA, // U+0075 (u)
  ALPHA, // U+0076 (v)
  ALPHA, // U+0077 (w)
  ALPHA, // U+0078 (x)
  ALPHA, // U+0079 (y)
  ALPHA, // U+007A (z)
  PUNCT, // U+007B ({)
  PUNCT, // U+007C (|)
  PUNCT, // U+007D (})
  PUNCT, // U+007E (~)
  CONTROL, // U+007F (DEL)
};

bool is_digit(const char c) { return ASCII_TABLE[c & 0x7F] & DIGIT; }

bool is_hexdigit(const char c) { return ASCII_TABLE[c & 0x7F] & HEXDIG; }

bool is_octdigit(const char c) { return ASCII_TABLE[c & 0x7F] & OCTDIG; }

bool is_control(const char c) { return ASCII_TABLE[c & 0x7F] & CONTROL; }

bool is_alpha(const char c) { return ASCII_TABLE[c & 0x7F] & ALPHA; }

bool is_alphanum(const char c)
{
  return ASCII_TABLE[c & 0x7F] & (ALPHA | DIGIT);
}

bool is_printable(const char c) { return !is_control(c); }
} // namespace cci
