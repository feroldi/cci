// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

#include "cci/util/unicode.hpp"
#include <cstdint>
#include <utility>

static constexpr uint8_t UTF8_DFA_STATES[]{
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
};

enum DecodeState : uint32_t
{
  UTF8_ACCEPT,
  UTF8_REJECT,
};

constexpr auto decode(uint32_t &state, uint32_t &code_point, std::byte b)
  -> uint32_t
{
  const auto byte = static_cast<uint8_t>(b);
  const uint32_t type = UTF8_DFA_STATES[byte];
  code_point = (state != UTF8_ACCEPT) ? (byte & 0x3fu) | (code_point << 6)
                                      : (0xff >> type) & byte;
  state = UTF8_DFA_STATES[256 + state + type];
  return state;
}

namespace cci::utf8 {

// Converts a UTF-8 sequence into a code point.
auto decode_sequence(const std::byte *&first, const std::byte *last,
                     uint32_t &code_point) -> DecodeResult
{
  for (uint32_t state = UTF8_ACCEPT; first != last; ++first)
    if (decode(state, code_point, *first) == UTF8_ACCEPT)
      return ++first, DecodeResult::Ok;
  return DecodeResult::Invalid;
}

// Tries to decode one code point from a UTF-8 sequence.
auto try_decode_code_point(const std::byte *first, const std::byte *last)
  -> uint32_t
{
  if (uint32_t cp = 0; decode_sequence(first, last, cp) == DecodeResult::Ok)
    return cp;
  return 0;
}

// See https://stackoverflow.com/a/46637343/3646096
auto is_whitespace(uint32_t code_point) -> bool
{
  switch (code_point)
  {
    case U'\u0009': // character tabulation
    case U'\u000A': // line feed
    case U'\u000B': // line tabulation
    case U'\u000C': // form feed
    case U'\u000D': // carriage return
    case U'\u0020': // space
    case U'\u0085': // next line
    case U'\u00A0': // no-break space
    case U'\u1680': // ogham space mark
    case U'\u180E': // mongolian vowel separator
    case U'\u2000': // en quad
    case U'\u2001': // em quad
    case U'\u2002': // en space
    case U'\u2003': // em space
    case U'\u2004': // three-per-em space
    case U'\u2005': // four-per-em space
    case U'\u2006': // six-per-em space
    case U'\u2007': // figure space
    case U'\u2008': // punctuation space
    case U'\u2009': // thin space
    case U'\u200A': // hair space
    case U'\u200B': // zero width space
    case U'\u200C': // zero width non-joiner
    case U'\u200D': // zero width joiner
    case U'\u2028': // line separator
    case U'\u2029': // paragraph separator
    case U'\u202F': // narrow no-break space
    case U'\u205F': // medium mathematical space
    case U'\u2060': // word joiner
    case U'\u3000': // ideographic space
    case U'\uFEFF': // zero width non-breaking space
      return true;
    default:
      return false;
  }
}

} // namespace cci::utf8
