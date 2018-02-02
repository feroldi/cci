#pragma once

#include <cstdint>
#include <utility>

namespace cci::utf8 {

// DecodeResult - The result status of a UTF-8 sequence conversion operation.
enum class DecodeResult
{
  Ok, //< A code point was decoded.
  Invalid, //< Some invalid byte in the sequence.
};

// Converts a UTF-8 sequence into a code point.
//
// \param first Iterator to the beginning of the UTF-8 sequence.
// \param last Iterator past the end of the UTF-8 sequence.
//
// \return DecodeResult::Ok if a code point is decoded, DecodeResult::Invalid if
// the sequence is rejected. The iterator `first` is advancted past the end
// of the code point on success, and at the invalid byte on failure.
auto decode_sequence(const std::byte *&first, const std::byte *last,
                     uint32_t &code_point) -> DecodeResult;

// Tries to decode one code point from a UTF-8 sequence.
//
// \param first Iterator to the beginning of the UTF-8 sequence.
// \param last Iterator past the end of the UTF-8 sequence.
//
// \return The decoded code point on success, or zero otherwise.
auto try_decode_code_point(const std::byte *first, const std::byte *last)
  -> uint32_t;

// Checks whether a UTF-8 code point represents a whitespace.
auto is_whitespace(uint32_t code_point) -> bool;

} // namespace cci::utf8
