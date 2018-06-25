// Unicode conversion functions from Unicode, Inc. adapted to C++.

// Copyright 2001-2004 Unicode, Inc.
//
// Disclaimer
//
// This source code is provided as is by Unicode, Inc. No claims are
// made as to fitness for any particular purpose. No warranties of any
// kind are expressed or implied. The recipient agrees to determine
// applicability of information provided. If this file has been
// purchased on magnetic or optical media from Unicode, Inc., the
// sole remedy for any claim will be exchange of defective media
// within 90 days of receipt.
//
// Limitations on Rights to Redistribute This Code
//
// Unicode, Inc. hereby grants the right to freely use the information
// supplied in this file in the creation of products supporting the
// Unicode Standard, and to make copies of this file in any form
// for internal or external distribution as long as this notice
// remains attached.

#pragma once

#include <cstdint>
#include <cstddef>

namespace cci::uni {

using UTF32 = uint32_t;
using UTF16 = uint16_t;
using UTF8 = uint8_t;

/* Some fundamental constants */
constexpr inline UTF32 UNI_REPLACEMENT_CHAR = 0x0000FFFDUL;
constexpr inline UTF32 UNI_MAX_BMP = 0x0000FFFFUL;
constexpr inline UTF32 UNI_MAX_UTF16 = 0x0010FFFFUL;
constexpr inline UTF32 UNI_MAX_UTF32 = 0x7FFFFFFFUL;
constexpr inline UTF32 UNI_MAX_LEGAL_UTF32 = 0x0010FFFFUL;

enum ConversionResult
{
  conversionOK, /* conversion successful */
  sourceExhausted, /* partial character in source, but hit end */
  targetExhausted, /* insuff. room in target for conversion */
  sourceIllegal, /* source sequence is illegal/malformed */
};

enum ConversionFlags
{
  strictConversion,
  lenientConversion,
};

ConversionResult convert_utf8_sequence(const UTF8 **sourceStart,
                                       const UTF8 *sourceEnd,
                                       UTF32 *targetStart,
                                       ConversionFlags flags);

ConversionResult convert_utf8_to_utf16(const UTF8 **sourceStart,
                                       const UTF8 *sourceEnd,
                                       UTF16 **targetStart, UTF16 *targetEnd,
                                       ConversionFlags flags);
ConversionResult convert_utf16_to_utf8(const UTF16 **sourceStart,
                                       const UTF16 *sourceEnd,
                                       UTF8 **targetStart, UTF8 *targetEnd,
                                       ConversionFlags flags);

ConversionResult convert_utf8_to_utf32(const UTF8 **sourceStart,
                                       const UTF8 *sourceEnd,
                                       UTF32 **targetStart, UTF32 *targetEnd,
                                       ConversionFlags flags);
ConversionResult convert_utf32_to_utf8(const UTF32 **sourceStart,
                                       const UTF32 *sourceEnd,
                                       UTF8 **targetStart, UTF8 *targetEnd,
                                       ConversionFlags flags);

ConversionResult convert_utf16_to_utf32(const UTF16 **sourceStart,
                                        const UTF16 *sourceEnd,
                                        UTF32 **targetStart, UTF32 *targetEnd,
                                        ConversionFlags flags);
ConversionResult convert_utf32_to_utf16(const UTF32 **sourceStart,
                                        const UTF32 *sourceEnd,
                                        UTF16 **targetStart, UTF16 *targetEnd,
                                        ConversionFlags flags);

bool is_legal_utf8_sequence(const UTF8 *source, const UTF8 *sourceEnd);

size_t num_bytes_for_utf8(UTF8 ch);

} // namespace cci::uni
