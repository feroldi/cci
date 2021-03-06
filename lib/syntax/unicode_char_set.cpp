#include "cci/syntax/unicode_char_set.hpp"
#include <algorithm>
#include <cstdint>
#include <iterator>

// UnicodeCharRange - Represents an inclusive range of Unicode characters. This
// is used to simplify the validation of code points in identifiers.
struct UnicodeCharRange
{
    uint32_t first;
    uint32_t last;
};

constexpr auto operator==(UnicodeCharRange range, uint32_t code_point) -> bool
{
    return code_point >= range.first && code_point <= range.last;
}

constexpr auto operator==(uint32_t code_point, UnicodeCharRange range) -> bool
{
    return code_point >= range.first && code_point <= range.last;
}

// C11 D.1: Universal character names for identifiers.
static constexpr UnicodeCharRange ALLOWED_IDENT_CHARS_SET[]{
    // C11 D.1/1
    {0x00A8, 0x00A8},
    {0x00AA, 0x00AA},
    {0x00AD, 0x00AD},
    {0x00AF, 0x00AF},
    {0x00B2, 0x00B5},
    {0x00B7, 0x00BA},
    {0x00BC, 0x00BE},
    {0x00C0, 0x00D6},
    {0x00D8, 0x00F6},
    {0x00F8, 0x00FF},

    // C11 D.1/2
    {0x0100, 0x167F},
    {0x1681, 0x180D},
    {0x180F, 0x1FFF},

    // C11 D.1/3
    {0x200B, 0x200B},
    {0x202A, 0x202E},
    {0x203F, 0x2040},
    {0x2054, 0x2054},
    {0x2060, 0x206F},

    // C11 D.1/4
    {0x2070, 0x218F},
    {0x2460, 0x24FF},
    {0x2776, 0x2793},
    {0x2C00, 0x2DFF},
    {0x2E80, 0x2FFF},

    // C11 D.1/5
    {0x3004, 0x3007},
    {0x3021, 0x302F},
    {0x3031, 0x303F},

    // C11 D.1/6
    {0x3040, 0xD7FF},

    // C11 D.1/7
    {0xF900, 0xFD3D},
    {0xFD40, 0xFDCF},
    {0xFDF0, 0xFE44},
    {0xFE47, 0xFFFD},

    // C11 D.1/8
    {0x10000, 0x1FFFD},
    {0x20000, 0x2FFFD},
    {0x30000, 0x3FFFD},
    {0x40000, 0x4FFFD},
    {0x50000, 0x5FFFD},
    {0x60000, 0x6FFFD},
    {0x70000, 0x7FFFD},
    {0x80000, 0x8FFFD},
    {0x90000, 0x9FFFD},
    {0xA0000, 0xAFFFD},
    {0xB0000, 0xBFFFD},
    {0xC0000, 0xCFFFD},
    {0xD0000, 0xDFFFD},
    {0xE0000, 0xEFFFD},
};

// C11 D.2: Ranges of characters disallowed initially.
static constexpr UnicodeCharRange DISALLOWED_INITIALLY_IDENT_CHARS_SET[]{
    // C11 D.2/1
    {0x0300, 0x036F},
    {0x1DC0, 0x1DFF},
    {0x20D0, 0x20FF},
    {0xFE20, 0xFE2F},
};

namespace cci
{

auto is_allowed_id_char(uint32_t cp) -> bool
{
    const auto rng_begin = std::begin(ALLOWED_IDENT_CHARS_SET);
    const auto rng_end = std::end(ALLOWED_IDENT_CHARS_SET);
    return std::find(rng_begin, rng_end, cp) != rng_end;
}

auto is_allowed_initially_id_char(uint32_t cp) -> bool
{
    const auto rng_begin = std::begin(DISALLOWED_INITIALLY_IDENT_CHARS_SET);
    const auto rng_end = std::end(DISALLOWED_INITIALLY_IDENT_CHARS_SET);
    return std::find(rng_begin, rng_end, cp) == rng_end;
}

} // namespace cci
