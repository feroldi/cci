#pragma once

#include <cstdint>

namespace cci {

// Checks whether a code point is an allowed identifier character as per C11
// D.1.
auto is_allowed_id_char(uint32_t code_point) -> bool;

// Checks whether a code point is an sallowed initially identifier character as
// per C11 D.2.
auto is_allowed_initially_id_char(uint32_t code_point) -> bool;

} // namespace cci
