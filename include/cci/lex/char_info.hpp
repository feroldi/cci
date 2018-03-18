#pragma once

namespace cci {
bool is_ascii(const char c);
bool is_digit(const char c);
bool is_hexdigit(const char c);
bool is_octdigit(const char c);
bool is_control(const char c);
bool is_alpha(const char c);
bool is_alphanum(const char c);
bool is_printable(const char c);
} // namespace cci
