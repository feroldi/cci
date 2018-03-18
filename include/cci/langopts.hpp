#pragma once

namespace cci {

// TargetInfo - Holds information about the target architecture.
struct TargetInfo
{
  size_t char_width = 8;
  size_t wchar_width = 32;
  size_t char16_t_width = 16;
  size_t char32_t_width = 32;

  TargetInfo() = default;
};

} // namespace cci
