#pragma once

#include <cstddef>
#include <cstdint>

namespace cci {
// SourceLocation - This represents an offset into the buffer of
// a SourceManager.
struct SourceLocation
{
  std::size_t offset; //< Offset into a SourceManager's buffer.

  SourceLocation() = default;

  // Constructs a SourceLocation with `offset`.
  // Useful when constructing in-place (e.g. vector::emplace_back).
  explicit SourceLocation(std::size_t offset) noexcept : offset(offset) {}

  // Constructs a new SourceLocation with an offset based off on `base`.
  auto with_offset(std::int64_t base) const -> SourceLocation
  {
    return SourceLocation(
      static_cast<std::size_t>(base + static_cast<std::int64_t>(offset)));
  }
};

inline bool operator==(SourceLocation lhs, SourceLocation rhs)
{
  return lhs.offset == rhs.offset;
}
inline bool operator!=(SourceLocation lhs, SourceLocation rhs)
{
  return lhs.offset != rhs.offset;
}
inline bool operator<(SourceLocation lhs, SourceLocation rhs)
{
  return lhs.offset < rhs.offset;
}
inline bool operator>(SourceLocation lhs, SourceLocation rhs)
{
  return lhs.offset > rhs.offset;
}
inline bool operator<=(SourceLocation lhs, SourceLocation rhs)
{
  return lhs.offset <= rhs.offset;
}
inline bool operator>=(SourceLocation lhs, SourceLocation rhs)
{
  return lhs.offset >= rhs.offset;
}

// SourceRange - This represents a range of source locations [start, end),
// containing a starting point and an ending one. Useful to represent the text
// of a token, or a single line.
struct SourceRange
{
  SourceLocation start;
  SourceLocation end;

  SourceRange() = default;
  SourceRange(SourceLocation start, SourceLocation end) noexcept
    : start(start), end(end)
  {}

  SourceRange(SourceLocation loc) noexcept : start(loc), end(loc.with_offset(1))
  {}
};

} // namespace cci
