#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace cci {

// SourceLocation - This represents an offset into the buffer of
// a SourceManager.
struct SourceLocation
{
  size_t offset; //< Offset into a SourceManager's buffer.

  // Constructs a SourceLocation with `offset`.
  // Useful when constructing in-place (e.g. vector::emplace_back).
  explicit SourceLocation(size_t offset) : offset(offset) {}

  // Constructs a new SourceLocation with an offset based off on `base`.
  auto with_offset(SourceLocation base) const -> SourceLocation
  {
    return SourceLocation(offset - base.offset);
  }

  // Constructs a new SourceLocation with an offset based off on `base`.
  auto with_offset(size_t base) const -> SourceLocation
  {
    return SourceLocation(offset - base);
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

// SourceRange - This represents a range of source locations, containing
// a starting point and an ending one. Useful to represent the text of a token,
// or a single line.
//
// Note that `end` is similar to an end iterator — it points past the last
// element of a container. In this case, `end` is treated as an offset
// past the last character. For instance, given a buffer "answer is 42\n",
// constructing a range that represents the substring "answer" would look like
// so:
//
//    SourceRange answer(SourceLocation(0u), SourceLocation(6u));
//
// Where `6u` is the position of the first white space in that buffer.
struct SourceRange
{
  SourceLocation start;
  SourceLocation end;

  SourceRange(SourceLocation start, SourceLocation end) noexcept
    : start(start), end(end)
  {}
};

// SourceManager - Manages a set of source files.
class SourceManager
{
  // Stores starting positions of every line in the source.
  std::vector<SourceLocation> line_offsets;

  // Source file's text content.
  std::string buffer;

  SourceManager(std::vector<SourceLocation> line_offsets, std::string buffer)
    : line_offsets(std::move(line_offsets)), buffer(std::move(buffer))
  {}

public:
  // Constructs a SourceManager from a source file.
  //
  // \returns `std::nullopt` if `source_path` doesn't exist or is a directory.
  static auto from_file(std::string_view source_path)
    -> std::optional<SourceManager>;

  // Constructs a SourceManager that manages a UTF-8 string buffer.
  static auto from_buffer(std::string buffer) -> SourceManager;

  // Returns the text slice represented by a SourceRange.
  auto text_slice(SourceRange) const -> std::string_view;

  // Returns the whole source's text.
  auto full_text() const -> std::string_view { return buffer; }

  // Returns the text line a SourceLocation is in.
  auto text_line(SourceLocation) const -> std::string_view;

  // Returns the line and column number for a SourceLocation.
  auto translate_to_linecolumn(SourceLocation) const
    -> std::pair<size_t, size_t>;

public:
  SourceManager(SourceManager &&that) noexcept
    : line_offsets(std::move(that.line_offsets)), buffer(std::move(that.buffer))
  {}

  SourceManager &operator=(SourceManager &&that) noexcept
  {
    if (this != std::addressof(that))
    {
      line_offsets = std::move(that.line_offsets);
      buffer = std::move(that.buffer);
    }

    return *this;
  }
};

} // namespace cci
