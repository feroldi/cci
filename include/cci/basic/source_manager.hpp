#pragma once

#include <string_view>
#include <string>
#include <vector>
#include <optional>
#include <cstddef>

namespace cci
{

// SourceLocation - This represents an offset into the buffer of
// a SourceManager.
struct SourceLocation
{
  std::size_t offset; //< Offset into a SourceManager's buffer.

  // Constructs a SourceLocation with `offset`.
  // Useful when constructing inplace (e.g. vector::emplace_back).
  explicit SourceLocation(std::size_t offset)
    : offset(offset) {}

  // Constructs a SourceLocation with `offset`.
  static auto with_offset(std::size_t offset) -> SourceLocation
  {
    return SourceLocation(offset);
  }

  auto based_on(SourceLocation base) const -> SourceLocation
  {
    return SourceLocation::with_offset(offset - base.offset);
  }
};

inline bool operator== (SourceLocation lhs, SourceLocation rhs) { return lhs.offset == rhs.offset; };
inline bool operator!= (SourceLocation lhs, SourceLocation rhs) { return lhs.offset != rhs.offset; };
inline bool operator<  (SourceLocation lhs, SourceLocation rhs) { return lhs.offset <  rhs.offset; };
inline bool operator>  (SourceLocation lhs, SourceLocation rhs) { return lhs.offset >  rhs.offset; };
inline bool operator<= (SourceLocation lhs, SourceLocation rhs) { return lhs.offset <= rhs.offset; };
inline bool operator>= (SourceLocation lhs, SourceLocation rhs) { return lhs.offset >= rhs.offset; };

// SourceRange - This represents a range of source locations, containing
// a starting point and an ending one. Useful to represent the text of a token,
// or a single line.
//
// Note that `end` is similar to an end iterator — it points past the last
// element of a container. In this case, `end` is treated as an offset
// past the last character. For instance, given a buffer "answer is 42\n",
// constructing a range that represents the substring "answer" would look like so:
//
//    SourceRange answer(SourceLocation(0u), SourceLocation(6u));
//
// Where `6u` is the position of the first white space in that buffer.
struct SourceRange
{
  SourceLocation start;
  SourceLocation end;

  SourceRange(SourceLocation start, SourceLocation end)
    : start(start), end(end) {}
};

// SourceManager - Manages a set of source files.
//
// TODO: Source files are concatenated into a memory buffer, and an offset for each
// file is kept in order to identify which source file a SourceLocation is from.
class SourceManager
{
  // Stores starting positions of every line in the source.
  //
  // Starting positions (except for the offset 0) begin after a new line.
  // For instance:
  //
  //    1| int main()
  //    2| {
  //    3|   printf("Hello, %s!\n", "World");
  //    4| }
  //
  // First position starts at the first byte ('i'), second one is at
  // after the first new line ('\n'), i.e. the '{' in the second line.
  std::vector<SourceLocation> line_offsets;

  // Source file's text content.
  std::string buffer;

  SourceManager(std::vector<SourceLocation> line_offsets, std::string buffer)
    : line_offsets(std::move(line_offsets)), buffer(std::move(buffer)) {}

public:
  // Returns a SourceManager with a single source file.
  //
  // None if `source_path` doesn't exist or is a directory.
  static auto from_file(std::string_view source_path) -> std::optional<SourceManager>;

  // Returns a SourceManager that manages a UTF-8 string buffer.
  static auto from_buffer(std::string buffer) -> SourceManager;

  // Returns the text represented by a SourceRange.
  auto get_text(SourceRange) const -> std::string_view;

  // Returns the whole sources' text.
  auto get_text() const -> std::string_view { return buffer; }

  // Returns the line text a SourceLocation is in.
  auto get_line_text(SourceLocation) const -> std::string_view;

  // Returns the line and column number for a SourceLocation.
  auto get_linecol(SourceLocation) const -> std::pair<unsigned, unsigned>;

public:
  SourceManager(SourceManager &&that) noexcept
    : line_offsets(std::move(that.line_offsets)),
      buffer(std::move(that.buffer)) {}

  SourceManager &operator= (SourceManager &&that) noexcept
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
