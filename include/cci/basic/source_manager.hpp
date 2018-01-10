#pragma once

#include "cci/util/filesystem.hpp"
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace cci {
class SourceManager;

// SourceLocation - This represents an offset into the buffer of
// a SourceManager.
struct SourceLocation
{
  size_t offset; //< Offset into a SourceManager's buffer.

  SourceLocation() = default;

  // Constructs a SourceLocation with `offset`.
  // Useful when constructing in-place (e.g. vector::emplace_back).
  explicit SourceLocation(size_t offset) noexcept : offset(offset) {}

  // Constructs a new SourceLocation with an offset based off on `base`.
  auto with_offset(int64_t base) const -> SourceLocation
  {
    return SourceLocation(static_cast<size_t>(static_cast<int64_t>(offset) + base));
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

  SourceRange(SourceLocation loc) noexcept
    : start(loc), end(loc.with_offset(1ull))
  {}
};

// SourceManager - Manages a set of source files.
class SourceManager
{
  // Stores starting positions of every line in the source.
  std::vector<SourceLocation> line_offsets;

  // Source file's text content. May be ASCII or UTF-8.
  std::string buffer;

  // Source file's path, if loaded from a file.
  std::optional<fs::path> buffer_filepath;

  SourceManager(std::vector<SourceLocation> line_offsets, std::string buffer)
    : line_offsets(std::move(line_offsets)), buffer(std::move(buffer))
  {}

public:
  SourceManager(SourceManager &&) = default;
  SourceManager &operator=(SourceManager &&) = default;

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

  // Checks whether this SourceManager was loaded from a file.
  auto loaded_from_file() const -> bool;

  // Returns the source's file path.
  auto file_path() const -> const fs::path &;
};

// FullSourceLoc - This represents a `SourceLocation` with its respective
// `SourceManager` owner.
struct FullSourceLoc
{
private:
  const SourceManager &src_mgr;
  SourceLocation loc;

public:
  FullSourceLoc() = default;
  FullSourceLoc(const SourceManager &ctx, SourceLocation loc) noexcept
    : src_mgr(ctx), loc(loc)
  {}

  auto get_manager() const -> const SourceManager & { return src_mgr; }
  auto get_location() const -> SourceLocation { return loc; }
  auto full_text() const -> std::string_view { return src_mgr.full_text(); }
  auto text_line() const -> std::string_view { return src_mgr.text_line(loc); }
  auto translate_to_linecolumn() const -> std::pair<size_t, size_t>
  {
    return src_mgr.translate_to_linecolumn(loc);
  }
  auto loaded_from_file() const -> bool { return src_mgr.loaded_from_file(); }
  auto file_path() const -> const fs::path & { return src_mgr.file_path(); }
};

} // namespace cci
