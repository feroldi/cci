#include "cci/basic/source_manager.hpp"
#include "cci/basic/file_stream.hpp"
#include "cci/util/contracts.hpp"
#include "fmt/format.h"
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace cci {

// Calculates all line offsets in `buffer`.
//
// For instance, given a buffer like the following:
//
//    "\n1234\n678\n\n"
//
// It finds every new-line character and keeps its offset plus 1 as a
// SourceLocation. Additionally, the first offset is always 0.
// Therefore, the list of source locations for the above buffer should look like
// so:
//
//    [0, 1, 6, 10, 11]
//
// Accessing the buffer with the above indices will give you
// the first character in the line. This is useful for the front-end,
// specifically the diagnostics system: printing out source lines to
// the user requires knowledge of where lines start and end.
//
// TL;DR: Starting positions (except for the offset 0) begin after a new-line.
// For instance:
//
//    1| int main()
//    2| {
//    3|   printf("Hello, %s!\n", "World");
//    4| }
//
// First position starts at the first byte ('i'), second one is at
// after the first new line ('\n'), i.e. the '{' in the second line.
static auto compute_source_line_offsets(const char *buf_begin,
                                        const char *buf_end)
  -> std::vector<SourceLocation>
{
  cci_expects(std::prev(buf_end)[0] == '\n' &&
              "Source file needs to end in a new line!");

  // First line starts at offset 0.
  std::vector offsets{SourceLocation(0)};
  auto line_ptr = buf_begin;

  while (line_ptr != buf_end)
  {
    line_ptr =
      std::find_if(line_ptr, buf_end, [](char C) { return C == '\n'; });
    std::advance(line_ptr, 1); //< Skips new line.
    offsets.emplace_back(static_cast<std::size_t>(line_ptr - buf_begin));
  }

  cci_ensures(line_ptr == buf_end);
  return offsets;
}

// Finds the index into `offsets` that corresponds to the line `loc` is in.
static auto find_source_line_index(const std::vector<SourceLocation> &offsets,
                                   SourceLocation loc) -> size_t
{
  cci_expects(offsets.size() > 1);
  cci_expects(loc >= offsets.front() && loc < offsets.back());

  for (auto it = offsets.begin(); it != std::prev(offsets.end()); ++it)
  {
    if (loc >= *it && loc < *std::next(it))
      return static_cast<size_t>(std::distance(offsets.begin(), it));
  }

  cci_unreachable();
}

auto SourceManager::from_file(std::string_view source_path)
  -> std::optional<SourceManager>
{
  std::optional<SourceManager> src_mgr;

  if (auto file_content = read_stream_utf8(source_path))
    src_mgr = SourceManager::from_buffer(std::move(*file_content));

  return src_mgr;
}

auto SourceManager::from_buffer(std::string buffer) -> SourceManager
{
  std::string_view buf = buffer;
  auto ln_offsets = compute_source_line_offsets(buf.begin(), buf.end());
  return SourceManager(std::move(ln_offsets), std::move(buffer));
}

auto SourceManager::text_slice(SourceRange range) const -> std::string_view
{
  cci_expects(!line_offsets.empty() && "Line offsets need to be computed!");
  return full_text().substr(range.start.offset,
                            range.end.offset - range.start.offset);
}

auto SourceManager::text_line(SourceLocation loc) const -> std::string_view
{
  cci_expects(!line_offsets.empty() && "Line offsets need to be computed!");

  const size_t i = find_source_line_index(line_offsets, loc);
  const auto line_start = line_offsets[i];

  // Gets the next new-line and removes the trailing '\n'.
  const auto line_end = line_offsets[i + 1].with_offset(1);

  return text_slice(SourceRange(line_start, line_end));
}

auto SourceManager::translate_to_linecolumn(SourceLocation loc) const
  -> std::pair<size_t, size_t>
{
  cci_expects(!line_offsets.empty() && "Line offsets need to be computed!");

  const size_t i = find_source_line_index(line_offsets, loc);
  const auto line_num = i + 1;

  // Column numbers can be calculated by subtracting the offset for the
  // first character in the line from the current source location.
  // This gives a 0-based column number, so adding one makes it 1-based.
  const auto column_num = loc.with_offset(line_offsets[i]).offset + 1;

  return std::pair(line_num, column_num);
}

} // namespace cci
