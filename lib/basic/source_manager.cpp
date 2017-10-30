#include "ccompiler/basic/source_manager.hpp"
#include "ccompiler/basic/file_stream.hpp"
#include "ccompiler/util/contracts.hpp"
#include "fmt/format.h"
#include <string_view>
#include <string>
#include <vector>
#include <optional>

namespace ccompiler {

// Calculates all '\n' (new line) offsets in `buffer`.
static auto calc_line_offsets(std::string_view buffer) -> std::vector<SourceLocation>
{
  Expects(std::prev(buffer.end())[0] == '\n' && "Source file needs to end in a '\n' (new line)!");

  std::vector<SourceLocation> offsets;
  auto buf_begin = buffer.begin();
  auto buf_end = buffer.end();
  auto line_ptr = buffer.begin();

  while (line_ptr != buf_end)
  {
    line_ptr = std::find_if(line_ptr, buf_end, [](char C) { return C == '\n' ; });
    std::advance(line_ptr, 1); //< Skips new line.
    offsets.emplace_back(static_cast<unsigned>(line_ptr - buf_begin));
    Ensures(line_ptr <= buf_end);
  }

  return offsets;
}

auto SourceManager::from_file(std::string_view source_path) -> std::optional<SourceManager>
{
  std::optional<SourceManager> src_mgr;

  if (auto file_content = read_stream_utf8(source_path))
  {
    auto ln_offsets = calc_line_offsets(*file_content);
    src_mgr = SourceManager(std::move(ln_offsets), std::move(*file_content));
  }

  return src_mgr;
}

auto SourceManager::get_text(SourceRange range) const -> std::string_view
{
  Expects(!line_offsets.empty() && "Line offsets need to be calculated!");
  return get_text().substr(range.start.offset, range.end.offset - range.start.offset);
}

auto SourceManager::get_line_text(SourceLocation loc) const -> std::string_view
{
  Expects(!line_offsets.empty() && "Line offsets need to be calculated!");

  auto line_it = std::find_if(line_offsets.begin(), line_offsets.end(),
                              [loc] (SourceLocation ln_loc) { return ln_loc >= loc; });
  std::advance(line_it, -1);

  Ensures(line_it != line_offsets.end());

  auto line_start = *line_it;
  auto line_end = *std::next(line_it);

  return get_text(SourceRange(line_start, line_end));
}

auto SourceManager::get_linecol(SourceLocation loc) const -> std::pair<unsigned, unsigned>
{
  Expects(!line_offsets.empty() && "Line offsets need to be calculated!");

  auto line_it = std::find_if(line_offsets.begin(), line_offsets.end(),
                             [loc] (SourceLocation ln_loc) { return ln_loc >= loc; });
  std::advance(line_it, -1);

  Ensures(line_it != line_offsets.end());

  auto line_num = static_cast<unsigned>(line_it - line_offsets.begin() + 1);
  auto col_num = static_cast<unsigned>(line_it->offset + loc.offset + 1);

  return std::pair(line_num, col_num);
}

} // namespace ccompiler
