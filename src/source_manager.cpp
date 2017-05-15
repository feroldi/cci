#include <stdexcept>
#include "cpp/format.hpp"
#include "utils/stream.hpp"
#include "source_manager.hpp"

SourceManager::SourceLineCache::SourceLineCache(SourceRange range)
{
  auto line_begin = range.begin();
  auto line_end = range.begin();

  while (line_end != range.end())
  {
    line_end =
      std::find_if(line_end, range.end(), [](char c) { return c == '\n'; });
    this->line_offsets.emplace_back(line_begin, line_end);
    line_begin = line_end + 1;
    std::advance(line_end, 1);
  }
}

auto SourceManager::SourceLineCache::linecol_from_location(SourceLocation loc) const -> LineColumn
{
  Expects(!std::empty(this->line_offsets));
  Expects(loc >= this->line_offsets.front().begin());
  Expects(loc <= this->line_offsets.back().end());

  for (auto it = line_offsets.begin(); it != line_offsets.end(); ++it)
  {
    const auto& range = *it;

    if (range.contains(loc))
    {
      size_t lineno = std::distance(this->line_offsets.begin(), it) + 1;
      size_t colno = std::distance(range.begin(), loc) + 1;

      return LineColumn{lineno, colno};
    }
  }

  Unreachable();
}

auto SourceManager::SourceLineCache::line_from_location(SourceLocation loc) const -> SourceRange
{
  auto [line_no, col_no] = this->linecol_from_location(loc);
  return this->line_at(line_no);
}

auto SourceManager::SourceLineCache::line_at(size_t line_no) const -> SourceRange
{
  return line_offsets[line_no - 1];
}

auto SourceManager::from_path(std::string filepath) -> SourceManager
{
  if (auto content = utils::read_stream(filepath); content.has_value())
  {
    return SourceManager(std::move(filepath), std::move(*content));
  }

  throw std::runtime_error(fmt::format(
      "file '{}' is not valid or doesn't exist", std::move(filepath)));
}

