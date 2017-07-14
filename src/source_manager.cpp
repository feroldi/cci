#include <stdexcept>
#include <algorithm>
#include "cpp/format.hpp"
#include "utils/stream.hpp"
#include "source_manager.hpp"

namespace ccompiler
{

SourceManager::SourceLineCache::SourceLineCache(SourceRange range)
{
  //Expects(*std::prev(range.end()) == '\n');
  auto line_begin = range.begin();
  auto line_end = std::find_if(range.begin(), range.end(), [](char c) { return c == '\n'; });

  if (line_end == range.end())
  {
    this->offsets.emplace_back(line_begin, line_end);
  }
  else
  {
    while (line_end != range.end())
    {
      this->offsets.emplace_back(line_begin, line_end);
      std::advance(line_end, 1);
      line_begin = line_end;
      line_end = std::find_if(line_end, range.end(), [](char c) { return c == '\n'; });

      if (line_end == range.end())
      {
        this->offsets.emplace_back(line_begin, line_end);
        break;
      }
    }
  }
}

auto SourceManager::linecol_from_location(SourceLocation loc) const -> LineColumn
{
  Expects(!std::empty(this->line_cache.offsets));
  Expects(loc >= this->line_cache.offsets.front().begin());
  Expects(loc <= this->line_cache.offsets.back().end());

  for (auto it = this->line_cache.offsets.begin(); it != this->line_cache.offsets.end(); ++it)
  {
    const auto& range = *it;

    if (range.contains(loc))
    {
      size_t lineno = std::distance(this->line_cache.offsets.begin(), it) + 1;
      size_t colno = std::distance(range.begin(), loc) + 1;

      return LineColumn{lineno, colno};
    }
  }

  Unreachable();
}

auto SourceManager::line_range_from_location(SourceLocation loc) const -> SourceRange
{
  auto [line_no, col_no] = this->linecol_from_location(loc);
  return this->line_range_at(line_no);
}

auto SourceManager::line_range_at(size_t line_no) const -> SourceRange
{
  return this->line_cache.offsets[line_no - 1];
}

auto SourceManager::from_path(std::string filepath) -> SourceManager
{
  if (auto content = utils::read_stream(filepath); content)
  {
    return SourceManager(std::move(filepath), std::move(*content));
  }

  throw std::runtime_error(fmt::format(
      "file '{}' is not valid or doesn't exist", std::move(filepath)));
}

} // namespace ccompiler
