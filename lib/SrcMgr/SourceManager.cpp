#if 0
#include <vector>

namespace ccompiler::SrcMgr {

static auto cache_lines_slow(const util::MemoryBuffer &buffer) -> std::vector<SourceRange>
{
  std::vector<SourceRange> line_cache;
  auto buf_begin = buffer.begin();
  auto buf_end = buffer.end();
  auto line_ptr = buffer.begin();

  while (line_ptr != buf_end)
  {
    auto line_start = buf_ptr;
    line_ptr = std::find_if(std::next(line_ptr), buf_end,
                            [](char C) { return C == '\n' || C == '\r'; });

    line_cache.emplace_back(line_start, line_ptr);
  }

  return line_cache;
}

auto SourceManager::get_line_location(SourceLocation L) const -> LineLocationInfo
{
  Expects(!line_cache.empty() && "Line cache has to be initialized.");

  signed line_num = 0; //< Current line number in the iteration.

  // TODO
  for (const auto &R : line_cache)
  {
    ++line_num;

    if (L >= R.begin() && L < R.end())
    {
      LineLocationInfo LLI;
      LLI.range = R;
      LLI.lineNo = line_num;
      LLI.colNo = R.loc - L.loc + 1;

      return LLI;
    }
  }

  Unreachable();
}

} // namespace ccompiler::SrcMgr
#endif
