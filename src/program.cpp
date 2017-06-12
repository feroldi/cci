#include "cpp/contracts.hpp"
#include "cpp/format.hpp"
#include "cpp/string_view.hpp"
#include "program.hpp"
#include "source_manager.hpp"

namespace ccompiler
{

auto format_error(const char* from, DiagLevel level, const optional<LineInfo>& line_info,
                  string_view description) -> std::string
{
  std::string message;
  message.reserve(256);

  if (from)
  {
    message += from;
    message.push_back(':');
  }
  else
  {
    message += "cc:";
  }

  if (line_info)
  {
    message +=
      fmt::format("{}:{}:", line_info->pos.line_no, line_info->pos.column_no);
  }

  message += [&] {
    switch (level)
    {
      case DiagLevel::Note:
        return " note: ";
      case DiagLevel::Warning:
        return " warning: ";
      case DiagLevel::Error:
        return " error: ";
      case DiagLevel::Fatal:
        return " fatal: ";
      default:
        Unreachable();
    }
  }();

  message += description.data();
  message.push_back('\n');

  if (line_info)
  {
    auto [pos, line, range] = *line_info;
    std::string highlight;
    highlight.reserve(pos.column_no + range.size());

    for (size_t i = 0; i < pos.column_no; ++i)
    {
      highlight.push_back(line[i] == '\t' ? '\t' : ' ');
    }

    highlight.back() = '^';

    if (range.size() > 1)
      highlight += std::string(range.size() - 1, '~');

    message +=
      fmt::format("{}\n{}", line.to_string(), highlight);
  }

  return message;
}

} // namespace ccompiler
