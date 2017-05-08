#include "program.hpp"
#include "cpp/contracts.hpp"
#include "cpp/format.hpp"
#include "cpp/string_view.hpp"
#include "utils/stream.hpp"

auto format_error_message(const string_view& error_from,
                          optional<LineInfo> line_info,
                          DiagnosticSeverity severity, const string_view& msg)
  -> std::string
{
  std::string message;
  message.reserve(128);

  if (!error_from.empty())
  {
    message += std::string(error_from);
    message.push_back(':');
  }

  if (line_info.has_value())
  {
    message +=
      fmt::format("{}:{}:", line_info->pos.line_no, line_info->pos.column_no);
  }

  message += [&] {
    switch (severity)
    {
      case DiagnosticSeverity::Note:
        return " note: ";
      case DiagnosticSeverity::Warning:
        return " warning: ";
      case DiagnosticSeverity::Error:
        return " error: ";
      case DiagnosticSeverity::Fatal:
        return " fatal: ";
      default:
        Unreachable();
    }
  }();

  message += std::string(msg);
  message.push_back('\n');

  if (line_info.has_value())
  {
    auto[line, pos, length] = *line_info;
    auto[lineno, colno] = pos;

    std::string highlight;
    highlight.reserve(colno + length);

    for (size_t i = 0; i < colno; ++i)
    {
      highlight.push_back(line[i] == '\t' ? '\t' : ' ');
    }

    highlight.back() = '^';
    highlight += std::string(length - 1, '~');

    message +=
      fmt::format("{}\n{}\n", std::string(line.begin(), line.end()), highlight);
  }

  return message;
}
