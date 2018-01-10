#include "cci/basic/diagnostics.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/variant.hpp"
#include "fmt/format.h"
#include <cstdio>
#include <string>
#include <string_view>

namespace cci {

constexpr auto to_string(CompilerDiagnostics::Level level) -> std::string_view
{
  switch (level)
  {
    case CompilerDiagnostics::Level::Ignore:
      return "ignored";
    case CompilerDiagnostics::Level::Note:
      return "note";
    case CompilerDiagnostics::Level::Mention:
      return "mention";
    case CompilerDiagnostics::Level::Warning:
      return "warning";
    case CompilerDiagnostics::Level::Error:
      return "error";
    case CompilerDiagnostics::Level::Fatal:
      return "fatal error";
  }
}

static auto format_error([[maybe_unused]] const DiagnosticsOptions &opts,
                         std::string_view from, size_t line_num,
                         size_t column_num, std::string_view type,
                         std::optional<FullSourceLoc> source,
                         std::string_view message) -> std::string
{
  std::string out;
  out.reserve(256);

  if (!from.empty())
  {
    out += from;
    out += ':';
  }
  else
  {
    out += "cci:";
  }

  if (line_num)
  {
    out += std::to_string(line_num);
    out += ':';

    if (column_num)
    {
      out += std::to_string(column_num);
      out += ':';
    }
  }

  if (!type.empty())
  {
    out += ' ';
    out += type;
    out += ": ";
  }

  if (!message.empty())
  {
    out += message;

    if (source)
    {
      cci_expects(line_num && column_num);

      const auto snippet = source->text_line();
      std::string carret;
      carret.reserve(column_num);

      for (size_t i = 0; i < column_num; ++i)
        carret.push_back(snippet[i] != '\t' ? ' ' : '\t');
      carret.back() = '^';

      out += fmt::format("\n{}\n{}", snippet, carret);
    }
  }

  return out;
}

static auto format_error(const DiagnosticsOptions &opts,
                         const SourceManager &src_mgr,
                         CompilerDiagnostics::Level level,
                         CompilerDiagnostics::Context context,
                         std::string_view message) -> std::string
{
  return std::visit(overloaded {
    [&] (nocontext_t) {
      return format_error(opts, "", 0ull, 0ull, to_string(level), std::nullopt, message);
    },
    [&] (SourceLocation location) {
      const FullSourceLoc loc(src_mgr, location);
      const auto [line_num, col_num] = loc.translate_to_linecolumn();
      const auto filename = loc.loaded_from_file() ? loc.file_path().c_str() : "<source>";
      return format_error(opts, filename, line_num, col_num, to_string(level),
                          loc, message);
    }
  }, context);
}

auto CompilerDiagnostics::get_output_level(Severity severity) const
  -> Level
{
  switch (severity)
  {
    case Severity::Note:
      return Level::Note;
    case Severity::Mention:
      if (opts.is_verbose)
        return Level::Mention;
      else
        return Level::Ignore;
    case Severity::Extension:
      if (opts.is_pedantic_as_error)
        return Level::Error;
      else if (opts.is_pedantic)
        return Level::Warning;
      else
        return Level::Ignore;
    case Severity::Warning:
      if (opts.is_warning_as_error)
        return Level::Error;
      else
        return Level::Warning;
    case Severity::Error:
      if (error_count >= max_errors)
        return Level::Fatal;
      else
        return Level::Error;
  }
}

void CompilerDiagnostics::report_message(Severity severity, Context context,
                                         std::string_view message)
{
  const auto level = get_output_level(severity);

  if (level != Level::Ignore)
  {
    const auto out =
      format_error(opts, src_mgr, level, std::move(context), message);
    std::fprintf(this->out_stream, "%s\n", out.c_str());

    if (level == Level::Warning)
      ++warn_count;
    else if (level == Level::Error && ++error_count >= max_errors)
      report(nocontext, diag::Common::fatal_too_many_errors);
    else if (level == Level::Fatal)
    {
      ++fatal_count;
      throw CompilerFatalError();
    }
  }
}

} // namespace cci
