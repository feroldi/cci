#pragma once

#include "cpp/format.hpp"
#include "cpp/optional.hpp"
#include "lexer.hpp"

struct Options
{
  bool pedantic = false;
  bool pedantic_errors = false;
};

// Tag used by format_error when there's no reference point.
struct no_context_tag
{
  explicit no_context_tag() noexcept = default;
};

inline constexpr no_context_tag no_context = no_context_tag{};

enum class DiagnosticSeverity
{
  Note,
  Warning,
  Error,
  Fatal,
};

struct LineInfo
{
  SourceLocation line;
  TextStream::LineColumn pos;
  size_t length;
};

auto format_error_message(const string_view& error_from, optional<LineInfo>,
                          DiagnosticSeverity, const string_view& msg)
  -> std::string;

template <typename... Args>
inline auto format_error(no_context_tag, DiagnosticSeverity ds,
                         const string_view& format, Args&&... args)
  -> std::string
{
  return format_error_message(
    "ccompiler", nullopt, ds,
    fmt::format(std::string(format), std::forward<Args>(args)...));
}

template <typename... Args>
inline auto format_error(const TokenInfo& context, DiagnosticSeverity ds,
                         const string_view& format, Args&&... args)
  -> std::string
{
  auto linecol = context.stream.linecol_from_source(context.source);
  auto line = context.stream.get_line(linecol.line_no);

  return format_error_message(
    context.stream.filename, LineInfo{line, linecol, context.source.size()}, ds,
    fmt::format(std::string(format), std::forward<Args>(args)...));
}

struct ProgramContext
{
private:
  const Options options;
  std::FILE* log_stream;

  void put(const std::string& msg) const
  {
    std::fprintf(log_stream, "%s\n", msg.c_str());
  }

public:
  explicit ProgramContext(const Options& opts, std::FILE* log)
    : options(opts), log_stream(log)
  {
  }

  template <typename Context, typename... Args>
  void error(Context&& c, Args&&... args)
  {
    this->put(format_error(std::forward<Context>(c), DiagnosticSeverity::Error,
                           std::forward<Args>(args)...));
  }
};
