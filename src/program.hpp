#pragma once

#include <stdexcept>
#include "cpp/format.hpp"
#include "cpp/optional.hpp"
#include "source_manager.hpp"
#include "lexer.hpp"

namespace ccompiler
{

struct ProgramFailure : std::runtime_error
{
  using std::runtime_error::runtime_error;
};

enum class DiagLevel
{
  Note,
  Warning,
  Error,
  Fatal,
};

struct LineInfo
{
  SourceManager::LineColumn pos;
  SourceRange line;
  SourceRange range;
};

struct NoContextTag
{};

constexpr inline NoContextTag nocontext = {};

auto format_error(const char* from, DiagLevel, const optional<LineInfo>&, string_view description) -> std::string;

template <typename... Args>
auto format_error(const char* from, DiagLevel level, const optional<LineInfo>& info, const char* format, Args&&... args) -> std::string
{
  return format_error(from, level, info, fmt::format(format, std::forward<Args>(args)...));
}

template <typename... Args>
inline auto format_error(NoContextTag, DiagLevel level, const char* format, Args&&... args) -> std::string
{
  return format_error(
    nullptr, level, nullopt,
    fmt::format(format, std::forward<Args>(args)...));
}

template <typename... Args>
inline auto format_error(const TokenStream::TokenDebug& context, DiagLevel level,
                         const char* format, Args&&... args)
  -> std::string
{
  const auto line = context.source.line_range_at(context.pos.line_no);

  return format_error(
    context.source.get_name().c_str(), level, LineInfo{context.pos, line, context.range},
    fmt::format(format, std::forward<Args>(args)...));
}

struct Options
{
  bool pedantic = false;
  bool pedantic_errors = false; //< implies pedantic
  bool warning_as_error = false;
};

struct ProgramContext
{
  const Options opts;

private:
  std::FILE* output;

  std::size_t error_count = 0;
  std::size_t warn_count = 0;
  std::size_t fatal_count = 0;
  const std::size_t max_errors = 32;

  void put(std::string msg) const
  {
    std::fprintf(this->output, "%s\n", msg.c_str());
  }

public:
  explicit ProgramContext(const Options& opts, std::FILE* log = stderr)
    : opts(opts), output(log)
  {}

  template <typename Context, typename... Args>
  void error(Context&& context, Args&&... args)
  {
    this->put(format_error(std::forward<Context>(context), DiagLevel::Error, std::forward<Args>(args)...));
    this->error_count += 1;

    if (this->error_count >= this->max_errors)
    {
      this->fatal("too many errors [max = {}]", this->max_errors);
    }
  }

  template <typename Context, typename... Args>
  void warn(Context&& context, Args&&... args)
  {
    if (this->opts.warning_as_error)
    {
      this->error(std::forward<Context>(context), std::forward<Args>(args)...);
    }
    else
    {
      this->put(format_error(std::forward<Context>(context), DiagLevel::Warning, std::forward<Args>(args)...));
      this->warn_count += 1;
    }
  }

  template <typename Context, typename... Args>
  void pedantic(Context&& context, Args&&... args)
  {
    if (this->opts.pedantic_errors)
    {
      this->error(std::forward<Context>(context), std::forward<Args>(args)...);
    }
    else if (this->opts.pedantic)
    {
      this->warn(std::forward<Context>(context), std::forward<Args>(args)...);
    }
  }

  template <typename... Args>
  [[noreturn]] void fatal(const char* format, Args&&... args)
  {
    this->put(format_error(nocontext, DiagLevel::Fatal, format, std::forward<Args>(args)...));
    this->fatal_count += 1;
    throw ProgramFailure("fatal error occurred");
  }
};

} // namespace ccompiler
