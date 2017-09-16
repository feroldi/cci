#pragma once

#include <stdexcept>
#include "ccompiler/include/Cpp/optional.hpp"
#include "ccompiler/include/Util/Format.hpp"
#include "ccompiler/include/SourceManager.hpp"
#include "ccompiler/include/TokenStream.hpp"

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

struct no_context_t { constexpr explicit no_context_t() = default; };
inline constexpr no_context_t no_context = no_context_t();

auto base_format_error(const char* from, DiagLevel, const optional<LineInfo>&, string_view description) -> std::string;

template <typename... Args>
inline auto format_error(const char* from, DiagLevel level, const optional<LineInfo>& info, const char* format, Args&&... args) -> std::string;

template <typename... Args>
inline auto format_error(no_context_t, DiagLevel level, const char* format, Args&&... args) -> std::string;

template <typename... Args>
inline auto format_error(const TokenStream::TokenDebug& context, DiagLevel level, const char* format, Args&&... args) -> std::string;

struct Options
{
  // Compiler options
  bool pedantic = false;
  bool pedantic_errors = false; //< implies pedantic
  bool warning_as_error = false;
  bool syntax_only = false;
  bool show_help = false; //< exits program if true
  bool dump_ast = false;
  std::string output_filename = "a.out";
  uint32_t optimization_level = 0;
  std::vector<std::string> source_filenames;

  static auto parse_arguments(int argc, char**& argv) -> Options;
  void dump(std::FILE* out) const;
};

struct ProgramContext
{
  const Options opts;

  explicit ProgramContext(const Options& opts, std::FILE* log = stderr)
    : opts(opts), output(log)
  {}

  template <typename Context, typename... Args>
  void note(Context&& context, Args&&... args)
  {
    this->put(format_error(std::forward<Context>(context), DiagLevel::Note, std::forward<Args>(args)...));
  }

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
    this->put(format_error(no_context, DiagLevel::Fatal, format, std::forward<Args>(args)...));
    this->fatal_count += 1;
    throw ProgramFailure("fatal error occurred");
  }

  auto has_errors() const noexcept -> bool
  {
    return this->error_count != 0 || this->fatal_count != 0;
  }

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
};

template <typename... Args>
inline auto format_error(const char* from, DiagLevel level, const optional<LineInfo>& info, const char* format, Args&&... args) -> std::string
{
  return base_format_error(from, level, info, fmt::format(format, std::forward<Args>(args)...));
}

template <typename... Args>
inline auto format_error(no_context_t, DiagLevel level, const char* format, Args&&... args) -> std::string
{
  return base_format_error(
    nullptr, level, nullopt,
    fmt::format(format, std::forward<Args>(args)...));
}

template <typename... Args>
inline auto format_error(const TokenStream::TokenDebug& context, DiagLevel level,
                         const char* format, Args&&... args)
  -> std::string
{
  const auto line = context.source.line_range_at(context.pos.line_no);

  return base_format_error(
    context.source.filename.c_str(), level, LineInfo{context.pos, line, context.range},
    fmt::format(format, std::forward<Args>(args)...));
}

} // namespace ccompiler
