#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include "fmt/format.h"
#include <cstdint>
#include <cstdio>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <variant>

namespace cci {

// DiagnosticsOptions - Options and configuration used by CompilerDiagnostics.
struct DiagnosticsOptions
{
  bool is_warning_as_error = false; //< Turns warnings into errors.
  bool is_pedantic = false; //< Enables pedantic warnings.
  bool is_pedantic_as_error =
    false; //< Turns pedantic warnings into errors (implies is_pedantic).
  bool is_verbose = false; //< Enables more information shown to the user.

  DiagnosticsOptions() = default;
};

struct CompilerFatalError : std::exception
{
  using std::exception::exception;
};

// Severity - Importance level (severity) of diagnostics.
//
// Diagnostics are emitted with a severity level,
// i.e. they may have differing importance based on their severity.
// Severities are later mapped to a level (`CompilerDiagnostics::Level`),
enum class Severity
{
  Note, //< Adds information to previous diagnostics.
  Mention, //< Generic information (could be thought of as verbose).
  Warning, //< Used when some code is valid but dubious.
  Extension, //< Used to issue when some code is nonportable due to extensions.
  Error, //< Issues ill-formed code.
};

struct ErrorCodeInfo
{
  Severity severity;
  const char *format;
};

// Type trait used to extend CompilerDiagnostics
// with a user-defined enum type.
template <typename>
struct diagnostics_error_code;

template <typename>
struct is_diagnostics_error_code : std::false_type
{
};

namespace diag {
enum Common
{
#define DIAG(CODE, LEVEL, FORMAT) CODE,
#include "cci/basic/diagnostics_common.inc"
#undef DIAG
};
} // namespace diag

template <>
struct diagnostics_error_code<diag::Common>
{
  constexpr static auto info(diag::Common code) -> ErrorCodeInfo
  {
#define DIAG(CODE, LEVEL, FORMAT)                                              \
  case diag::CODE:                                                             \
    return {LEVEL, FORMAT};
    switch (code)
    {
#include "cci/basic/diagnostics_common.inc"
    }
#undef DIAG
  }
};

template <>
struct is_diagnostics_error_code<diag::Common> : std::true_type
{
};

struct nocontext_t
{};
inline constexpr nocontext_t nocontext = {};

template <typename T>
inline constexpr bool is_diagnostics_error_code_v =
  is_diagnostics_error_code<T>::value;

// CompilerDiagnostics - This handles how diagnostics are showed
// to the user, as well as provides a minimum API for error diagnosing.
class CompilerDiagnostics
{
  const DiagnosticsOptions &opts;
  const SourceManager &src_mgr;

public:
  using Context = std::variant<nocontext_t, SourceLocation>;

  // Level - Diagnostics importance level.
  //
  // Every diagnostic has a level of importance. Depending on which it is,
  // a diagnostic may prevent compilation from continuing or not, or even
  // not be reported at all.
  enum class Level
  {
    Ignore, //< Ignores the diagnostic (doesn't report it).
    Note, //< Extra information for previous diagnostic.
    Mention, //< Extra information (verbose).
    Warning, //< Compilation may continue.
    Error, //< Stops compiling process.
    Fatal, //< Stops the whole program.
  };

  explicit CompilerDiagnostics(const DiagnosticsOptions &opts,
                               const SourceManager &src_mgr,
                               std::FILE *out_stream = stderr)
    : opts(opts), src_mgr(src_mgr), out_stream(out_stream)
  {}

  // Sets maximum errors emitted permitted. A fatal error
  // is issued when the error count exceeds it.
  //
  // \throws CompilerFatalError if error count exceeds `max_errors`.
  void set_max_errors(uint64_t max_errors)
  {
    this->max_errors = max_errors;
    if (this->error_count >= this->max_errors)
      report(nocontext, diag::Common::fatal_too_many_errors);
  }

  template <typename... Args>
  void report(Severity severity, Context context, const char *format,
              Args &&... args)
  {
    report_message(severity, std::move(context),
                   fmt::format(format, std::forward<Args>(args)...));
  }

  template <typename ErrorCode, typename... Args>
  auto report(Context context, ErrorCode ec, Args &&... args)
    -> std::enable_if_t<is_diagnostics_error_code_v<ErrorCode>>
  {
    const auto [severity, format] = diagnostics_error_code<ErrorCode>::info(ec);
    report(severity, std::move(context), format, std::forward<Args>(args)...);
  }

  // Checks whether there has been any reported errors.
  auto has_errors() const -> bool
  {
    return error_count != 0 || fatal_count != 0;
  }

  // Checks whether there has been any reported warnings.
  auto has_warnings() const -> bool { return warn_count != 0; }

private:
  std::FILE *out_stream; //< Stream to which diagnostics are outputted.

  uint64_t warn_count = 0;
  uint64_t error_count = 0;
  uint64_t fatal_count = 0;

  uint64_t max_errors = 128; //< Maximum count of emitted errors possible.

  // Emits a complete diagnostic to the user.
  void report_message(Severity, Context, std::string_view message);

  // Returns an output level for a mapped severity.
  auto get_output_level(Severity) const -> Level;
};

} // namespace cci
