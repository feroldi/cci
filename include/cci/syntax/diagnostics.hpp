#pragma once

#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/small_vector.hpp"
#include "fmt/format.h"
#include <atomic>
#include <functional>
#include <optional>
#include <string>
#include <variant>

namespace cci::diag {
struct Handler;

/// Information about a diagnostic.
struct Diagnostic
{
  using Arg = std::variant<Category, srcmap::Range>;

  srcmap::SourceLoc loc; ///< Location from where the diagnostic was reported.
  std::string message; ///< The diagnostic message.
  std::vector<srcmap::Range> ranges; ///< Location ranges that are related to
                                     ///< this diagnostic.
  std::vector<Arg> args; ///< Arguments for the format message.

  Diagnostic(srcmap::SourceLoc loc, std::string message)
    : loc(loc), message(std::move(message))
  {}
};

/// Helper class to construct a `Diagnostic`.
struct DiagnosticBuilder
{
  DiagnosticBuilder(srcmap::SourceLoc loc, std::string message,
                    Handler &handler)
    : handler(handler)
    , diag(std::make_unique<Diagnostic>(loc, std::move(message)))
  {}

  DiagnosticBuilder(DiagnosticBuilder &&) = default;
  DiagnosticBuilder &operator=(DiagnosticBuilder &&) = default;

  /// Adds a source range to give more context to the diagnostic.
  auto range(srcmap::Range range) -> DiagnosticBuilder &
  {
    this->diag->ranges.push_back(range);
    return *this;
  }

  /// Adds an argument to the diagnostic.
  auto arg(Diagnostic::Arg arg) -> DiagnosticBuilder &
  {
    this->diag->args.push_back(std::move(arg));
    return *this;
  }

  /// Hands the diagnostic to the handler.
  ~DiagnosticBuilder() noexcept(!CCI_CONTRACTS);

private:
  Handler &handler; ///< The handler that will be handed the diagnostic.
  std::unique_ptr<Diagnostic> diag; ///< Diagnostic being constructed.
};

/// A diagnostic handler.
//
/// Diagnostics are reported and treated through this handler. Once reported, a
/// diagnostic is passed to an emitter (a function callback) responsible to
/// treat it. It may do anything like aborting compilation process, or just
/// completely ignore diagnostics.
struct Handler
{
  /// The emitter type.
  using Emitter = std::function<void(const Diagnostic &)>;

  /// Constructs a handler with a given `Emitter` and the `SourceMap` associated
  /// with it.
  Handler(Emitter emitter, const srcmap::SourceMap &map)
    : emitter(std::move(emitter)), map(map)
  {}

  Handler(Handler &&other) noexcept
    : emitter(std::move(other.emitter))
    , err_count_(other.err_count_.load())
    , map(other.map)
  {}
  Handler &operator=(Handler &&other) noexcept
  {
    std::swap(other, *this);
    return *this;
  }

  /// Helper function to facilitate the construction of a `DiagnosticBuilder`.
  auto report(srcmap::ByteLoc loc, std::string message) -> DiagnosticBuilder
  {
    DiagnosticBuilder builder(this->map.lookup_source_location(loc),
                              std::move(message), *this);
    return builder;
  }

  /// Checks whether there has been any errors reported.
  auto has_errors() const -> bool { return this->err_count_ > 0; }

  /// Returns how many errors have been reported.
  auto err_count() const -> size_t { return this->err_count_.load(); }

  /// Returns the source map associated with it.
  auto source_map() const -> const srcmap::SourceMap & { return this->map; }

  /// Sets a new emitter to be called at diagnostic emission, overriding the old
  /// one.
  void set_emitter(Emitter emitter) { this->emitter = std::move(emitter); }

protected:
  Emitter emitter;
  friend struct DiagnosticBuilder;

  /// Emits a diagnostic.
  void emit(std::unique_ptr<Diagnostic> diag)
  {
    cci_expects(diag);
    this->emitter(*diag);
    this->bump_err_count();
  }

private:
  std::atomic<size_t> err_count_ = 0;
  const srcmap::SourceMap &map;

  /// Increases the error count by one.
  void bump_err_count() { this->err_count_.fetch_add(1); }
};

/// Returns a diagnostic emitter that just ignores diagnostics.
auto ignoring_emitter() -> Handler::Emitter;

/// Selection type for `fmt::format`.
//
/// This is used to select between cases in a format string. For example,
///
///     fmt::format("this is {good|bad}", select(0))
///
/// will output `"this is good"`.
enum class select : size_t
{
};

} // namespace cci::diag

namespace fmt {
template <>
struct formatter<cci::diag::select>
{
  small_vector<std::string, 4> cases;

  auto parse(parse_context &ctx) -> parse_context::iterator
  {
    auto it = begin(ctx); // "good|bad|ugly"
    auto prev = it;
    while (it != end(ctx))
    {
      if (*it == '}')
        break;
      if (*it == '|')
      {
        cases.emplace_back(prev, it);
        prev = std::next(it);
      }
      ++it;
    }
    cases.emplace_back(prev, it);
    return it;
  }

  template <typename FormatContext>
  auto format(cci::diag::select opt, FormatContext &ctx)
  {
    if (empty(cases))
      throw format_error("selector format is empty");
    if (opt >= cci::diag::select(cases.size()))
      throw format_error(::fmt::format("selector({}) doesn't exist, max is {}",
                                       static_cast<size_t>(opt), cases.size()));
    return format_to(begin(ctx), "{}", cases[static_cast<size_t>(opt)]);
  }
};
} // namespace fmt
