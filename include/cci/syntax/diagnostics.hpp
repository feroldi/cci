#pragma once

#include "cci/syntax/source_map.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/small_vector.hpp"
#include "fmt/format.h"
#include <atomic>
#include <functional>
#include <optional>
#include <string>

namespace cci::diag {
struct Handler;

// This is used to select between cases in a fmt::format string. For example,
// `fmt::format("this is {good|bad}", select(0))` will output `"this is good"`.
enum class select : size_t
{
};

struct Diagnostic
{
  srcmap::SourceLoc loc;
  std::string message;
  std::vector<srcmap::Range> ranges;

  Diagnostic(srcmap::SourceLoc loc, std::string message)
    : loc(loc), message(std::move(message))
  {}
};

struct DiagnosticBuilder
{
  DiagnosticBuilder(srcmap::SourceLoc loc, std::string message,
                    Handler &handler)
    : handler(handler)
    , diag(std::make_unique<Diagnostic>(loc, std::move(message)))
  {}

  DiagnosticBuilder(DiagnosticBuilder &&) = default;
  DiagnosticBuilder &operator=(DiagnosticBuilder &&) = default;

  auto range(srcmap::Range range) -> DiagnosticBuilder &
  {
    this->diag->ranges.push_back(range);
    return *this;
  }

  ~DiagnosticBuilder() noexcept(!CCI_ENABLE_CONTRACTS);

private:
  Handler &handler;
  std::unique_ptr<Diagnostic> diag;
};

struct Handler
{
  using Emitter = std::function<void(const Diagnostic &)>;

  explicit Handler(Emitter emitter, const srcmap::SourceMap &map)
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

  auto report(srcmap::ByteLoc loc, std::string message) -> DiagnosticBuilder
  {
    DiagnosticBuilder builder(this->map.lookup_source_location(loc),
                              std::move(message), *this);
    return builder;
  }

  auto has_errors() const -> bool { return this->err_count_ > 0; }
  auto err_count() const -> size_t { return this->err_count_.load(); }
  auto source_map() const -> const srcmap::SourceMap & { return this->map; }

  void set_emitter(Emitter emitter) { this->emitter = std::move(emitter); }

protected:
  Emitter emitter;
  friend struct DiagnosticBuilder;

  void emit(std::unique_ptr<Diagnostic> diag)
  {
    cci_expects(diag);
    this->emitter(*diag);
    this->bump_err_count();
  }

private:
  std::atomic<size_t> err_count_ = 0;
  const srcmap::SourceMap &map;

  void bump_err_count() { this->err_count_.fetch_add(1); }
};

auto ignoring_emitter() -> Handler::Emitter;

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
