#pragma once

#include <type_traits>

template <typename InvocableF>
struct scope_guard
{
  template <typename F>
  explicit scope_guard(F&& f) noexcept
    : callback(std::forward<F>(f))
  {}

  ~scope_guard()
  {
    if (this->should_run)
      this->callback();
  }

  void dismiss() noexcept { this->should_run = false; }

  // Scope guard should not be able to copy/move.
  scope_guard(const scope_guard&) = delete;
  scope_guard(scope_guard&&) = delete;
  scope_guard& operator= (const scope_guard&) = delete;
  scope_guard& operator= (scope_guard&&) = delete;

private:
  InvocableF callback;
  bool should_run = true;
};

template <typename F>
inline auto scope_exit(F&& f) noexcept -> scope_guard<std::decay_t<F>>
{
  return scope_guard<std::decay_t<F>>(std::forward<F>(f));
}
