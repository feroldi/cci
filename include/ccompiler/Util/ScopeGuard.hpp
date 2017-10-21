#pragma once

#include <type_traits>

template <typename InvocableF>
struct ScopeGuard
{
  template <typename F>
  explicit ScopeGuard(F&& f) noexcept
    : callback(std::forward<F>(f))
  {}

  ~ScopeGuard()
  {
    if (should_run)
      callback();
  }

  void dismiss() noexcept { should_run = false; }

  // Scope guard should not be able to copy/move.
  ScopeGuard(const ScopeGuard&) = delete;
  ScopeGuard(ScopeGuard&&) = delete;
  ScopeGuard& operator= (const ScopeGuard&) = delete;
  ScopeGuard& operator= (ScopeGuard&&) = delete;

private:
  InvocableF callback;
  bool should_run = true;
};

template <typename F>
inline ScopeGuard<std::decay_t<F>> ScopeExit(F&& f) noexcept
{
  return ScopeGuard<std::decay_t<F>>(std::forward<F>(f));
}
