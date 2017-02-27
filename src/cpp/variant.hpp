#pragma once

#include <type_traits>
#include <eggs/variant.hpp>

using eggs::variants::variant;
using eggs::variants::get;

// In case we need to switch to another variant,
// just change the inner visit.
template <typename F, typename... V>
inline constexpr auto visit(F&& f, V&&... v)
  -> decltype(eggs::variants::apply(std::forward<F>(f), std::forward<V>(v)...))
{
  return eggs::variants::apply(std::forward<F>(f), std::forward<V>(v)...);
}

// i.e. std::holds_alternative.
template <typename T, typename Variant>
inline constexpr auto is(const Variant& v) -> bool
{
  return visit([] (const auto& obj) {
    return std::is_same<T, std::decay_t<decltype(obj)>>::value;
  }, v);
}

namespace detail
{
template <typename T, typename... Ts>
struct make_visitor_impl : T, make_visitor_impl<Ts...>
{
  using T::operator();
  using make_visitor_impl<Ts...>::operator();
  constexpr make_visitor_impl(T&& t, Ts&&... ts) noexcept :
    T{std::forward<T>(t)},
    make_visitor_impl<Ts...>{std::forward<Ts>(ts)...}
  {}
};

template <typename T>
struct make_visitor_impl<T> : T
{
  using T::operator();
  constexpr make_visitor_impl(T&& t) noexcept :
    T{std::forward<T>(t)}
  {}
};
} // namespace detail

// Make lambdas overloadable.
template <typename... Ts>
inline constexpr auto make_visitor(Ts&&... ts) noexcept
  -> detail::make_visitor_impl<std::decay_t<Ts>...>
{
  return {std::forward<Ts>(ts)...};
}

// Essentially calls visit(make_visitor(fs...), v)
template <typename V, typename... Fs>
inline constexpr auto match(V&& v, Fs&&... fs)
{
  return visit(make_visitor(std::forward<Fs>(fs)...), std::forward<V>(v));
}

