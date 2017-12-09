#pragma once

#include <type_traits>

namespace cci {

template <typename T, typename Variant>
inline constexpr auto is(const Variant &v) -> bool
{
  return std::holds_alternative<T>(v);
}

template <typename... Ts>
struct overloaded : Ts...
{
  using Ts::operator()...;
};

template <typename... T>
overloaded(T...) -> overloaded<T...>;

} // namespace cci
