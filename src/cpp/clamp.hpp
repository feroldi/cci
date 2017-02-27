#pragma once

#include <functional>

namespace clamp_detail
{
template<typename T, typename U, typename Compare> 
inline constexpr auto min(T&& a, U&& b, Compare&& comp)
{
    return std::forward<Compare>(comp)(b, a) ? std::forward<U>(b) : std::forward<T>(a);
}

template<typename T, typename U, typename Compare> 
inline constexpr auto max(T&& a, U&& b, Compare&& comp)
{
    return std::forward<Compare>(comp)(b, a) ? std::forward<T>(a) : std::forward<U>(b);
}
} // namespace detail

template<typename T, typename U, typename V, typename Compare>
inline constexpr auto clamp(T&& v, U&& lo, V&& hi, Compare&& comp)
{
    return comp(v, hi)
      ? clamp_detail::max(std::forward<T>(v), std::forward<U>(lo), std::forward<Compare>(comp))
      : clamp_detail::min(std::forward<T>(v), std::forward<V>(hi), std::forward<Compare>(comp));
}

template<typename T, typename U, typename V>
inline constexpr auto clamp(T&& v, U&& lo, V&& hi)
{
    return clamp(std::forward<T>(v), std::forward<U>(lo), std::forward<V>(hi), std::less<>());
}

