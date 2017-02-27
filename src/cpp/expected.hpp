#pragma once

#include <utility>
#include <type_traits>
#include <eggs/variant.hpp>

namespace expected
{

template <typename S, typename E>
struct Expected;

namespace traits
{
  template <typename S, typename E>
  inline constexpr std::true_type is_expected_test(Expected<S, E>*);
  inline constexpr std::false_type is_expected_test(...);

  template <typename S, typename E>
  static constexpr auto decl_expect(Expected<S, E>*) -> Expected<S, E>;
  static constexpr auto decl_expect(...) -> void;

  template <typename T>
  struct is_expected : decltype(is_expected_test(std::declval<T*>())) {};

  template <typename T>
  using decl_expect_t = decltype(decl_expect(std::declval<T*>()));

  template <typename T>
  struct expect_base : std::enable_if<is_expected<decl_expect_t<T>>::value, decl_expect_t<T>> {};

  template <typename T>
  using expect_base_t = typename expect_base<T>::type;

  template <typename E>
  struct traits : traits<expect_base_t<E>> {};

  template <typename S, typename E>
  struct traits<Expected<S, E>>
  {
    using success_type = S;
    using error_type = E;
  };

  template <typename E>
  using success_t = typename traits<E>::success_type;

  template <typename E>
  using error_t = typename traits<E>::error_type;
} // namespace traits

namespace monads
{
  template <typename T, typename Variant>
  inline constexpr auto is(Variant&& v) -> bool
  {
    return eggs::variants::apply([] (auto const& obj) {
      return std::is_same<T, std::decay_t<decltype(obj)>>::value;
    }, std::forward<Variant>(v));
  }

  template <typename Expect>
  inline constexpr auto is_success(Expect&& e) -> bool
  {
    return is<traits::success_t<std::decay_t<Expect>>>(std::forward<Expect>(e));
  }

  template <typename Expect>
  inline constexpr auto is_failure(Expect&& e) -> bool
  {
    return is<traits::error_t<std::decay_t<Expect>>>(std::forward<Expect>(e));
  }

  template <typename Expect>
  inline constexpr auto success(Expect&& e) -> decltype(auto)
  {
    return eggs::variants::get<traits::success_t<std::decay_t<Expect>>>(std::forward<Expect>(e));
  }

  template <typename Expect>
  inline constexpr auto failure(Expect&& e) -> decltype(auto)
  {
    return eggs::variants::get<traits::error_t<std::decay_t<Expect>>>(std::forward<Expect>(e));
  }

  template <typename Expected, typename Otherwise>
  inline constexpr auto success_or(Expected&& e, Otherwise&& v) -> decltype(auto)
  {
    if (is_success(e))
      return success(std::forward<Expected>(e));
    else
      return std::forward<Otherwise>(v);
  }

  template <typename S, typename E, typename Expect, typename Function>
  inline constexpr auto apply_then(Expect&& e, Function&& f) -> std::result_of_t<Function(S)>
  {
    if (is<S>(e))
      return std::forward<Function>(f)(eggs::variants::get<S>(std::forward<Expect>(e)));

    if constexpr (traits::is_expected<std::result_of_t<Function(S)>>::value)
      return eggs::variants::get<E>(std::forward<Expect>(e));
  }

  template <typename Expect, typename Function>
  inline constexpr auto apply_map(Expect&& e, Function&& f)
    -> Expected<std::result_of_t<Function(traits::success_t<std::decay_t<Expect>>)>, traits::error_t<std::decay_t<Expect>>>
  {
    if (is_success(e))
      return std::forward<Function>(f)(success(std::forward<Expect>(e)));
    else
      return failure(std::forward<Expect>(e));
  }

  template <typename Expect, typename Function>
  inline constexpr auto apply_map_error(Expect&& e, Function&& f)
    -> Expected<traits::success_t<std::decay_t<Expect>>, std::result_of_t<Function(traits::error_t<std::decay_t<Expect>>)>>
  {
    if (is_failure(e))
      return std::forward<Function>(f)(failure(std::forward<Expect>(e)));
    else
      return success(std::forward<Expect>(e));
  }
} // namespace monads

template <typename S, typename E>
struct Expected : eggs::variants::variant<S, E>
{
  static_assert(!std::is_same<S, E>::value, "success and error types shall not equal.");

  using eggs::variants::variant<S, E>::variant;

  template <typename Fallback>
  constexpr decltype(auto) success_or(Fallback&& v) const& noexcept
  { return monads::success_or(*this, std::forward<Fallback>(v)); }

  template <typename Fallback>
  constexpr decltype(auto) success_or(Fallback&& v) && noexcept
  { return monads::success_or(std::move(*this), std::forward<Fallback>(v)); }

  template <typename Function>
  constexpr decltype(auto) then(Function&& f) const& noexcept
  { return monads::apply_then<S, E>(*this, std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) then(Function&& f) && noexcept
  { return monads::apply_then<S, E>(std::move(*this), std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) then_error(Function&& f) const& noexcept
  { return monads::apply_then<E, S>(*this, std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) then_error(Function&& f) && noexcept
  { return monads::apply_then<E, S>(std::move(*this), std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) map(Function&& f) const& noexcept
  { return monads::apply_map(*this, std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) map(Function&& f) && noexcept
  { return monads::apply_map(std::move(*this), std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) map_error(Function&& f) const& noexcept
  { return monads::apply_map_error(*this, std::forward<Function>(f)); }

  template <typename Function>
  constexpr decltype(auto) map_error(Function&& f) && noexcept
  { return monads::apply_map_error(std::move(*this), std::forward<Function>(f)); }
};

} // namespace expected

