// Copyright 2017, by Mário Feroldi
//
// observer_ptr (Library Fundamentals TS v2)
// http://en.cppreference.com/w/cpp/experimental/observer_ptr
//
// This code is licensed under the MIT License (MIT).

#pragma once

#include <cstddef>
#include <type_traits>
#include <functional>

namespace cci
{

template <typename T>
struct observer_ptr
{
  using element_type = T;

  constexpr observer_ptr() noexcept : ptr(nullptr) {}
  constexpr observer_ptr(std::nullptr_t) noexcept : ptr(nullptr) {}
  constexpr explicit observer_ptr(element_type* p) noexcept : ptr(p) {}

  template <
    typename W2,
    std::enable_if_t<std::is_convertible<W2*, element_type*>::value>...>
  observer_ptr(observer_ptr<W2> other) noexcept : ptr(other.get()) {}

  observer_ptr(const observer_ptr&) = default;
  observer_ptr(observer_ptr&&) = default;

  /// Modifiers

  constexpr auto release() noexcept -> element_type*
  {
    element_type* p = this->get();
    this->reset();
    return p;
  }

  constexpr void reset(element_type* p = nullptr) noexcept { this->ptr = p; }

  constexpr void swap(observer_ptr& other) noexcept
  {
    using std::swap;
    swap(this->ptr, other.ptr);
  }

  /// Observers

  constexpr auto get() const noexcept -> element_type* { return this->ptr; }

  constexpr operator bool() const noexcept { return this->get() != nullptr; }

  constexpr auto operator* () const noexcept
    -> std::add_lvalue_reference_t<element_type>
  { return *this->get(); }

  constexpr auto operator-> () const noexcept
    -> element_type*
  { return this->get(); }

  constexpr explicit operator element_type* () const noexcept
  { return this->get(); }


private:
  T* ptr;
};

template <typename T>
inline auto make_observer(T* p) noexcept
{ return ::cci::observer_ptr<T>(p); }

template <typename W1, typename W2>
inline bool operator== (const ::cci::observer_ptr<W1>& p1, const ::cci::observer_ptr<W2>& p2)
{ return p1.get() == p2.get(); }

template <typename W1, typename W2>
inline bool operator!= (const ::cci::observer_ptr<W1>& p1, const ::cci::observer_ptr<W2>& p2)
{ return !(p1 == p2); }

template <typename W>
inline bool operator== (const ::cci::observer_ptr<W>& p, std::nullptr_t) noexcept
{ return !p; }

template <typename W>
inline bool operator== (std::nullptr_t, const ::cci::observer_ptr<W>& p) noexcept
{ return !p; }

template <typename W>
inline bool operator!= (const ::cci::observer_ptr<W>& p, std::nullptr_t) noexcept
{ return static_cast<bool>(p); }

template <typename W>
inline bool operator!= (std::nullptr_t, const ::cci::observer_ptr<W>& p) noexcept
{ return static_cast<bool>(p); }

template <typename W1, typename W2>
inline bool operator< (const ::cci::observer_ptr<W1>& p1, const ::cci::observer_ptr<W2>& p2)
{
  using W3 = std::common_type_t<W1*, W2*>;
  return std::less<W3>()(p1.get(), p2.get());
}

template <typename W1, typename W2>
inline bool operator> (const ::cci::observer_ptr<W1>& p1, const ::cci::observer_ptr<W2>& p2)
{ return p2 < p1; }

template <typename W1, typename W2>
inline bool operator<= (const ::cci::observer_ptr<W1>& p1, const ::cci::observer_ptr<W2>& p2)
{ return !(p2 < p1); }

template <typename W1, typename W2>
inline bool operator>= (const ::cci::observer_ptr<W1>& p1, const ::cci::observer_ptr<W2>& p2)
{ return !(p1 < p2); }

} // namespace cci

namespace std
{
template <typename W>
inline void swap(::cci::observer_ptr<W>& lhs, ::cci::observer_ptr<W>& rhs) noexcept
{ lhs.swap(rhs); }

template <typename T>
struct hash<::cci::observer_ptr<T>>
{
  hash() noexcept = default;

  constexpr auto operator() (::cci::observer_ptr<T> p) noexcept -> std::size_t
  {
    return std::hash<T*>()(p.get());
  }
};
} // namespace std
