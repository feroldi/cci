// Copyright (c) 2018 Mário Feroldi
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE or copy at
// http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <atomic>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

namespace cci::pmr {

// [mem.res.class], class memory_resource
class memory_resource
{
  static constexpr std::size_t _max_align = alignof(std::max_align_t);

public:
  // [mem.res.public], public member functions
  memory_resource() = default;
  memory_resource(const memory_resource &) = default;
  virtual ~memory_resource() = default;

  memory_resource &operator=(const memory_resource &) = default;

  [[nodiscard]] void *allocate(std::size_t bytes,
                               std::size_t alignment = _max_align) {
    return do_allocate(bytes, alignment);
  }

  void deallocate(void *p, std::size_t bytes,
                  std::size_t alignment = _max_align)
  {
    return do_deallocate(p, bytes, alignment);
  }

  bool is_equal(const memory_resource &other) const noexcept
  {
    return do_is_equal(other);
  }

private:
  // [mem.res.private], private member functions
  virtual void *do_allocate(std::size_t bytes, std::size_t alignment) = 0;
  virtual void do_deallocate(void *p, std::size_t bytes,
                             std::size_t alignment) = 0;
  virtual bool do_is_equal(const memory_resource &other) const noexcept = 0;
};

inline bool operator==(const memory_resource &a,
                       const memory_resource &b) noexcept
{
  return &a == &b || a.is_equal(b);
}

inline bool operator!=(const memory_resource &a,
                       const memory_resource &b) noexcept
{
  return !(a == b);
}

memory_resource *new_delete_resource() noexcept;
memory_resource *null_memory_resource() noexcept;
memory_resource *set_default_resource(memory_resource *r) noexcept;
memory_resource *get_default_resource() noexcept;

template <class Tp>
class polymorphic_allocator
{
private:
  memory_resource *res;

public:
  using value_type = Tp;

  // [mem.poly.allocator.ctor], constructors
  polymorphic_allocator() noexcept : res(get_default_resource()) {}
  polymorphic_allocator(memory_resource *r) : res(r) { assert(r); }

  polymorphic_allocator(const polymorphic_allocator &) = default;

  template <class U>
  polymorphic_allocator(const polymorphic_allocator<U> &other) noexcept
    : res(other.resource())
  {}

  polymorphic_allocator &operator=(const polymorphic_allocator &) = delete;

  // [mem.poly.allocator.mem], member functions
  [[nodiscard]] Tp *allocate(std::size_t n) {
    return static_cast<Tp *>(res->allocate(n * sizeof(Tp), alignof(Tp)));
  }

  void deallocate(Tp *p, std::size_t n)
  {
    return res->deallocate(p, n * sizeof(Tp), alignof(Tp));
  }

  template <class T, class... Args>
  void construct(T *p, Args &&... args)
  {
    using uses_alloc_tag =
      uses_alloc_ctor_t<T, polymorphic_allocator &, Args...>;
    return _construct(uses_alloc_tag(), p, std::forward<Args>(args)...);
  }

  template <class T1, class T2, class... Args1, class... Args2>
  void construct(std::pair<T1, T2> *p, std::piecewise_construct_t,
                 std::tuple<Args1...> x, std::tuple<Args2...> y)
  {
    using x_uses_alloc_tag =
      uses_alloc_ctor_t<T1, polymorphic_allocator &, Args1...>;
    using y_uses_alloc_tag =
      uses_alloc_ctor_t<T2, polymorphic_allocator &, Args2...>;

    ::new (p) std::pair<T1, T2>(std::piecewise_construct,
                                _construct_p(x_uses_alloc_tag(), std::move(x)),
                                _construct_p(y_uses_alloc_tag(), std::move(y)));
  }

  template <class T1, class T2>
  void construct(std::pair<T1, T2> *p)
  {
    return construct(p, std::piecewise_construct, std::tuple(), std::tuple());
  }

  template <class T1, class T2, class U, class V>
  void construct(std::pair<T1, T2> *p, U &&x, V &&y)
  {
    return construct(p, std::forward_as_tuple(std::forward<U>(x)),
                     std::forward_as_tuple(std::forward<V>(y)));
  }

  template <class T1, class T2, class U, class V>
  void construct(std::pair<T1, T2> *p, const std::pair<U, V> &pr)
  {
    return construct(p, std::forward_as_tuple(pr.first),
                     std::forward_as_tuple(pr.second));
  }

  template <class T1, class T2, class U, class V>
  void construct(std::pair<T1, T2> *p, std::pair<U, V> &&pr)
  {
    return construct(p, std::piecewise_construct,
                     std::forward_as_tuple(std::forward<U>(pr.first)),
                     std::forward_as_tuple(std::forward<V>(pr.second)));
  }

  template <class T>
  void destroy(T *p)
  {
    p->~T();
  }

  polymorphic_allocator select_on_container_copy_construction() const
  {
    return polymorphic_allocator();
  }

  memory_resource *resource() const { return res; }

private:
  template <bool UsesAlloc, typename T, typename Alloc, typename... Args>
  struct uses_alloc_ctor_impl
  {
    static const int value = 0;
  };

  template <typename T, typename Alloc, typename... Args>
  struct uses_alloc_ctor_impl<true, T, Alloc, Args...>
  {
    static const bool first_ctor =
      std::is_constructible_v<T, std::allocator_arg_t, Alloc, Args...>;
    static const bool second_ctor =
      std::conditional_t<first_ctor, std::false_type,
                         std::is_constructible<T, Args..., Alloc>>::value;

    static_assert(first_ctor || second_ctor,
                  " request for uses-allocator construction is ill-formed");

    static const int value = first_ctor ? 1 : 2;
  };

  // FIXME: std::uses_allocator might not consider std::erased_type for
  // libraries that don't implement it.
  template <typename T, typename Alloc, typename... Args>
  struct uses_alloc_ctor
  {
    using type = std::integral_constant<
      int, uses_alloc_ctor_impl<std::uses_allocator_v<T, Alloc>, T, Alloc,
                                Args...>::value>;
  };

  template <typename T, typename Alloc, typename... Args>
  using uses_alloc_ctor_t = typename uses_alloc_ctor<T, Alloc, Args...>::type;

  using uses_alloc0 = std::integral_constant<int, 0>;
  using uses_alloc1 = std::integral_constant<int, 1>;
  using uses_alloc2 = std::integral_constant<int, 2>;

  template <typename T, typename... Args>
  void _construct(uses_alloc0, T *storage, Args &&... args)
  {
    ::new (storage) T(std::forward<Args>(args)...);
  }

  template <typename T, typename... Args>
  void _construct(uses_alloc1, T *storage, Args &&... args)
  {
    ::new (storage) T(std::allocator_arg, *this, std::forward<Args>(args)...);
  }

  template <typename T, typename... Args>
  void _construct(uses_alloc2, T *storage, Args &&... args)
  {
    ::new (storage) T(std::forward<Args>(args)..., *this);
  }

  // Piecewise construction.

  template <typename Tuple>
  Tuple &&_construct_p(uses_alloc0, Tuple &t)
  {
    return std::move(t);
  }

  template <typename... Args>
  decltype(auto) _construct_p(uses_alloc1, std::tuple<Args...> &t)
  {
    return std::tuple_cat(std::make_tuple(std::allocator_arg, *this),
                          std::move(t));
  }

  template <typename... Args>
  decltype(auto) _construct_p(uses_alloc2, std::tuple<Args...> &t)
  {
    return std::tuple_cat(std::move(t), std::make_tuple(*this));
  }
};

template <class T1, class T2>
inline bool operator==(const polymorphic_allocator<T1> &a,
                       const polymorphic_allocator<T2> &b) noexcept
{
  return *a.resource() == *b.resource();
}

template <class T1, class T2>
inline bool operator!=(const polymorphic_allocator<T1> &a,
                       const polymorphic_allocator<T2> &b) noexcept
{
  return !(a == b);
}

class monotonic_buffer_resource : public memory_resource
{
public:
  explicit monotonic_buffer_resource(memory_resource *mr) : upstream(mr) {}

  monotonic_buffer_resource(std::size_t initial_size, memory_resource *mr)
    : upstream(mr), next_region_size(initial_size)
  {
    assert(initial_size > 0);
  }

  monotonic_buffer_resource(void *buffer, std::size_t buffer_size,
                            memory_resource *mr)
    : upstream(mr)
    , region_base_ptr(reinterpret_cast<std::byte *>(buffer))
    , region_cur_ptr(reinterpret_cast<std::byte *>(buffer))
    , region_end_ptr(reinterpret_cast<std::byte *>(buffer) + buffer_size)
    , next_region_size(compute_next_grow(buffer_size))
  {
    assert(buffer_size > 0);
  }

  monotonic_buffer_resource()
    : monotonic_buffer_resource(get_default_resource())
  {}

  explicit monotonic_buffer_resource(std::size_t initial_size)
    : monotonic_buffer_resource(initial_size, get_default_resource())
  {}

  monotonic_buffer_resource(void *buffer, std::size_t buffer_size)
    : monotonic_buffer_resource(buffer, buffer_size, get_default_resource())
  {}

  monotonic_buffer_resource(const monotonic_buffer_resource &) = delete;

  virtual ~monotonic_buffer_resource() override { release(); }

  monotonic_buffer_resource &
  operator=(const monotonic_buffer_resource &) = delete;

  void release()
  {
    // Deallocates all regions by walking backwards in the list. Stops walking
    // if we hit a buffer we don't own, or when there aren't any more regions.
    while (owns_region && region_base_ptr)
    {
      owned_region_header header;
      std::memcpy(&header, region_base_ptr, sizeof(header));
      upstream->deallocate(region_base_ptr, region_size());
      region_base_ptr = header.prev_region_base_ptr;
      region_end_ptr = header.prev_region_end_ptr;
      owns_region = header.owns_prev_region;
    }

    assert(!owns_region);
    region_cur_ptr = region_base_ptr;
  }

  memory_resource *upstream_resource() const { return upstream; }

protected:
  void *do_allocate(std::size_t bytes, std::size_t alignment) override
  {
    if (region_base_ptr)
    {
      assert(region_cur_ptr);

      auto space = static_cast<std::size_t>(region_end_ptr - region_cur_ptr);
      void *aligned_cur_ptr = region_cur_ptr;
      if (std::align(alignment, bytes, aligned_cur_ptr, space))
      {
        region_cur_ptr = static_cast<std::byte *>(aligned_cur_ptr) + bytes;
        return aligned_cur_ptr;
      }
    }

    // We either don't have a region yet, or we need to allocate one.
    // When the initial buffer (if any) is exhausted, it obtains additional
    // buffers from an upstream memory resource supplied at construction.
    // Each additional buffer is larger than the previous one, following a
    // geometric progression.

    // The next region needs to be able to fit its own header and the requested
    // bytes. Alignment is added so there's enough space for the requested
    // bytes, otherwise aligned addresses may cause the memory resource to think
    // the region is full, when in fact it has enough space but no good aligned
    // address.
    const auto required_size = sizeof(owned_region_header) + alignment + bytes;
    if (next_region_size < required_size)
      next_region_size = required_size;

    const auto next_region_storage = upstream->allocate(next_region_size);
    if (next_region_storage)
    {
      auto next_region_header = new (next_region_storage) owned_region_header;
      next_region_header->prev_region_base_ptr = region_base_ptr;
      next_region_header->prev_region_end_ptr = region_end_ptr;
      next_region_header->owns_prev_region = owns_region;

      const auto next_region_base_ptr =
        static_cast<std::byte *>(next_region_storage);
      region_base_ptr = next_region_base_ptr;
      region_cur_ptr = next_region_base_ptr + sizeof(owned_region_header);
      region_end_ptr = next_region_base_ptr + next_region_size;
      [[maybe_unused]] const auto old_next_region_size = next_region_size;
      next_region_size = compute_next_grow(next_region_size);
      assert(next_region_size >= old_next_region_size);
      owns_region = true;

      // We could just call do_allocate recursively here, but we need to assert
      // that the aligned address is good.
      auto space = static_cast<std::size_t>(region_end_ptr - region_cur_ptr);
      void *cur_ptr = region_cur_ptr;
      const auto aligned_cur_ptr = std::align(alignment, bytes, cur_ptr, space);
      assert(aligned_cur_ptr);
      region_cur_ptr = static_cast<std::byte *>(aligned_cur_ptr) + bytes;
      return aligned_cur_ptr;
    }

    return nullptr;
  }

  void do_deallocate(void *, std::size_t, std::size_t) override
  {
    // Do nothing.
  }

  bool do_is_equal(const memory_resource &other) const noexcept override
  {
    return this == &other;
  }

private:
  // Upstream memory resource from which we allocate regions.
  memory_resource *upstream;

  std::byte *region_base_ptr = nullptr; ///< Current region.
  std::byte *region_cur_ptr = nullptr; ///< Current free space in the region.
  std::byte *region_end_ptr = nullptr; ///< End of the region.
  std::size_t next_region_size = 4096; ///< Size of the next allocated region.

  // Whether we allocated the region ourselves. Only the first region may be
  // unowned.
  bool owns_region = false;

  // Information about a region allocated by this monotonic buffer resource. The
  // first bytes of an owned region contain the following structure.
  //
  // This approach effectively creates a linked list of regions, connecting the
  // last region to the previous one and so on.
  struct owned_region_header
  {
    std::byte *prev_region_base_ptr;
    std::byte *prev_region_end_ptr;
    bool owns_prev_region;
  };

  constexpr std::size_t compute_next_grow(std::size_t reg_size) const
  {
    return reg_size * 2;
  }

  std::size_t region_size() const
  {
    return static_cast<std::size_t>(region_end_ptr - region_base_ptr);
  }
};

inline memory_resource *new_delete_resource() noexcept
{
  struct type : memory_resource
  {
    void *do_allocate(std::size_t bytes, std::size_t alignment) override
    {
      if (alignment > __STDCPP_DEFAULT_NEW_ALIGNMENT__)
      {
        auto al = std::align_val_t(alignment);
        return ::operator new(bytes, al);
      }
      return ::operator new(bytes);
    }

    void do_deallocate(void *p, std::size_t,
                       std::size_t alignment) noexcept override
    {
      if (alignment > __STDCPP_DEFAULT_NEW_ALIGNMENT__)
      {
        auto al = std::align_val_t(alignment);
        return ::operator delete(p, al);
      }
      return ::operator delete(p);
    }

    bool do_is_equal(const memory_resource &other) const noexcept override
    {
      return this == &other;
    }
  };

  // Constructing the memory resource this way ensures that no exit-time
  // destructors will be called.
  alignas(type) static char buffer[sizeof(type)];
  memory_resource *mr = new (buffer) type;
  return mr;
}

inline memory_resource *null_memory_resource() noexcept
{
  struct type : memory_resource
  {
    void *do_allocate(std::size_t, std::size_t) override
    {
      throw std::bad_alloc();
    }

    void do_deallocate(void *, std::size_t, std::size_t) noexcept override {}

    bool do_is_equal(const memory_resource &other) const noexcept override
    {
      return this == &other;
    }
  };

  // Constructing the memory resource this way ensures that no exit-time
  // destructors will be called.
  alignas(type) static char buffer[sizeof(type)];
  memory_resource *mr = new (buffer) type;
  return mr;
}

namespace detail {
inline std::atomic<memory_resource *> &get_default_resource_impl() noexcept
{
  static std::atomic<memory_resource *> r(new_delete_resource());
  return r;
}
} // namespace detail

inline memory_resource *get_default_resource() noexcept
{
  return detail::get_default_resource_impl().load();
}

inline memory_resource *set_default_resource(memory_resource *r) noexcept
{
  if (!r)
    r = new_delete_resource();
  return detail::get_default_resource_impl().exchange(r);
}

} // namespace feroldi::pmr
