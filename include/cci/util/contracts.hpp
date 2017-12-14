/// Contracts, Unreachable and Assertion
///
/// https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Ri-expects
/// https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Ri-ensures
///
#pragma once

// Compatibility with non-Clang compilers.
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#ifdef CCI_ENABLE_CONTRACTS
#include <cassert>
#include <stdexcept>

// http://stackoverflow.com/a/19343239/2679626
#ifndef STRINGIFY
#define STRINGIFY_DETAIL(x) #x
#define STRINGIFY(x) STRINGIFY_DETAIL(x)
#endif

struct broken_contract : std::runtime_error
{
  broken_contract(const char *msg) : std::runtime_error(msg) {}
};

struct unreachable_exception : std::runtime_error
{
  unreachable_exception(const char *msg) : std::runtime_error(msg) {}
};

/// I.6: Prefer Expects() for expressing preconditions.
/// See https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Ri-expects
#define cci_expects(cond)                                                      \
  ((cond) ? void(0)                                                            \
          : throw broken_contract("precondition failure at  " __FILE__         \
                                  ":" STRINGIFY(__LINE__) ": " #cond))

/// I.8: Prefer Ensures() for expressing postconditions.
/// See https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Ri-ensures
#define cci_ensures(cond)                                                      \
  ((cond) ? void(0)                                                            \
          : throw broken_contract("postcondition failure at  " __FILE__        \
                                  ":" STRINGIFY(__LINE__) ": " #cond))

/// Unreachable code must be marked with this.
#define cci_unreachable()                                                      \
  (throw unreachable_exception("unreachable code reached at " __FILE__         \
                               ":" STRINGIFY(__LINE__)))

#else // ifdef CCI_ENABLE_CONTRACTS

#define cci_expects(cond)
#define cci_ensures(cond)
#if defined(__GNUC__) || __has_builtin(__builtin_unreachable)
#define cci_unreachable() __builtin_unreachable()
#else
#define cci_unreachable() std::terminate()
#endif

#endif // ifdef CCI_ENABLE_CONTRACTS
