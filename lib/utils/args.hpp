#pragma once

#include <cstring>
#include <cassert>
#include <stdexcept>
#include "../cpp/string_view.hpp"
#include "../cpp/variant.hpp"
#include "../cpp/format.hpp"
#include "../cpp/contracts.hpp"

namespace utils
{

struct invalid_option : std::logic_error
{
  using std::logic_error::logic_error;
};

inline auto is_opt(char** argv) -> bool
{
  Expects(argv != nullptr);
  Expects(*argv != nullptr);

  string_view arg = *argv;

  // Every argument starting with - is a valid option.
  return arg.size() >= 2 && arg[0] == '-';
}

inline auto opt_exists(char**& argv, const string_view& short_opt, const string_view& long_opt) -> bool
{
  Expects(argv != nullptr);
  Expects(*argv != nullptr);
  Expects(short_opt.size() <= 2);
  Expects(long_opt.empty() || long_opt.size() >= 2);

  if (!is_opt(argv))
  {
    return false;
  }

  const string_view arg = *argv;

  if (!short_opt.empty() && arg.substr(0, 2) == short_opt)
  {
    if (arg.size() > 2)
    {
      std::advance(*argv, 1);
      (*argv)[0] = '-';
    }
    else
    {
      std::advance(argv, 1);
    }

    return true;
  }

  if (!long_opt.empty() && arg == long_opt)
  {
    // Just skip once, no argument should be present.
    std::advance(argv, 1);
    return true;
  }

  return false;
}

inline auto opt_get(char**& argv, const string_view& short_opt, const string_view& long_opt) -> string_view
{
  Expects(argv != nullptr);
  Expects(*argv != nullptr);
  Expects(short_opt.size() <= 2);
  Expects(long_opt.empty() || long_opt.size() >= 2);

  if (!is_opt(argv))
  {
    return string_view{};
  }

  const string_view arg = *argv;

  if (!short_opt.empty() && arg.substr(0, 2) == short_opt)
  {
    if (arg.size() > 2)
    {
      const auto arg_value = arg.substr((arg[2] == '=') ? 3 : 2);

      if (!arg_value.empty())
      {
        std::advance(argv, 1);
        return arg_value;
      }
    }
    else if (*std::next(argv) != nullptr)
    {
      std::advance(argv, 2);
      return *std::prev(argv);
    }
    else
    {
      throw invalid_option(fmt::format("missing argument to `{}'", short_opt.data()));
    }
  }

  const auto equal_pos = std::min(arg.find('='), arg.size());

  if (!long_opt.empty() && arg.substr(0, equal_pos) == long_opt)
  {
    if (equal_pos != arg.size())
    {
      auto arg_value = arg.substr(equal_pos + 1);

      if (!arg_value.empty())
      {
        std::advance(argv, 1);
        return arg_value;
      }
    }
    else if (*std::next(argv) != nullptr)
    {
      auto it = std::next(argv);
      std::advance(argv, 2);
      return *it;
    }
    else
    {
      throw invalid_option(fmt::format("missing argument to `{}'", long_opt.data()));
    }
  }

  return string_view{};
}

} // namespace utils
