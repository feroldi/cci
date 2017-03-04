#pragma once

#include <cstdio>
#include <cstdint>
#include <string>
#include <experimental/string_view>

using std::experimental::string_view;

class Program
{
  std::uint64_t errors_count{0};
  std::uint64_t warns_count{0};
  std::FILE* log;

  void output_msg(std::string const& msg)
  {
    std::fprintf(log, "%s\n", msg.c_str());
  }

public:

  template <typename... Args>
  void error(char const* begin, char const* end, string_view msg, Args&&... args)
  {
  }

  template <typename... Args>
  void warning(char const* begin, char const* end, string_view msg, Args&&... args)
  {
  }

};


