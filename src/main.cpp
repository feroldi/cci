#include <cstdio>
#include <string>
#include <fmt/format.h>
#include <fmt/format.cc>
#include "lexer.hpp"

int main(int argc, char** argv)
{
  auto tokens = lexer_tokenize_text(argv[1]);

  for (auto token : tokens)
  {
    fmt::print("{}\n", std::string(string_view(token.data)));
  }
}

