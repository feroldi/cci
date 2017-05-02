#include <cstdio>
#include <string>
#include <fmt/format.h>
#include <fmt/format.cc>
#include "lexer.hpp"

int main(int argc, char** argv)
{
  TextStream stream(argv[1]);
  fmt::print("{}\n", stream.text);

  auto tokens = lexer_tokenize_text(stream.text);

  for (const auto& token : tokens)
  {
    auto [lineno, colno] = stream.linecol_from_source_location(token.data);
    fmt::print("{}:{}:\t{}\n", lineno, colno, std::string(string_view(token.data)));
  }
}

