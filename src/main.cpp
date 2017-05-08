#include "program.hpp"
#include <cstdio>
#include <string>

int main(int /*argc*/, char** argv)
{
  auto stream = TextStream(argv[1]);
  auto program = ProgramContext{Options{}, stdout};
  auto tokens = lexer_tokenize_text(program, stream, stream.data);
}
