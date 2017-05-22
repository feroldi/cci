#include "program.hpp"
#include <cstdio>
#include <string>

int main(int /*argc*/, char** argv)
{
  auto source = SourceManager::from_path(argv[1]);
  auto program = ProgramContext(Options{}, stderr);
  auto tokens = TokenStream::parse(program, source);
}
