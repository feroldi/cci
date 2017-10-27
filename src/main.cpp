#include "ccompiler/basic/source_manager.hpp"
#include "fmt/format.h"
#include <cassert>

int main(int argc, char **argv)
{
  using namespace ccompiler;
  assert(argc == 2);

  if (auto src = SourceManager::from_file(argv[1]))
    fmt::print("{}", src->get_text());
}
