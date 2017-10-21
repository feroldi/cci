#include "ccompiler/Util/MemoryBuffer.hpp"
#include "fmt/format.h"
#include <cassert>

int main(int argc, char **argv)
{
  using namespace ccompiler;

  assert(argc == 2);

  auto buf = util::MemoryBuffer::from_file(argv[1]);
  fmt::print("file name: {}\n", buf->name());
  fmt::print("file content: {}\n", buf->buffer());
}
