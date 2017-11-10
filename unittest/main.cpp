// This is a main source file that needs to exist, otherwise
// we couldn't create an executable for the unit tests.
// The CMake's `add_executable` function requires at least one source
// file in order to work.
#include "gtest/gtest.h"

int main(int argc, char **argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
