// This is a dummy source file that needs to exist, otherwise
// we couldn't create an executable for the unit tests.
//
// The CMake's `add_executable` function requires at least one source
// file in order to work.

#include "gtest/gtest.h"

TEST(DummyTests, Dummy)
{
  EXPECT_TRUE(true);
}
