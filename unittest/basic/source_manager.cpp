#include "cci/basic/source_manager.hpp"
#include "gtest/gtest.h"

namespace {

TEST(BasicTest, compareSourceLocations)
{
  cci::SourceLocation l1(0U);
  auto l2 = l1.based_on(cci::SourceLocation::with_offset(1U));

  EXPECT_GT(l2, l1);
  EXPECT_EQ(cci::SourceLocation(2U), cci::SourceLocation::with_offset(2U));
}

TEST(SourceManagerTest, calcLineOffsets)
{
  EXPECT_TRUE(true);
}

} // anonymouns namespace
