find_package(GTest REQUIRED)
include(GoogleTest)

add_subdirectory(basic)

add_cci_unittest_executable(UnitTests
  SOURCES main.cpp
  DEPENDS
    CCIBasicTest)
