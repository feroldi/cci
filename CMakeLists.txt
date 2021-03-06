cmake_minimum_required(VERSION 3.11)
project(cci)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

option(CCI_CONTRACTS "Enable contracts (assertions)." ON)
option(CCI_COVERAGE "Enable code coverage measurements with gcov/lcov." OFF)

if (CCI_COVERAGE)
  include(CodeCoverage)
  append_coverage_compiler_flags()
  set(COVERAGE_LCOV_EXCLUDES "/usr/*" "${CMAKE_CURRENT_SOURCE_DIR}/unittest/*")
endif()

include(CTest)

add_subdirectory(lib)
if (BUILD_TESTING)
  add_subdirectory(unittest)
endif()
