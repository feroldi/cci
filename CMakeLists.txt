# See /cmake/modules for more details/documentation on how CCI is built.
cmake_minimum_required(VERSION 3.9)
project(cci)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(GetGitRevisionDescription)
include(CTest)

option(CCI_ENABLE_CONTRACTS "Enable contracts (assertions). This makes the binary slow." ON)
option(CCI_USE_GIT_REVISION "Use Git revision to make up CCI's build version." ON)

# CCI depends on fmtlib 4.0.0 or later.
find_package(FMT 4 REQUIRED)

# See /include/cci/util/contracts.hpp
if(CCI_ENABLE_CONTRACTS)
  add_definitions(-DCCI_ENABLE_CONTRACTS)
endif()

# TODO: Configure `install`.

# Includes building pieces of the compiler.
add_subdirectory(lib)
add_subdirectory(src)

if (BUILD_TESTING)
  add_subdirectory(unittest)
endif()
