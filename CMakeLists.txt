cmake_minimum_required(VERSION 3.9)

project(cci)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++11 over c++11
set(CMAKE_CXX_EXTENSIONS OFF)

add_subdirectory(cmake)

include_directories(
  include
  deps
)

# FIXME: this should be an add_subdirectory.
add_library(fmt STATIC deps/fmt/fmt/format.cc)

option(CCI_ENABLE_WARNINGS "Turn on all warnings." ON)
option(CCI_ENABLE_WERROR "Stop compiling upon warnings." ON)
option(CCI_ENABLE_PEDANTIC "Compile with -pedantic on." ON)
option(CCI_ENABLE_NATIVE "Generate code based on native hardware (-march=native)." ON)
option(CCI_ENABLE_CONTRACTS "Enable contracts (assertions). This makes the binary slow." ON)
option(CCI_ENABLE_ASAN "Enable address sanitizer." OFF)
option(CCI_ENABLE_INT_WRAP "Enable integer wrapping (-fwrapv)." OFF)
option(CCI_ENABLE_TESTS "Enable testing with GTest." OFF)
option(CCI_USE_GIT_REVISION "Use Git revision to make up CCI's build version." ON)

# Includes building pieces of the compiler.
add_subdirectory(lib)
add_subdirectory(src)

if (CCI_ENABLE_TESTS)
  # Including /unittest enables testing automatically.
  add_subdirectory(unittest)
endif()
