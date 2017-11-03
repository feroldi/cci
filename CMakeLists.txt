cmake_minimum_required(VERSION 3.9)

project(sunfyre)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++11 over c++11
set(CXX_EXTENSIONS OFF)

add_executable(sunfyre src/main.cpp)

include_directories(
  include
  deps
)

# FIXME: this should be an add_subdirectory.
add_library(fmt STATIC deps/fmt/fmt/format.cc)
target_link_libraries(sunfyre PRIVATE fmt)

target_compile_features(sunfyre PUBLIC cxx_std_17)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  target_compile_options(sunfyre PUBLIC
    -stdlib=libc++
    #-fuse-ld=lld
  )

  # Enable LTO (Link Time Optimization)
  set_property(TARGET sunfyre PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)

  if(CMAKE_BUILD_TYPE MATCHES DEBUG)
    target_compile_options(sunfyre PUBLIC -fno-limit-debug-info)
  endif()

  target_link_libraries(sunfyre PUBLIC c++ c++abi)
endif()

option(SUNFYRE_ENABLE_WARNINGS "Turn on all warnings." ON)
if (SUNFYRE_ENABLE_WARNINGS)
  target_compile_options(sunfyre PUBLIC
    -Wall
    -Wextra
    -Weverything
    )
endif()

option(SUNFYRE_ENABLE_WERROR "Stop compiling upon warnings." ON)
if (SUNFYRE_ENABLE_WERROR)
  target_compile_options(sunfyre PUBLIC -Werror)
endif()

option(SUNFYRE_ENABLE_PEDANTIC "Compile with -pedantic on." ON)
if (SUNFYRE_ENABLE_PEDANTIC)
  target_compile_options(sunfyre PUBLIC -pedantic -pedantic-errors)
endif()

option(SUNFYRE_ENABLE_NATIVE "Generate code based on native hardware (-march=native)." ON)
if (SUNFYRE_ENABLE_NATIVE)
  target_compile_options(sunfyre PUBLIC -march=native)
endif()

option(SUNFYRE_ENABLE_CONTRACTS "Enable contracts (assertions). This makes the binary slow." ON)
if (SUNFYRE_ENABLE_CONTRACTS)
  # TODO
endif()

option(SUNFYRE_ENABLE_ASAN "Enable address sanitizer." OFF)
if (SUNFYRE_ENABLE_ASAN)
  target_compile_options(sunfyre PUBLIC -fsanitize=address)
  target_link_libraries(sunfyre PUBLIC -fsanitize=address)
endif()

option(SUNFYRE_ENABLE_INT_WRAP "Enable integer wrapping (-fwrapv)." OFF)
if (SUNFYRE_ENABLE_INT_WRAP)
  target_compile_options(sunfyre PUBLIC -fwrapv)
endif()

option(SUNFYRE_ENABLE_TESTS "Enable testing with GTest." OFF)

# Default compile options.
target_compile_options(sunfyre
  PUBLIC
    # Disable a few unwanted warnings.
    -Wno-c++98-compat
    -Wno-c++98-compat-pedantic
    -Wno-gnu-statement-expression
    -Wno-padded
    -Wno-weak-vtables
    -Wno-documentation
    -Wno-shadow
    -Wno-missing-variable-declarations
    -Wno-switch-enum
    -Wno-covered-switch-default
    -Wno-global-constructors
    -Wno-format-nonliteral # for fmtlib
    # Clang's bug with templated deduction guides. Fixed in r316820.
    -Wno-undefined-func-template
)

add_subdirectory(lib)

if (SUNFYRE_ENABLE_TESTS)
  add_subdirectory(unittest)
endif()
