cmake_minimum_required(VERSION 3.9)

project(ccompiler)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++11 over c++11
set(CXX_EXTENSIONS OFF)

add_executable(ccompiler src/main.cpp)

include_directories(
  include
  deps
)

# FIXME: this should be an add_subdirectory.
add_library(fmt STATIC deps/fmt/fmt/format.cc)

target_compile_features(ccompiler PUBLIC cxx_std_17)

target_compile_options(ccompiler
  PUBLIC
    -stdlib=libc++

    # Turn on all them warnings.
    -Wall
    -Wextra
    -Weverything
    -Werror

    # Turn pedantic warnings into errors.
    -pedantic-errors

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

    # Clang's bug with templated deduction guides
    -Wno-undefined-func-template

    # Emit full debug information.
    -fno-limit-debug-info
)

#add_subdirectory(include)
add_subdirectory(lib)

target_link_libraries(ccompiler
  PUBLIC
    c++
    c++abi
  PRIVATE
    fmt
)

# Enable LTO (Link Time Optimization)
set_property(TARGET ccompiler PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
