cmake_minimum_required(VERSION 3.7)
project(cctest)

set(CMAKE_CXX_COMPILER             "/usr/bin/clang++")
set(CMAKE_CXX_FLAGS_DEBUG          "-O1 -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")

add_executable(cctest test.cpp)

target_sources(cctest
  PRIVATE
    "lexer.hpp"
    "../src/lexer.hpp"
    "../src/lexer.cpp"
)

target_link_libraries(cctest
  PUBLIC
    c++
    c++abi
)

target_compile_options(cctest
  PUBLIC
    -std=c++1z
    -Wall
    -Wextra
    -pedantic-errors
    -Wno-c++98-compat
)

