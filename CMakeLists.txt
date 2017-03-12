cmake_minimum_required(VERSION 3.7)
project(ccompiler)

set(CMAKE_CXX_COMPILER             "/usr/bin/clang++")
set(CMAKE_CXX_FLAGS_DEBUG          "-g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O4 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")

add_executable(ccompiler src/main.cpp)

target_sources(ccompiler
  PRIVATE
    src/lexer.cpp
  PUBLIC
    src/lexer.hpp
)

target_link_libraries(ccompiler
  PUBLIC
    c++
    c++abi
)

target_compile_options(ccompiler
  PUBLIC
    -std=c++1z -Wall -Wextra -pedantic-errors -Wno-c++98-compat
)

