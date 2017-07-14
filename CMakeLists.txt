cmake_minimum_required(VERSION 3.7)

project(ccompiler)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++11 over c++11
set(CXX_EXTENSIONS OFF)

add_executable(ccompiler src/main.cpp)

#add_library(fmt deps/fmt/fmt/format.cc)

target_sources(ccompiler
  PUBLIC
    src/parser.hpp
    src/lexer.hpp
    src/program.hpp
    src/source_manager.hpp
    src/utils/stream.hpp
    src/cpp/format.hpp
    src/cpp/variant.hpp
  PRIVATE
    src/parser.cpp
    src/lexer.cpp
    src/program.cpp
    src/source_manager.cpp
    src/utils/stream.cpp
    deps/fmt/fmt/format.cc
)

target_link_libraries(ccompiler
  PUBLIC
    c++
    c++abi
)

target_compile_features(ccompiler PUBLIC cxx_std_17)

target_compile_options(ccompiler
  PUBLIC
    -stdlib=libc++
    -Wall
    -Wextra
#   -Weverything
    -pedantic-errors
    -Wno-c++98-compat
    -Wno-c++98-compat-pedantic
    -Wno-gnu-statement-expression
    -fno-limit-debug-info
)

# Enable LTO (Link Time Optimization)
set_property(TARGET ccompiler PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
