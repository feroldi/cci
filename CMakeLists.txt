cmake_minimum_required(VERSION 3.7)

project(ccompiler)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++11 over c++11
set(CXX_EXTENSIONS OFF)

add_executable(ccompiler src/main.cpp)

target_sources(ccompiler
  PUBLIC
    "src/lexer.hpp"
    "src/program.hpp"
    "src/source_manager.hpp"
    "src/cpp/format.hpp"
    "src/utils/stream.hpp"
  PRIVATE
    "src/lexer.cpp"
    "src/program.cpp"
    "src/source_manager.cpp"
    "src/cpp/format.cpp"
    "src/utils/stream.cpp"
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
    -pedantic-errors
    -Wno-c++98-compat
)

# Enable LTO (Link Time Optimization)
set_property(TARGET ccompiler PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
