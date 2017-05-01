cmake_minimum_required(VERSION 3.7)

project(ccompiler)

set(CMAKE_CXX_FLAGS_DEBUG          "-g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")

add_executable(${PROJECT_NAME} src/main.cpp)

target_sources(${PROJECT_NAME}
  PRIVATE
    src/lexer.cpp
    src/utils/stream.cpp
  PUBLIC
    src/lexer.hpp
    src/utils/stream.hpp
)

target_link_libraries(${PROJECT_NAME}
  PUBLIC
    c++
    c++abi
)

target_compile_options(${PROJECT_NAME}
  PUBLIC
    -std=c++1z -stdlib=libc++ -Wall -Wextra -pedantic-errors -Wno-c++98-compat
)

