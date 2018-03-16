# cmake -DCMAKE_TOOLCHAIN_FILE=/path/to/this/file

if (NOT CMAKE_C_COMPILER)
  find_program(CLANG_EXECUTABLE NAMES clang)

  if (NOT CLANG_EXECUTABLE)
    message(FATAL_ERROR "Could not find 'clang'.")
  endif()

  set(CMAKE_C_COMPILER "${CLANG_EXECUTABLE}" CACHE FILEPATH "")
endif()

if (NOT CMAKE_CXX_COMPILER)
  find_program(CLANGXX_EXECUTABLE NAMES clang++)

  if (NOT CLANGXX_EXECUTABLE)
    message(FATAL_ERROR "Could not find 'clang++'.")
  endif()

  set(CMAKE_CXX_COMPILER "${CLANGXX_EXECUTABLE}" CACHE FILEPATH "")
endif()

set(CMAKE_INTERPROCEDURAL_OPTIMIZATION ON)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++17 over c++17
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS "-Werror -Wall -Wextra -Wundef -pedantic-errors -stdlib=libc++ -fcolor-diagnostics -march=native -fno-limit-debug-info -fsanitize=address"
  CACHE STRING "Toolchain C++ compiler flags." FORCE)

set(CMAKE_EXE_LINKER_FLAGS "-stdlib=libc++ -lc++abi -fuse-ld=lld -fsanitize=address"
  CACHE STRING "Toolchain C++ linker flags." FORCE)
