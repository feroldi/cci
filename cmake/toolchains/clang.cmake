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
