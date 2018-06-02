# cmake -DCMAKE_TOOLCHAIN_FILE=/path/to/this/file

#if (NOT CMAKE_C_COMPILER)
#  find_program(GCC_EXECUTABLE NAMES gcc)
#
#  if (NOT GCC_EXECUTABLE)
#    message(FATAL_ERROR "Could not find 'gcc'.")
#  endif()
#
#  set(CMAKE_C_COMPILER "${GCC_EXECUTABLE}" CACHE FILEPATH "")
#endif()
#
#if (NOT CMAKE_CXX_COMPILER)
#  find_program(GXX_EXECUTABLE NAMES g++)
#
#  if (NOT GXX_EXECUTABLE)
#    message(FATAL_ERROR "Could not find 'g++'.")
#  endif()
#
#  set(CMAKE_CXX_COMPILER "${GXX_EXECUTABLE}" CACHE FILEPATH "")
#endif()

# For std::filesystem and gtest.
link_libraries(stdc++fs pthread)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++17 over c++17
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS
  "-Werror \
   -Wall \
   -Wextra \
   -Wundef \
   -pedantic-errors \
   -fdiagnostics-color \
   -march=native"
  CACHE STRING "Toolchain C++ compiler flags." FORCE)
