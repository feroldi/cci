cmake_minimum_required(VERSION 3.9)

project(cci)

add_subdirectory(cmake)

# Disable extended variants of C++ dialects
# i.e. don't choose gnu++11 over c++11
set(CXX_EXTENSIONS OFF)

add_executable(cci src/cci.cpp)

include_directories(
  include
  deps
)

# FIXME: this should be an add_subdirectory.
add_library(fmt STATIC deps/fmt/fmt/format.cc)
target_link_libraries(cci PRIVATE fmt)

target_compile_features(cci PUBLIC cxx_std_17)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  target_compile_options(cci PUBLIC
    -stdlib=libc++
    #-fuse-ld=lld
  )

  # Enable LTO (Link Time Optimization)
  set_property(TARGET cci PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)

  if(CMAKE_BUILD_TYPE MATCHES DEBUG)
    target_compile_options(cci PUBLIC -fno-limit-debug-info)
  endif()

  target_link_libraries(cci PUBLIC c++ c++abi)
endif()

option(CCI_ENABLE_WARNINGS "Turn on all warnings." ON)
if (CCI_ENABLE_WARNINGS)
  target_compile_options(cci PUBLIC
    -Wall
    -Wextra
    -Weverything
    )
endif()

option(CCI_ENABLE_WERROR "Stop compiling upon warnings." ON)
if (CCI_ENABLE_WERROR)
  target_compile_options(cci PUBLIC -Werror)
endif()

option(CCI_ENABLE_PEDANTIC "Compile with -pedantic on." ON)
if (CCI_ENABLE_PEDANTIC)
  target_compile_options(cci PUBLIC -pedantic -pedantic-errors)
endif()

option(CCI_ENABLE_NATIVE "Generate code based on native hardware (-march=native)." ON)
if (CCI_ENABLE_NATIVE)
  target_compile_options(cci PUBLIC -march=native)
endif()

option(CCI_ENABLE_CONTRACTS "Enable contracts (assertions). This makes the binary slow." ON)
if (CCI_ENABLE_CONTRACTS)
  target_compile_definitions(cci PUBLIC -DCCI_ENABLE_CONTRACTS)
endif()

option(CCI_ENABLE_ASAN "Enable address sanitizer." OFF)
if (CCI_ENABLE_ASAN)
  target_compile_options(cci PUBLIC -fsanitize=address)
  target_link_libraries(cci PUBLIC -fsanitize=address)
endif()

option(CCI_ENABLE_INT_WRAP "Enable integer wrapping (-fwrapv)." OFF)
if (CCI_ENABLE_INT_WRAP)
  target_compile_options(cci PUBLIC -fwrapv)
endif()

option(CCI_ENABLE_TESTS "Enable testing with GTest." OFF)
option(CCI_USE_GIT_REVISION "Use Git revision to make up CCI's build version." ON)

# Default compile options.
target_compile_options(cci
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

# Enable LTO (Link Time Optimization)
set_property(TARGET cci PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)

# Includes building pieces of the compiler.
add_subdirectory(lib)

if (CCI_ENABLE_TESTS)
  # Including /unittest enables testing automatically.
  add_subdirectory(unittest)
endif()

# Takes advantage of git version control to make up the compiler's build version.
if (CCI_USE_GIT_REVISION)
  target_compile_definitions(cci PUBLIC -DCCI_USING_GIT_REVISION)
  git_get_head_revision(GIT_REFSPEC GIT_HASH)
  git_get_exact_tag(GIT_TAG)
  configure_file(git_revision.cpp.in ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cpp @ONLY NEWLINE_STYLE UNIX)
  target_sources(cci PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cpp)
endif()
