# Configures a target to have common compiler flags and options
# across targets.
#
# USAGE
#   cci_set_common_configs(<target>)
#
# Compile features, options, definitions and libraries that are common
# to all targets in this project are put here. Use this function in
# order to maintain a concise compilation.
function(cci_set_common_configs target)
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    # Enable LTO (Link Time Optimization)
    set_property(TARGET ${target} PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
  endif()

  target_compile_features(${target} PUBLIC cxx_std_17)

  target_compile_options(${target}
    PUBLIC $<$<CXX_COMPILER_ID:Clang>:-stdlib=libc++>
    PRIVATE $<$<BOOL:CCI_ENABLE_WARNINGS>:-Wall -Wextra $<$<CXX_COMPILER_ID:Clang>:-Weverything>>
    PRIVATE $<$<BOOL:CCI_ENABLE_WERROR>:-Werror>
    PRIVATE $<$<BOOL:CCI_ENABLE_PEDANTIC>:-pedantic-errors>
    PRIVATE $<$<BOOL:CCI_ENABLE_NATIVE>:-march=native>
    PUBLIC $<$<BOOL:CCI_ENABLE_SAN>:-fsanitize=address>
    PRIVATE $<$<BOOL:CCI_INT_WRAP>:-fwrapv>
    PRIVATE $<$<BOOL:DEBUG>:-fno-limit-debug-info>

    PRIVATE -Wno-c++98-compat
    PRIVATE -Wno-c++98-compat-pedantic
    PRIVATE -Wno-gnu-statement-expression
    PRIVATE -Wno-padded
    PRIVATE -Wno-weak-vtables
    PRIVATE -Wno-documentation
    PRIVATE -Wno-shadow
    PRIVATE -Wno-missing-variable-declarations
    PRIVATE -Wno-switch-enum
    PRIVATE -Wno-covered-switch-default
    PRIVATE -Wno-global-constructors
    # for fmtlib
    PRIVATE -Wno-format-nonliteral
    # Clang's bug with templated deduction guides. Fixed in r316820.
    PRIVATE -Wno-undefined-func-template)

  target_compile_definitions(${target}
    PRIVATE $<$<BOOL:CCI_ENABLE_CONTRACTS>:CCI_ENABLE_CONTRACTS>)

  target_link_libraries(${target}
    PUBLIC $<$<CXX_COMPILER_ID:Clang>:c++>
    PUBLIC $<$<CXX_COMPILER_ID:Clang>:c++abi>
    PUBLIC $<$<CXX_COMPILER_ID:Clang>:-fuse-ld=lld>
    PUBLIC $<$<BOOL:CCI_ENABLE_SAN>:-fsanitize=address>)
endfunction()

# Creates a CCI library.
#
# OPTIONS
#   STATIC
#     Builds the library as static.
#   SHARED
#     Builds the library as shared.
#   SOURCES files...
#     Sets the library's source files.
#   DEPENDS targets...
#     Adds other libraries as dependencies. Effectively
#     links them.
#
# USAGE
#   add_cci_library(<target name> SOURCES <files>... DEPENDS <targets>...)
#
# add_cci_library creates a new target with the name ${libname} as
# a library. Files listed in SOURCES will be used to compose the library's
# sources. Anything listed in DEPENDS will get linked to the library.
function(add_cci_library libname)
  cmake_parse_arguments(ARG "" "" "SOURCES;DEPENDS" ${ARGN})
  add_library(${libname} ${ARG_SOURCES})
  target_include_directories(${libname}
    PUBLIC ${CMAKE_SOURCE_DIR}/include
    PUBLIC ${CMAKE_SOURCE_DIR}/deps)
  target_link_libraries(${libname} PRIVATE ${ARG_DEPENDS})
  cci_set_common_configs(${libname})
endfunction()

# Creates a CCI tool/executable.
#
# OPTIONS
#   SOURCES files...
#     Sets the executable's source files.
#   DEPENDS targets...
#     Adds other targets as dependencies. Effectively
#     links them into the executable.
#
# USAGE
#   add_cci_executable(<target name> SOURCEs <files>... DEPEDS <targets>...)
#
# add_cci_executable creates an executable with the target ${exe_name}.
# It's useful for adding tools to the project that uses the main CCI libraries.
# Files listed in SOURCES will be used to compose the executable's
# sources. Anything listed in DEPENDS will get linked into the executable.
function(add_cci_executable exe_name)
  cmake_parse_arguments(ARG "" "" "SOURCES;DEPENDS" ${ARGN})
  add_executable(${exe_name} ${ARG_SOURCES})
  target_link_libraries(${exe_name} PRIVATE ${ARG_DEPENDS})
  cci_set_common_configs(${exe_name})
endfunction()

# Creates a test suite.
#
# OPTIONS
#   SOURCES files...
#     Sets the test's source files.
#   DEPENDS targets...
#     Adds other targets as dependencies. Effectively
#     links them into the test's executable.
#
# USAGE
#   add_cci_unittest(<test suite> SOURCES <files>... DEPENDS <targets>...)
#
# Adds a single test (usually a directory containing tests) to the CCI's test
# suite. A test suite here is an executable tracked by CTest.
function(add_cci_unittest test_suite)
  add_cci_executable(${test_suite} ${ARGN} DEPENDS GTest::GTest GTest::Main)
  target_include_directories(${test_suite} PRIVATE ${GTEST_INCLUDE_DIRS})
  # GoogleTest makes a few uses of NULL.
  target_compile_options(${test_suite} PRIVATE -Wno-zero-as-null-pointer-constant)
  gtest_add_tests(TARGET ${test_suite})
endfunction()
