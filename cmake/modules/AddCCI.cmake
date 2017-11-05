# Configures a target to have common compiler flags and options
# across targets.
function(cci_set_common_configs target)
  target_compile_features(${target} PUBLIC cxx_std_17)

  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    #-fuse-ld=lld
    target_compile_options(${target} PUBLIC
      -stdlib=libc++)

    # Enable LTO (Link Time Optimization)
    set_property(TARGET ${target} PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)

    if (CMAKE_BUILD_TYPE MATCHES DEBUG)
      target_compile_options(${target} PUBLIC -fno-limit-debug-info)
    endif()

    target_link_libraries(${target} PUBLIC c++ c++abi)
  endif()

  if (CCI_ENABLE_WARNINGS)
    target_compile_options(${target} PUBLIC
      -Wall
      -Wextra
      -Weverything)
  endif()

  if (CCI_ENABLE_WERROR)
    target_compile_options(${target} PUBLIC -Werror)
  endif()

  if (CCI_ENABLE_PEDANTIC)
    target_compile_options(${target} PUBLIC -pedantic -pedantic-errors)
  endif()

  if (CCI_ENABLE_NATIVE)
    target_compile_options(${target} PUBLIC -march=native)
  endif()

  if (CCI_ENABLE_CONTRACTS)
    target_compile_definitions(${target} PUBLIC -DCCI_ENABLE_CONTRACTS)
  endif()

  if (CCI_ENABLE_ASAN)
    target_compile_options(${target} PUBLIC -fsanitize=address)
    target_link_libraries(${target} PUBLIC -fsanitize=address)
  endif()

  if (CCI_ENABLE_INT_WRAP)
    target_compile_options(${target} PUBLIC -fwrapv)
  endif()

  # Default compile options.
  target_compile_options(${target}
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
      -Wno-undefined-func-template)

endfunction(cci_set_common_configs target)

# Creates a new CCI library.
#
# USAGE:
#
#     add_cci_library(<target name> SOURCES <files>... DEPENDS <targets>...)
#
# add_cci_library creates a new target with the name ${libname} as
# a library. Sources listed in SOURCES will be used to compose the library's
# sources. Anything listed in DEPENDS will get linked to the library.
function(add_cci_library libname)
  cmake_parse_arguments(ARG "" "" "SOURCES;DEPENDS" ${ARGN})
  add_library(${libname} ${ARG_SOURCES})
  if(NOT "${ARG_DEPENDS}" STREQUAL "")
    target_link_libraries(${libname} PRIVATE ${ARG_DEPENDS})
  endif()
  cci_set_common_configs(${libname})
endfunction(add_cci_library libname)

# Creates a new CCI tool/executable.
function(add_cci_tool toolname)
  cmake_parse_arguments(ARG "" "" "SOURCES;DEPENDS" ${ARGN})
  add_executable(${toolname} ${ARG_SOURCES})
  if(NOT "${ARG_DEPENDS}" STREQUAL "")
    target_link_libraries(${toolname} PRIVATE ${ARG_DEPENDS})
  endif()
  cci_set_common_configs(${toolname})

  # Takes advantage of git version control to make up the compiler's build version.
  if (CCI_USE_GIT_REVISION)
    target_compile_definitions(${toolname} PUBLIC -DCCI_USING_GIT_REVISION)
    git_get_head_revision(GIT_REFSPEC GIT_HASH)
    git_get_exact_tag(GIT_TAG)
    configure_file(${PROJECT_SOURCE_DIR}/git_revision.cpp.in ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cpp @ONLY NEWLINE_STYLE UNIX)
    target_sources(${toolname} PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cpp)
  endif()
endfunction(add_cci_tool toolname)
