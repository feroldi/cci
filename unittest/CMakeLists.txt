find_package(GTest REQUIRED)
include(GoogleTest)
add_subdirectory(syntax)

if (CCI_COVERAGE)
  include(CodeCoverage)
  setup_target_for_coverage_lcov(
    NAME coverage
    EXECUTABLE ctest --output-on-failure
    DEPENDENCIES cci_syntax_test)
else()
  add_custom_target(coverage
    COMMAND ctest --output-on-failure
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    DEPENDS cci_syntax_test
    COMMENT "Project generated with no coverage suport. Skipping...") 
endif()
