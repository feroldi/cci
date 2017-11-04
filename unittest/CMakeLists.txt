enable_testing()
find_package(GTest REQUIRED)
include(GoogleTest)
include_directories(${GTEST_INCLUDE_DIRS})

add_executable(UnitTests ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp)
target_link_libraries(UnitTests GTest::GTest GTest::Main)
gtest_add_tests(TARGET UnitTests)

function(add_unit_test_sources)
  #cmake_parse_arguments(ARG "" "" "" ${ARGN})
  foreach(source ${ARGN})
    target_sources(UnitTests PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/${source})
  endforeach(source)
endfunction()

add_subdirectory(basic)
