add_executable(cci_basic_test
  source_manager.cpp)
target_link_libraries(cci_basic_test
  PRIVATE cci_basic GTest::GTest GTest::Main)
target_include_directories(cci_basic_test PRIVATE ${GTEST_INCLUDE_DIRS})
target_compile_options(cci_basic_test PRIVATE -Wno-zero-as-null-pointer-constant)
target_compile_features(cci_basic_test PUBLIC cxx_std_17)
gtest_add_tests(TARGET cci_basic_test)
