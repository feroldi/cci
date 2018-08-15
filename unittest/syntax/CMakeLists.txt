add_executable(cci_syntax_test
  source_map_test.cpp
  lexer_test.cpp
  literal_parser_test.cpp)
target_link_libraries(cci_syntax_test
  PRIVATE cci_syntax cci_util fmt::fmt GTest::GTest GTest::Main)
target_compile_features(cci_syntax_test PUBLIC cxx_std_17)
gtest_add_tests(TARGET cci_syntax_test)
