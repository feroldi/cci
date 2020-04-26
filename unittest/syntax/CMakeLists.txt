add_executable(cci_syntax_test
  char_info_test.cpp
  diagnostics_test.cpp
  literal_parser_test.cpp
  parser_test.cpp
  scanner_test.cpp
  source_map_test.cpp
  unicode_char_set_test.cpp)

target_link_libraries(cci_syntax_test
  PRIVATE cci_syntax cci_ast cci_util GTest::GTest GTest::Main)

target_compile_features(cci_syntax_test PUBLIC cxx_std_20)
gtest_add_tests(TARGET cci_syntax_test)
