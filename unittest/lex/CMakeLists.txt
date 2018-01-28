add_executable(cci_lex_test lexer.cpp)
target_link_libraries(cci_lex_test
  PRIVATE cci_basic cci_lex cci_util GTest::GTest GTest::Main)
target_compile_features(cci_lex_test PUBLIC cxx_std_17)
gtest_add_tests(TARGET cci_lex_test)
