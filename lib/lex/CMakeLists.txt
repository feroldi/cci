add_library(cci_lex
  lexer.cpp
  unicode_char_set.cpp
  char_info.cpp
  literal_parser.cpp)
target_include_directories(cci_lex
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_link_libraries(cci_lex
  PUBLIC
    cci_basic
    cci_util
)
target_compile_features(cci_lex PUBLIC cxx_std_17)
install(TARGETS cci_lex
        EXPORT ${PROJECT_NAME}-targets
        DESTINATION ${CMAKE_INSTALL_LIBDIR})
