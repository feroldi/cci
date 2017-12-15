add_library(cci_lex lexer.cpp)
target_include_directories(cci_lex
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_link_libraries(cci_lex
  PUBLIC
    cci_basic
  PRIVATE
    # FIXME: not sure if PRIVATE is wanted,
    # or if it's ok to have cci_util in the public headers
    cci_util
)
target_compile_features(cci_lex PUBLIC cxx_std_17)
