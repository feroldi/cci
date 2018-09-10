add_library(cci_ast
  ast_context.cpp
  expr.cpp)
target_include_directories(cci_ast
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_link_libraries(cci_ast PUBLIC cci_util cci_syntax fmt::fmt)
target_compile_features(cci_ast PUBLIC cxx_std_17)
