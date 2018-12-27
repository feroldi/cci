add_library(cci_syntax
  char_info.cpp
  diagnostics.cpp
  literal_parser.cpp
  parser.cpp
  scanner.cpp
  sema.cpp
  source_map.cpp
  unicode_char_set.cpp)
target_include_directories(cci_syntax
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_link_libraries(cci_syntax PUBLIC cci_util)
target_compile_features(cci_syntax PUBLIC cxx_std_17)
