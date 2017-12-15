add_library(cci_basic
  source_manager.cpp
  file_stream.cpp)
target_include_directories(cci_basic
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_compile_features(cci_basic PUBLIC cxx_std_17)

# FIXME: not sure if PRIVATE is wanted,
# or if it's ok to have cci_util in the public headers
target_link_libraries(cci_basic PRIVATE cci_util)
