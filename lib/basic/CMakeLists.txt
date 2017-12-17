add_library(cci_basic
  source_manager.cpp
  file_stream.cpp)
target_include_directories(cci_basic
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_compile_features(cci_basic PUBLIC cxx_std_17)
target_link_libraries(cci_basic PUBLIC cci_util)

install(TARGETS cci_basic
        EXPORT ${PROJECT_NAME}-targets
        DESTINATION ${CMAKE_INSTALL_LIBDIR})
