add_library(cci_sema sema.cpp)
target_include_directories(cci_sema
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_link_libraries(cci_sema PRIVATE cci_util cci_syntax fmt::fmt)
target_compile_features(cci_sema PUBLIC cxx_std_17)
install(TARGETS cci_sema
        EXPORT ${PROJECT_NAME}-targets
        DESTINATION ${CMAKE_INSTALL_LIBDIR})
