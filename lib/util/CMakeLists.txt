add_library(cci_util unicode.cpp file_stream.cpp)
target_include_directories(cci_util
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_compile_features(cci_util PUBLIC cxx_std_17)
if(CCI_ENABLE_CONTRACTS)
  target_compile_definitions(cci_util PUBLIC CCI_ENABLE_CONTRACTS=1)
else()
  target_compile_definitions(cci_util PUBLIC CCI_ENABLE_CONTRACTS=0)
endif()
install(TARGETS cci_util
        EXPORT ${PROJECT_NAME}-targets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
