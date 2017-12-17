add_library(cci_util INTERFACE)
target_include_directories(cci_util INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)
target_compile_features(cci_util INTERFACE cxx_std_17)

if (CCI_ENABLE_CONTRACTS)
  target_compile_definitions(cci_util INTERFACE CCI_ENABLE_CONTRACTS=1)
else()
  target_compile_definitions(cci_util INTERFACE CCI_ENABLE_CONTRACTS=0)
endif()

install(TARGETS cci_util
        EXPORT ${PROJECT_NAME}-targets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
