add_library(cci_parser parser.cpp)
target_include_directories(cci_parser
  PUBLIC $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
  PUBLIC $<INSTALL_INTERFACE:include>)
target_link_libraries(cci_parser PRIVATE cci_basic cci_lex cci_util)
target_compile_features(cci_parser PUBLIC cxx_std_17)
install(TARGETS cci_parser
        EXPORT ${PROJECT_NAME}-targets
        DESTINATION ${CMAKE_INSTALL_LIBDIR})
