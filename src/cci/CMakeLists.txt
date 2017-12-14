add_executable(cci cci.cpp)
target_link_libraries(cci PRIVATE cci_basic cci_lex cci_parser fmt::fmt)
target_compile_features(cci PUBLIC cxx_std_17)

# Takes advantage of git version control to make up the compiler's build version.
if(CCI_USE_GIT_REVISION)
  target_compile_definitions(cci PRIVATE CCI_USING_GIT_REVISION)
  git_get_head_revision(GIT_REFSPEC GIT_HASH)
  git_get_exact_tag(GIT_TAG)
  configure_file(
    ${PROJECT_SOURCE_DIR}/git_revision.cpp.in
    ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cpp
    @ONLY NEWLINE_STYLE UNIX)
  target_sources(cci PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/git_revision.cpp)
endif()
