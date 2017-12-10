add_cci_executable(cci
  SOURCES cci.cpp
  DEPENDS CCIBasic CCILex CCIParser fmt::fmt)

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
