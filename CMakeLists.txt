cmake_minimum_required(VERSION 3.9)
project(cci VERSION 0.1.0)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(GetGitRevisionDescription)
include(CTest)
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

option(CCI_ENABLE_CONTRACTS "Enable contracts (assertions). This makes the binary slow." ON)
option(CCI_USE_GIT_REVISION "Use Git revision to make up CCI's build version." ON)

# CCI depends on fmtlib 5.0.0 or later.
find_package(FMT 5 REQUIRED)

set(CMAKE_CONFIG_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

# Includes building pieces of the compiler.
add_subdirectory(lib)
#add_subdirectory(src)

if (BUILD_TESTING)
  add_subdirectory(unittest)
endif()

install(DIRECTORY include/cci DESTINATION include)
install(EXPORT ${PROJECT_NAME}-targets
  NAMESPACE cci::
  DESTINATION ${CMAKE_CONFIG_INSTALL_DIR})

# There's possibly a better way to do this, but let's keep it as is for now.
# https://github.com/Sarcasm/slog/blob/master/CMakeLists.txt#L88-L106
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}/${PROJECT_NAME}-config-version.cmake"
  VERSION ${${PROJECT_NAME}_VERSION}
  COMPATIBILITY SameMajorVersion)
export(EXPORT ${PROJECT_NAME}-targets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}/${PROJECT_NAME}-targets.cmake"
  NAMESPACE cci::)
configure_file(cmake/${PROJECT_NAME}-config.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}/${PROJECT_NAME}-config.cmake"
  COPYONLY)

install(
  FILES
    cmake/${PROJECT_NAME}-config.cmake
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}/${PROJECT_NAME}-config-version.cmake"
  DESTINATION
    ${CMAKE_CONFIG_INSTALL_DIR})
