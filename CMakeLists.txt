cmake_minimum_required(VERSION 3.11)
project(cci)

set(CMAKE_MODULE_PATH
  ${PROJECT_SOURCE_DIR}/cmake/modules
  ${CMAKE_MODULE_PATH})

include(CTest)

option(CCI_CONTRACTS "Enable contracts (assertions). This makes the binary slow." ON)

add_subdirectory(lib)
if (BUILD_TESTING)
  add_subdirectory(unittest)
endif()
