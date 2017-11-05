#target_sources(cci
#  PRIVATE
#    ${CMAKE_CURRENT_SOURCE_DIR}/source_manager.cpp
#    ${CMAKE_CURRENT_SOURCE_DIR}/file_stream.cpp)

add_cci_library(CCIBasic
  SOURCES
    source_manager.cpp
    file_stream.cpp)
