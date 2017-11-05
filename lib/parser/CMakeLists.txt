#target_sources(cci
#  PRIVATE
#    ${CMAKE_CURRENT_SOURCE_DIR}/parser.cpp
#)

add_cci_library(CCIParser
  SOURCES
    parser.cpp
  DEPENDS
    CCIBasic
    CCILex)
