@PACKAGE_INIT@

include(CMakeFindDependencyMacro)
find_dependency(FMT 5)
include("${CMAKE_CURRENT_LIST_DIR}/cci-targets.cmake")
