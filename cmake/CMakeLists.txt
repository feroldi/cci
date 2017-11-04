# CCI_CMAKE_MODULES_DIR is used by the functions defined in local modules,
# it holds the local module directory.
set(CCI_CMAKE_MODULES_DIR ${CMAKE_CURRENT_SOURCE_DIR}/modules PARENT_SCOPE)
add_subdirectory(modules)
