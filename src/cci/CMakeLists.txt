add_executable(cci cci.cpp)
target_link_libraries(cci PRIVATE
  cci_syntax
  cci_sema
  cci_util)
target_compile_features(cci PUBLIC cxx_std_17)
