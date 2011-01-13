set(LLVM_NO_RTTI 1)

add_clang_library(clangSourceSieve
  SourceSieve.cpp
  )

target_link_libraries(clangSourceSieve
  #  LLVMCore

  clangFrontend
  clangAST
  clangBasic
  clangLex
  )

IF(MSVC)
  get_target_property(NON_ANSI_COMPILE_FLAGS clangSourceSieve COMPILE_FLAGS)
  string(REPLACE /Za
    "" NON_ANSI_COMPILE_FLAGS
    ${NON_ANSI_COMPILE_FLAGS})
  set_target_properties(clangSourceSieve PROPERTIES COMPILE_FLAGS ${NON_ANSI_COMPILE_FLAGS})
ENDIF(MSVC)