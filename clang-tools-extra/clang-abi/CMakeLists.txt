set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Option
  FrontendOpenMP
  Support
  )

add_clang_tool(clang-abi
  ClangAbi.cpp
  )

clang_target_link_libraries(clang-abi
  PRIVATE
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangRewriteFrontend
  clangSerialization
  clangStaticAnalyzerFrontend
  clangTooling
  clangToolingSyntax
  clangToolingRefactoring

  clangASTMatchers
  clangRewrite
  clangToolingCore
  clangParse
  clangSema
  clangAnalysis
  clangLex
  clangEdit

  LLVMipo
  LLVMScalarOpts
  LLVMInstCombine
  LLVMTransformUtils
  LLVMAnalysis
  LLVMTarget
  LLVMOption
  LLVMMCParser
  LLVMMC
  LLVMObject
  LLVMBitReader
  LLVMCore
  LLVMSupport
  LLVMDemangle
  )
