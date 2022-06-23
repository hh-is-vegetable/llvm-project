//===--- tools/clang-check/ClangCheck.cpp - Clang check tool --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements a clang-check tool that runs clang based on the info
//  stored in a compilation database.
//
//  This tool uses the Clang Tooling infrastructure, see
//    http://clang.llvm.org/docs/HowToSetupToolingForLLVM.html
//  for details on setting it up with LLVM source tree.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
// #include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Rewrite/Frontend/FrontendActions.h"
#include "clang/StaticAnalyzer/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Syntax/BuildTree.h"
#include "clang/Tooling/Syntax/Tokens.h"
#include "clang/Tooling/Syntax/Tree.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendAction.h"
#include <set>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include "llvm/Support/CommandLine.h"
// #include "ClangAbi.hpp"

using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
using namespace std;
using namespace clang;
using namespace std;
// using namespace abigen;

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
    "\tFor example, to run clang-abi on contract files\n"
    "\tif the file is contract.cpp, use:\n"
    "\n"
    "\t  clang-abi contract.cpp\n"
    "\n"
    "\n"
);

static cl::OptionCategory ClangAbiGen("clang-abigen options");

static const opt::OptTable &Options = getDriverOptTable();

static cl::opt<bool> 
    ABIGen("abigen",
            cl::desc("ABI file generation"),
            cl::cat(ClangAbiGen));

static cl::opt<bool>
    ASTDump("ast-dump",
            cl::desc(Options.getOptionHelpText(options::OPT_ast_dump)),
            cl::cat(ClangAbiGen));
static cl::opt<bool>
    ASTList("ast-list",
            cl::desc(Options.getOptionHelpText(options::OPT_ast_list)),
            cl::cat(ClangAbiGen));
static cl::opt<bool>
    ASTPrint("ast-print",
             cl::desc(Options.getOptionHelpText(options::OPT_ast_print)),
             cl::cat(ClangAbiGen));
static cl::opt<std::string> ASTDumpFilter(
    "ast-dump-filter",
    cl::desc(Options.getOptionHelpText(options::OPT_ast_dump_filter)),
    cl::cat(ClangAbiGen));
static cl::opt<bool>
    Analyze("analyze",
            cl::desc(Options.getOptionHelpText(options::OPT_analyze)),
            cl::cat(ClangAbiGen));
static cl::opt<std::string>
    AnalyzerOutput("analyzer-output-path",
                   cl::desc(Options.getOptionHelpText(options::OPT_o)),
                   cl::cat(ClangAbiGen));


static cl::opt<bool> SyntaxTreeDump("syntax-tree-dump",
                                    cl::desc("dump the syntax tree"),
                                    cl::cat(ClangAbiGen));
static cl::opt<bool> TokensDump("tokens-dump",
                                cl::desc("dump the preprocessed tokens"),
                                cl::cat(ClangAbiGen));

namespace {

class DumpSyntaxTree : public clang::ASTFrontendAction {
public:
  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override {
    class Consumer : public clang::ASTConsumer {
    public:
      Consumer(clang::CompilerInstance &CI) : Collector(CI.getPreprocessor()) {}

      void HandleTranslationUnit(clang::ASTContext &AST) override {
        clang::syntax::TokenBuffer TB = std::move(Collector).consume();
        if (TokensDump)
          llvm::outs() << TB.dumpForTests();
        clang::syntax::Arena A(AST.getSourceManager(), AST.getLangOpts(), TB);
        llvm::outs() << clang::syntax::buildSyntaxTree(A, AST)->dump(
            AST.getSourceManager());
      }

    private:
      clang::syntax::TokenCollector Collector;
    };
    return std::make_unique<Consumer>(CI);
  }
};

class ClangCheckActionFactory {
public:
  std::unique_ptr<clang::ASTConsumer> newASTConsumer() {
    if (ASTList)
      return clang::CreateASTDeclNodeLister();
    if (ASTDump)
      return clang::CreateASTDumper(nullptr /*Dump to stdout.*/, ASTDumpFilter,
                                    /*DumpDecls=*/true,
                                    /*Deserialize=*/false,
                                    /*DumpLookups=*/false,
                                    /*DumpDeclTypes=*/false,
                                    clang::ADOF_Default);
    if (ASTPrint)
      return clang::CreateASTPrinter(nullptr, ASTDumpFilter);
    return std::make_unique<clang::ASTConsumer>();
  }
};

// -------------------------------------------abi gen
inline bool is_template_specialization( const clang::QualType& type, const std::vector<std::string>& names ) {
  auto check = [&](const clang::Type* pt) {
      if (auto tst = llvm::dyn_cast<clang::TemplateSpecializationType>(pt)) {
        if (auto rt = llvm::dyn_cast<clang::RecordType>(tst->desugar())) {
            if ( names.empty() ) {
              return true;
            } else {
              for ( auto name : names ) {
                  if ( const auto* decl = rt->getDecl() ) {
                    if (decl->getName().str() == name) {
                        return true;
                    }
                  }
              }
            }
        }
      }
    return false;
  };
  bool is_specialization = false;
  if (auto pt = llvm::dyn_cast<clang::ElaboratedType>(type.getTypePtr())) {
      is_specialization = check(pt->desugar().getTypePtr());
  } else
      is_specialization = check(type.getTypePtr());

  return is_specialization;
}

inline const bool isPrimitive(const clang::Type* t) {
  if(t->isBooleanType() || t->isCharType() || 
     t->isIntegerType() || t->isFloatingType()) 
    return true;
  return false;
}

static void addPrimitive(const clang::Type* t) {

}

static void addClassType(const clang::Type* t) {
  
}

static void addStructType(const clang::Type* t) {
  
}

static void addVectorType(const clang::Type* t) {
  
}

static void addArrayType(const clang::Type* t) {
  
}

class TypeInf;
class AbiGen;

// record abi methods
class MethodSig {
private:
  std::string methodName;
  // inputs and output for method or function
  std::vector<std::string> inputTypes;
  std::vector<std::string> returnTypes;
public:
  std::string getName() {
    return methodName;
  }
  // constructor
  MethodSig(std::string name) {
    methodName = name.c_str();
    inputTypes = vector<std::string>();
    returnTypes = vector<std::string>();
  }
  void addOutputType(const clang::QualType& t) {
    // getNonReferenceType: If Type is a reference type (e.g., const int&),
    // returns the type that the reference refers to ("const int").
    returnTypes.push_back(t.getNonReferenceType().getAsString());
  }
  void addInputType(const clang::QualType& t) {
    // todo: check is void
    inputTypes.push_back(t.getNonReferenceType().getAsString());
  }
  // for debug
  string toString() {
    std::stringstream ss;
    ss << "method name:\n"
       << "\t" << methodName << "\n";
    if(!inputTypes.empty()) {
      ss << "input type:\n";
      for(auto s: inputTypes) {
        ss << "\t" << s << "\n";
      }
    }
    if(!returnTypes.empty()) {
      ss << "output type:\n";
      for(auto s: returnTypes) {
        ss << "\t" << s << "\n";
      }
    }
    return ss.str();
  }
};

// record abi type information
typedef struct TypeInf{
  std::string typeName;
  bool isPrimitive;
  std::vector<std::string> fields;
}TypeInf;

class AbiGen {
  // methods and their param
  std::map<std::string, MethodSig*> abiMethods;
  // type set, only used for abi generate, ignore other type
  std::map<std::string, TypeInf*> abiTypeMap;

public: 
  static AbiGen& get() {
      static AbiGen ag;
      return ag;
  }
  std::map<std::string, MethodSig*> methods() {
    return abiMethods;
  }

  void addAbiType(const clang::QualType& t) {
    // t->dump();
    // getTypePtr return pt
    string typeName = t.getNonReferenceType().getAsString();
    llvm::outs() << "----------type name:" << typeName << "\n";
    // this type is recorded
    if(abiTypeMap.find(typeName) != abiTypeMap.end())
      return;
    // auto tp = t.getNonReferenceType().getTypePtr(); 
    auto tp = t.getTypePtr(); 

    // =========judge type and deal
    // you should judge template first
    if(is_template_specialization(t, {"vector", "array"})) {
      llvm::outs() << "\ttemplate:" << typeName << "\n";
    } 
    if(tp->isVectorType()) {
      llvm::outs() << "\tvector:" << typeName << "\n";
      addVectorType(tp);
    }
    if(tp->isArrayType()) {
        llvm::outs() << "\tarray:" << typeName << "\n";
        addArrayType(tp);
    }
    if(isPrimitive(tp)) {
      if(tp->isCharType())llvm::outs() << "\tchar:" << typeName << "\n";
      if(tp->isIntegerType())llvm::outs() << "\tinteger:" << typeName << "\n", t->dump();
      if(tp->isFloatingType())llvm::outs() << "\tinteger:" << typeName << "\n";
      if(tp->isBooleanType())llvm::outs() << "\tbool:" << typeName << "\n";
      // llvm::outs() << "\tprimitive:" << typeName << "\n";
      addPrimitive(tp);
    }
    if(tp->isClassType()) {
      llvm::outs() << "\tclass:" << typeName << "\n";
      addClassType(tp);
    }
    if(tp->isStructureType()) {
      llvm::outs() << "\tstruct:" << typeName << "\n";
      addStructType(tp);
    }
    if(tp->isVoidType()) {
      llvm::outs() << "\tvoid type:" << typeName << "\n";
    }
    if(tp->isBuiltinType())llvm::outs() << "\tbuiltin type:" << typeName << "\n";
    if(tp->isPointerType())llvm::outs() << "\tpointer:" << typeName << "\n";
    if(tp->isAlignValT())llvm::outs() << "\tAlignValT:" << typeName << "\n";
    if(tp->isElaboratedTypeSpecifier())llvm::outs() << "\tElaborated:" << typeName << "\n";
    {
      // llvm::outs() << "\tdefault:not a type abi allowed\n";
    }
  }

  // handle methods (or functions)
  void addAbiMethod(FunctionDecl* decl) {
    decl->dumpColor();
    // get method name
    auto methodName = decl->getNameAsString();
    llvm::outs() << "add abi method: "
                 << methodName << "\n";
    // check if it already exists
    if(abiMethods.find(methodName) != abiMethods.end())
      return;
    // new MethodSig class
    MethodSig* methodSig = new MethodSig(methodName);
    //  input param type 
    for (auto param : decl->parameters()) {
      // add QualType input
      methodSig->addInputType(param->getType());
      // deal type
      addAbiType(param->getType());
    }
    // return, c++ only has one return
    methodSig->addOutputType(decl->getReturnType());
    // deal return type
    addAbiType(decl->getReturnType());
    // insert into abiMethods
    abiMethods[methodName] = methodSig;
  }
  
};

//-------------------visitor
class ABIGenClassVisitor : public RecursiveASTVisitor<ABIGenClassVisitor> {
public:
  AbiGen& abiGen = AbiGen::get();
  
  explicit ABIGenClassVisitor(ASTContext *Context)
    : Context(Context) {}
  // it is said clang won't visit template instance by default
  bool shouldVisitTemplateInstantiations() const { return true;}

  // c++ struct/union/class
  virtual bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
    // For debugging, dumping the AST nodes will show which nodes are already
    // being visited.
    if(Declaration->hasAttr<FvmContractAttr>()) {
      Declaration->dump();
    }
    // The return value indicates whether we want the visitation to proceed.
    // Return false to stop the traversal of the AST.
    return true;
  }
  // c++ method, no check now
  virtual bool VisitCXXMethodDecl(clang::CXXMethodDecl* decl) {
    // decl->dump();
    return true;
  }
  // function, CXXMethodDecl extends FunctionDecl, so this one is enough
  virtual bool VisitFunctionDecl(FunctionDecl* func_decl) {
    if(func_decl->hasAttr<FvmContractAttr>()) {
      abiGen.addAbiMethod(func_decl);
    }
    // func_decl->dump();
    return true;
  }

private:
  ASTContext *Context;
};

class ABIGenClassConsumer : public clang::ASTConsumer {
public:
  explicit ABIGenClassConsumer(ASTContext *Context)
    : Visitor(Context) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    // Traversing the translation unit decl via a RecursiveASTVisitor
    // will visit all nodes in the AST.
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }
private:
  // A RecursiveASTVisitor implementation.
  ABIGenClassVisitor Visitor;
};

class ABIGenClassAction : public clang::ASTFrontendAction {
public:
  // virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
  //   clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
  //   return std::make_unique<ABIGenClassConsumer>();
  // }
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
    clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::make_unique<ABIGenClassConsumer>(&Compiler.getASTContext()); // get ASTContext
  } 
};

// abi gen---------------------------------------

} // namespace


int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  // Initialize targets for clang module support.
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  auto ExpectedParser =
      CommonOptionsParser::create(argc, argv, ClangAbiGen);
  if (!ExpectedParser) {
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  if (Analyze) {
    // Set output path if is provided by user.
    //
    // As the original -o options have been removed by default via the
    // strip-output adjuster, we only need to add the analyzer -o options here
    // when it is provided by users.
    if (!AnalyzerOutput.empty())
      Tool.appendArgumentsAdjuster(
          getInsertArgumentAdjuster(CommandLineArguments{"-o", AnalyzerOutput},
                                    ArgumentInsertPosition::END));

    // Running the analyzer requires --analyze. Other modes can work with the
    // -fsyntax-only option.
    //
    // The syntax-only adjuster is installed by default.
    // Good: It also strips options that trigger extra output, like -save-temps.
    // Bad:  We don't want the -fsyntax-only when executing the static analyzer.
    //
    // To enable the static analyzer, we first strip all -fsyntax-only options
    // and then add an --analyze option to the front.
    Tool.appendArgumentsAdjuster(
        [&](const CommandLineArguments &Args, StringRef /*unused*/) {
          CommandLineArguments AdjustedArgs;
          for (const std::string &Arg : Args)
            if (Arg != "-fsyntax-only")
              AdjustedArgs.emplace_back(Arg);
          return AdjustedArgs;
        });
    Tool.appendArgumentsAdjuster(
        getInsertArgumentAdjuster("--analyze", ArgumentInsertPosition::BEGIN));
  }

  ClangCheckActionFactory CheckFactory;
  std::unique_ptr<FrontendActionFactory> FrontendFactory;

  // Choose the correct factory based on the selected mode.
  if (Analyze)
    FrontendFactory = newFrontendActionFactory<clang::ento::AnalysisAction>();
  else if(ABIGen)
    FrontendFactory = newFrontendActionFactory<ABIGenClassAction>();
  else if (SyntaxTreeDump || TokensDump)
    FrontendFactory = newFrontendActionFactory<DumpSyntaxTree>();
  else
    FrontendFactory = newFrontendActionFactory(&CheckFactory);

  
  Tool.run(FrontendFactory.get());
  auto methodMap = AbiGen::get().methods();
  for(auto methodIter = methodMap.begin(); methodIter != methodMap.end(); methodIter++) {
    llvm::outs() << (methodIter->second)->toString() << "\n";
  }
  // return Tool.run(FrontendFactory.get());
}
