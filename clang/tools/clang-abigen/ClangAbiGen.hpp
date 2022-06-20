
#pragma once 

#include "clang/AST/AST.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/QualTypeNames.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/Rewriters.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
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

using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;
using namespace std;
// using namespace abigen;

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp(
    "\tFor example, to run clang-check on all files in a subtree of the\n"
    "\tsource tree, use:\n"
    "\n"
    "\t  find path/in/subtree -name '*.cpp'|xargs clang-check\n"
    "\n"
    "\tor using a specific build path:\n"
    "\n"
    "\t  find path/in/subtree -name '*.cpp'|xargs clang-check -p build/path\n"
    "\n"
    "\tNote, that path/in/subtree and current directory should follow the\n"
    "\trules described above.\n"
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

namespace /*abigen*/ {

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
class abiGen {
  // inputs and output for method
  typedef struct methodParamTypes{
    std::vector<std::string> inputs;
    std::vector<std::string> output;
  } methodParamTypes; 
  std::map<std::string, methodParamTypes> abiMethods;
  std::set<std::string> typeSet;
public:
  void addType(std::string type) {
    typeSet.insert(type);
  }

};

// class ABIGenClassVisitor : public RecursiveASTVisitor<ABIGenClassVisitor> {
// public:
//   explicit ABIGenClassVisitor(CompilerInstance *CI) {
//     get_error_emitter().set_compiler_instance(CI);
//   }

//   bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
//     // For debugging, dumping the AST nodes will show which nodes are already
//     // being visited.
//     Declaration->dump();

//     // The return value indicates whether we want the visitation to proceed.
//     // Return false to stop the traversal of the AST.
//     return true;
//   }
//   bool VisitCXXMethodDecl(clang::CXXMethodDecl* decl) {
//     decl->dump();
//     return true;
//   }
// };

// class ABIGenClassConsumer : public clang::ASTConsumer {
// public:
//   virtual void HandleTranslationUnit(clang::ASTContext &Context) {
//     // Traversing the translation unit decl via a RecursiveASTVisitor
//     // will visit all nodes in the AST.
//     Visitor.TraverseDecl(Context.getTranslationUnitDecl());
//   }
// private:
//   // A RecursiveASTVisitor implementation.
//   ABIGenClassVisitor Visitor;
// };

// class ABIGenClassAction : public clang::ASTFrontendAction {
// public:
//   // virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
//   //   clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
//   //   return std::make_unique<ABIGenClassConsumer>();
//   // }
//   virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
//     clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
//     return std::make_unique<ABIGenClassConsumer>(&Compiler.getASTContext()); // get ASTContext
//   } 
// };

// // abi gen---------------------------------------

} // namespace


// class FindNamedClassVisitor
//   : public RecursiveASTVisitor <FindNamedClassVisitor> {
// public:
//   bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
//     // For debugging, dumping the AST nodes will show which nodes are already
//     // being visited.
//     Declaration->dump();

//     // The return value indicates whether we want the visitation to proceed.
//     // Return false to stop the traversal of the AST.
//     return true;
//   }
// };
// namespace {
//     class eosio_abigen_visitor : public RecursiveASTVisitor<eosio_abigen_visitor> {
      

//     public:
//         explicit eosio_abigen_visitor(CompilerInstance *CI) {
//         get_error_emitter().set_compiler_instance(CI);
//         }

//         bool shouldVisitTemplateInstantiations() const {
//         return true;
//         }

//         virtual bool VisitCXXMethodDecl(clang::CXXMethodDecl* decl) {
//         return true;
//         }
//         virtual bool VisitCXXRecordDecl(clang::CXXRecordDecl* decl) {
        
//         return true;
//         }
//         virtual bool VisitDecl(clang::Decl* decl) {
        
//         return true;
//         }
//     };
// }
    