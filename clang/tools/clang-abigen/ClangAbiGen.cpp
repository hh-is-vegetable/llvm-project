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

// static cl::opt<bool>
//     Fixit("fixit", cl::desc(Options.getOptionHelpText(options::OPT_fixit)),
//           cl::cat(ClangAbiGen));
// static cl::opt<bool> FixWhatYouCan(
//     "fix-what-you-can",
//     cl::desc(Options.getOptionHelpText(options::OPT_fix_what_you_can)),
//     cl::cat(ClangAbiGen));

static cl::opt<bool> SyntaxTreeDump("syntax-tree-dump",
                                    cl::desc("dump the syntax tree"),
                                    cl::cat(ClangAbiGen));
static cl::opt<bool> TokensDump("tokens-dump",
                                cl::desc("dump the preprocessed tokens"),
                                cl::cat(ClangAbiGen));

namespace {

// // FIXME: Move FixItRewriteInPlace from lib/Rewrite/Frontend/FrontendActions.cpp
// // into a header file and reuse that.
// class FixItOptions : public clang::FixItOptions {
// public:
//   FixItOptions() {
//     FixWhatYouCan = ::FixWhatYouCan;
//   }

//   std::string RewriteFilename(const std::string& filename, int &fd) override {
//     // We don't need to do permission checking here since clang will diagnose
//     // any I/O errors itself.

//     fd = -1;  // No file descriptor for file.

//     return filename;
//   }
// };

// /// Subclasses \c clang::FixItRewriter to not count fixed errors/warnings
// /// in the final error counts.
// ///
// /// This has the side-effect that clang-check -fixit exits with code 0 on
// /// successfully fixing all errors.
// class FixItRewriter : public clang::FixItRewriter {
// public:
//   FixItRewriter(clang::DiagnosticsEngine& Diags,
//                 clang::SourceManager& SourceMgr,
//                 const clang::LangOptions& LangOpts,
//                 clang::FixItOptions* FixItOpts)
//       : clang::FixItRewriter(Diags, SourceMgr, LangOpts, FixItOpts) {
//   }

//   bool IncludeInDiagnosticCounts() const override { return false; }
// };

// /// Subclasses \c clang::FixItAction so that we can install the custom
// /// \c FixItRewriter.
// class ClangCheckFixItAction : public clang::FixItAction {
// public:
//   bool BeginSourceFileAction(clang::CompilerInstance& CI) override {
//     FixItOpts.reset(new FixItOptions);
//     Rewriter.reset(new FixItRewriter(CI.getDiagnostics(), CI.getSourceManager(),
//                                      CI.getLangOpts(), FixItOpts.get()));
//     return true;
//   }
// };

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

class ABIGenClassVisitor
  : public RecursiveASTVisitor<ABIGenClassVisitor> {
public:
  bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
    // For debugging, dumping the AST nodes will show which nodes are already
    // being visited.
    Declaration->dump();

    // The return value indicates whether we want the visitation to proceed.
    // Return false to stop the traversal of the AST.
    return true;
  }
  bool VisitCXXMethodDecl(clang::CXXMethodDecl* decl) {
    decl->dump();
    return true;
  }
};

class ABIGenClassConsumer : public clang::ASTConsumer {
public:
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
  // else if (Fixit)
  //   FrontendFactory = newFrontendActionFactory<ClangCheckFixItAction>();
  else if (SyntaxTreeDump || TokensDump)
    FrontendFactory = newFrontendActionFactory<DumpSyntaxTree>();
  else
    FrontendFactory = newFrontendActionFactory(&CheckFactory);

  return Tool.run(FrontendFactory.get());
}
