//===-- WebAssemblyChangePrivateGlobal.cpp - Change private Global --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Change Global Internal.
///
/// This is for memref, global now get by local.get, but const string is not.
///
/// When a global variable is private internal and unnamed_addr, it can not work
/// normally after WebAssemblyDealGlobalAddress, so we fix it here.
///
/// We add this file temporarily, we will remove this file and add more efficient
/// way to deal
///
///
//===----------------------------------------------------------------------===//

#include "WebAssembly.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "wasm-change-private-global"

namespace {
class ChangePrivateGlobal final : public ModulePass {
  StringRef getPassName() const override {
    return "WebAssembly Change Private Global";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    ModulePass::getAnalysisUsage(AU);
  }

  bool runOnModule(Module &M) override;

public:
  static char ID;
  ChangePrivateGlobal() : ModulePass(ID) {}
};
} // End anonymous namespace

char ChangePrivateGlobal::ID = 0;
INITIALIZE_PASS(ChangePrivateGlobal, DEBUG_TYPE,
                "Change private globals for WebAssembly", false, false)

ModulePass *llvm::createWebAssemblyChangePrivateGlobal() {
  return new ChangePrivateGlobal();
}

bool ChangePrivateGlobal::runOnModule(Module &M) {
  LLVM_DEBUG(dbgs() << "********** Change Private Globals **********\n");
  bool isChanged = false;
  for (GlobalVariable &GV : M.globals()) {
    if(GV.getValueType()->isFunctionTy())
      continue ;
    if (GV.hasPrivateLinkage() || GV.hasGlobalUnnamedAddr()) {
      if(GV.hasPrivateLinkage()) {
        GV.setLinkage(GlobalValue::InternalLinkage);
      }
      if(GV.hasGlobalUnnamedAddr()) {
        GV.setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::None);
      }
      if(GV.isConstant() && GV.hasInitializer()) {
        LLVM_DEBUG(dbgs() << "global " << GV.getName() << " is const and has initializer:";GV.getInitializer()->dump());
        if(ConstantDataArray* CDA = dyn_cast_or_null<ConstantDataArray>(GV.getInitializer())) {
          if (CDA->isString()) {
            StringRef content = CDA->getAsString();
            unsigned hashV = (unsigned)llvm::hash_value(content);
            std::string newName = "str_" + std::to_string(hashV);
            GV.setName(newName);
            LLVM_DEBUG(dbgs() << "global new name:" << newName << "\n");
          }
        }
      }
      isChanged = true;
    }
  }
  
  return isChanged;
}
