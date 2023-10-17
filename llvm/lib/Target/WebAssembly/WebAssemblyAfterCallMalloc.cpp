//===-- WebAssemblyAfterCallMalloc.cpp - Insert memref.alloc after calling malloc --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Insert MEMREF_ALLOC after calling malloc to protect heap object. In wasi-libc, we the real allocator is dlmalloc instead of malloc,
/// but we only focus malloc which used by users and trust wasi-libc.
///
/// We trust the implementation of the malloc function in wasi-libc, and if the malloc function allocates a memory region successfully,
/// we think it declares a memory object.
///
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/WebAssemblyMCTargetDesc.h"
#include "WebAssembly.h"
#include "WebAssemblyDebugValueManager.h"
#include "WebAssemblyMachineFunctionInfo.h"
#include "WebAssemblySubtarget.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/MC/MCContext.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
using namespace llvm;

#define DEBUG_TYPE "wasm-after-call-malloc"

namespace {
class WebAssemblyAfterCallMalloc final : public MachineFunctionPass {
  StringRef getPassName() const override {
    return "WebAssembly After Call Malloc";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addPreserved<MachineBlockFrequencyInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

public:
  static char ID; // Pass identification, replacement for typeid
  WebAssemblyAfterCallMalloc() : MachineFunctionPass(ID) {}
};
} // end anonymous namespace

char WebAssemblyAfterCallMalloc::ID = 0;
INITIALIZE_PASS(WebAssemblyAfterCallMalloc, DEBUG_TYPE,
                "Insert MEMREF_ALLOC after call malloc", false, false)

FunctionPass *llvm::createWebAssemblyAfterCallMalloc() {
  return new WebAssemblyAfterCallMalloc();
}

bool WebAssemblyAfterCallMalloc::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** WebAssembly After Call Malloc **********\n"
                       "********** Function: "
                    << MF.getName() << '\n');

  bool Changed = false;

  MachineRegisterInfo &MRI = MF.getRegInfo();
  const auto *TII = MF.getSubtarget<WebAssemblySubtarget>().getInstrInfo();

  for(auto& MBB : MF) {
    for (MachineBasicBlock::iterator I = MBB.begin(),
                                     E = MBB.end();
         I != E;) {
      MachineInstr &MI = *I++;

      if (!MI.isCall()) continue;
      bool IsMalloc = false;
      const GlobalValue* GV = nullptr;
      for (unsigned Idx = 0; Idx < MI.getNumOperands(); ++Idx) {
        if (!MI.getOperand(Idx).isGlobal() || !MI.getOperand(Idx).getGlobal()->getValueType()->isFunctionTy())continue;
        MachineOperand& MO = MI.getOperand(Idx);
        GV = MO.getGlobal();
        if (GV->getName() == "dlmalloc") {
          IsMalloc = true;
          break;
        }
      }
      if (!IsMalloc) continue;
      assert(I->getOpcode() == WebAssembly::CALL_RESULTS && "CALL_RESULTS is next to CALL_PARAMS, and we don't support RET_CALL_RESULTS");
      LLVM_DEBUG(dbgs() << "dump before change"; MI.getParent()->dump(););

      Register ToBeReplaceReg = I->getOperand(0).getReg();
      Register CallResReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
      I->getOperand(0).ChangeToRegister(CallResReg, true);
      // %base = memref.field 0, %callRes
      // %attr = i32.const 0
      // %null = memref.null
      // %alloc = memref.alloc %base, %size, %attr
      // %select = memref.select %alloc, %null, %base
      Register BaseReg = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      Register SizeReg = MI.getOperand(1).getReg();
      if (MI.getOperand(1).isKill())MI.getOperand(1).setIsKill(false);
      // Register AttrReg = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      Register NullReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
      Register AllocReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
      Register SelectReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
      auto InsertPos = std::next(I);
      // Insert after CALL_RESULTS
      BuildMI(*MI.getParent(), InsertPos, MI.getDebugLoc(), TII->get(WebAssembly::MEMREF_FIELD), BaseReg)
          .addImm(0)
          .addReg(CallResReg);

      // BuildMI(*MI.getParent(), InsertPos, MI.getDebugLoc(), TII->get(WebAssembly::CONST_I32), AttrReg)
      //     .addImm(0);
      BuildMI(*MI.getParent(), InsertPos, MI.getDebugLoc(), TII->get(WebAssembly::MEMREF_NULL), NullReg);
      BuildMI(*MI.getParent(), InsertPos, MI.getDebugLoc(), TII->get(WebAssembly::MEMREF_ALLOC), AllocReg)
          .addImm(0x22) //attr:0010 0010 valid metada, heap
          .addReg(BaseReg)
          .addReg(SizeReg);
      BuildMI(*MI.getParent(), InsertPos, MI.getDebugLoc(), TII->get(WebAssembly::SELECT_MEMREF), SelectReg)
          .addReg(AllocReg)
          .addReg(NullReg)
          .addReg(BaseReg);

      for (auto USE_MO = MRI.use_begin(ToBeReplaceReg); USE_MO != MRI.use_end();) {
        auto MO = USE_MO++;
        MO->setReg(SelectReg);
      }

      LLVM_DEBUG(dbgs() << "dump a after"; MI.getParent()->dump(););
    }
  }
  return Changed;
}
