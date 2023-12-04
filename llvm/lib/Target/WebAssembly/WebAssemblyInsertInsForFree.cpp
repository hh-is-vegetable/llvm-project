//===-- WebAssemblyInsertInsForFree.cpp - Insert Instructions for free --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Insert instructions before/after calling free. In wasi-libc, we don't care about specific realization, we only care about before/after use.
///
/// We trust the implementation of the free function in wasi-libc, and if the free function frees a memory region successfully,
/// we think a memory object is deleted.
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

#define DEBUG_TYPE "wasm-insert-instructions-for-free"

namespace {
class WebAssemblyInsertInsForFree final : public MachineFunctionPass {
  StringRef getPassName() const override {
    return "WebAssembly Insert Instructions For Free";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addPreserved<MachineBlockFrequencyInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

public:
  static char ID; // Pass identification, replacement for typeid
  WebAssemblyInsertInsForFree() : MachineFunctionPass(ID) {}
};
} // end anonymous namespace

char WebAssemblyInsertInsForFree::ID = 0;
INITIALIZE_PASS(WebAssemblyInsertInsForFree, DEBUG_TYPE,
                "Insert Instructions For Free", false, false)

FunctionPass *llvm::createWebAssemblyInsertInsForFree() {
  return new WebAssemblyInsertInsForFree();
}

bool WebAssemblyInsertInsForFree::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** Insert Instructions For Free **********\n"
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
      bool IsFree = false;
      const GlobalValue* GV = nullptr;
      for (unsigned Idx = 0; Idx < MI.getNumOperands(); ++Idx) {
        if (!MI.getOperand(Idx).isGlobal() || !MI.getOperand(Idx).getGlobal()->getValueType()->isFunctionTy())continue;
        MachineOperand& MO = MI.getOperand(Idx);
        GV = MO.getGlobal();
        if (GV->getName() == "free") {
          IsFree = true;
          break;
        }
      }
      if (!IsFree) continue;
      LLVM_DEBUG(dbgs() << "dump before change"; MI.getParent()->dump(););


      // auto InsertIns = I;
      const uint32_t HeapVariableFlag = 0x02; // 0000 0010
      // if (I->getOpcode() == WebAssembly::CALL_RESULTS)InsertIns = std::next(InsertIns);
      BuildMI(MBB, &MI, MI.getDebugLoc(), TII->get(WebAssembly::MEMREF_DEALLOC))
          .addImm(HeapVariableFlag)
          .addReg(MI.getOperand(1).getReg());
      // Register ToBeFreeReg = MI.getOperand(1).getReg();
      // Register AddrIntValReg = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      // Register ZeroConstReg = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      // Register AllocReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
      // // %toBeFree
      // // %AddrIntVal = memref.field 0, %toBeFree
      // // %zero = i32.const 0
      // // %alloc = memref.alloc %AddrIntVal, %zero, %zero
      // BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(), TII->get(WebAssembly::MEMREF_FIELD), AddrIntValReg)
      //     .addImm(0)
      //     .addReg(ToBeFreeReg);
      //  BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(), TII->get(WebAssembly::CONST_I32), ZeroConstReg)
      //      .addImm(0);
      // BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(), TII->get(WebAssembly::MEMREF_ALLOC), AllocReg)
      //     .addImm(0x02) // attr:metadata no use, 0000 0010.TODO:use memref.free
      //     .addReg(AddrIntValReg)
      //     .addReg(ZeroConstReg);
      // MI.getOperand(1).ChangeToRegister(AllocReg, false);


      LLVM_DEBUG(dbgs() << "dump a after"; MI.getParent()->dump(););
    }
  }
  return Changed;
}
