//===-- WebAssemblyDealGlobalAddress.cpp - Add Global.get for globals --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file create symbol for global variables and get global address by GLOBAL_GET.
///
/// Global Address is a const value, we insert GLOBAL_GET to get the global address.
/// Global variables are still modify by load/store instruction.
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

#define DEBUG_TYPE "wasm-global-address"

namespace {
class WebAssemblyDealGlobalAddress final : public MachineFunctionPass {
  StringRef getPassName() const override {
    return "WebAssembly Deal Global Address";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addPreserved<MachineBlockFrequencyInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

public:
  static char ID; // Pass identification, replacement for typeid
  WebAssemblyDealGlobalAddress() : MachineFunctionPass(ID) {}
};
} // end anonymous namespace

char WebAssemblyDealGlobalAddress::ID = 0;
INITIALIZE_PASS(WebAssemblyDealGlobalAddress, DEBUG_TYPE,
                "Insert GLOBAL_GET for global address", false, false)

FunctionPass *llvm::createWebAssemblyDealGlobalAddress() {
  return new WebAssemblyDealGlobalAddress();
}

bool WebAssemblyDealGlobalAddress::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** WebAssembly Deal Global Address **********\n"
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

      unsigned NumOperands = MI.getNumOperands();
      for (unsigned Idx = 0; Idx < NumOperands; Idx++) {
        MachineOperand &MO = MI.getOperand(Idx);
        // try to find the MI, whose Machine Operand is global value
        if (!MO.isGlobal() || MO.getGlobal()->getValueType()->isFunctionTy()) {
          continue;
        }
        const auto *GV = MO.getGlobal();
        LLVM_DEBUG(dbgs() << "Global Machine Operand:"; GV->dump());

        // Global MachineOperand may be "@val + 4", 4 is the offset
        int64_t GVOffset = MO.getOffset();
        MO.setOffset(0);

        // create global symbol
        auto *sym = MF.getContext().getOrCreateSymbol(GV->getName());
        MCSymbolWasm *Sym = cast<MCSymbolWasm>(sym);
        Sym->setType(wasm::WASM_SYMBOL_TYPE_GLOBAL);
        // set symbol GlobalType
        wasm::WasmGlobalType GlobalType = {wasm::WASM_TYPE_MEMREF, false};
        Sym->setGlobalType(GlobalType);
        LLVM_DEBUG(dbgs() << "create or get wasm global sym:"; Sym->dump());

        Register GlobalGetResReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
        // create GLOBAL_GET_MEMREF machine instruction
        BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(),
                TII->get(WebAssembly::GLOBAL_GET_MEMREF), GlobalGetResReg)
            .addSym(sym);

        if (GVOffset) {
          Register OffsetReg =
              MRI.createVirtualRegister(&WebAssembly::I32RegClass);
          BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(),
                  TII->get(WebAssembly::CONST_I32), OffsetReg)
              .addImm(GVOffset); // i32.const GVOffset

          Register AddResReg =
              MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
          BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(),
                  TII->get(WebAssembly::MEMREF_ADD), AddResReg)
              .addReg(GlobalGetResReg)
              .addReg(OffsetReg); // memref.add GV, Offset
          GlobalGetResReg = AddResReg;
        }

        Changed = true;
        if (MI.getOpcode() == WebAssembly::GLOBAL_GET_Addr) {
          Register ToBeReplacedReg = MI.getOperand(0).getReg();
          for (MachineOperand &MO : MRI.use_operands(ToBeReplacedReg)) {
            MO.setReg(GlobalGetResReg);
          }
          MI.eraseFromParent();
          break;
        }else {
          MO.ChangeToRegister(GlobalGetResReg, false);
        }

      } // end for MO
    }
  }
  return Changed;
}
