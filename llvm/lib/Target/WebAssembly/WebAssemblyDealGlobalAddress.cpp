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
        if (MO.isGlobal() && !MO.getGlobal()->getValueType()->isFunctionTy()) {
          const auto *GV = MO.getGlobal();
          LLVM_DEBUG(dbgs() << "Global Machine Operand:"; GV->dump());
          // todo: may need deal LocalLinkage Type
//          if(!GV->hasLocalLinkage()) {
            StringRef GVName = GV->getName();
            // Global may be "@val + 4", 4 is the offset
            int64_t GVOffset = MO.getOffset();
            MO.setOffset(0);

            // change the global value when it has follow attr
            // it is necessary, or it will be deleted after create symbol
//            if(GV->hasGlobalUnnamedAddr() || GV->hasPrivateLinkage()) {
//              GlobalValue* GVToChange = const_cast<GlobalValue *>(GV);
//              if(GV->hasGlobalUnnamedAddr()) GVToChange->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::None);
//              if(GV->hasPrivateLinkage()) GVToChange->setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
//            }

            // create global symbol
            auto *sym = MF.getContext().getOrCreateSymbol(GVName);
            MCSymbolWasm *Sym = cast<MCSymbolWasm>(sym);
            Sym->setType(wasm::WASM_SYMBOL_TYPE_GLOBAL);

            // set symbol GlobalType
            wasm::WasmGlobalType GlobalType = {wasm::WASM_TYPE_MEMREF, false};
            Sym->setGlobalType(GlobalType);
            LLVM_DEBUG(dbgs() << "create or get wasm global sym:"; Sym->dump());

            Register GVReg =
                MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);

            // current global_get should in correct position, if MI GV, Reg0, Reg1..., then global_get GV should before MO0 in stack
            unsigned RegIdx = Idx + 1;
            for (; RegIdx < NumOperands; RegIdx++)
              if (MI.getOperand(RegIdx).isReg() &&
                  !MI.getOperand(RegIdx).isImplicit())
                break;
            MachineInstr *InsertMI = &MI;
            if (RegIdx < NumOperands) {
              InsertMI = MRI.getVRegDef(MI.getOperand(RegIdx).getReg());
            }

            // build new MI
            BuildMI(*InsertMI->getParent(), InsertMI, InsertMI->getDebugLoc(),
                    TII->get(WebAssembly::GLOBAL_GET_MEMREF), GVReg)
                .addSym(sym);

            // deal Offset
            if (GVOffset) {
              Register OffsetReg =
                  MRI.createVirtualRegister(&WebAssembly::I32RegClass);
              BuildMI(*InsertMI->getParent(), InsertMI, InsertMI->getDebugLoc(),
                      TII->get(WebAssembly::CONST_I32), OffsetReg)
                  .addImm(GVOffset); // i32.const GVOffset

              Register AddResReg =
                  MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
              BuildMI(*InsertMI->getParent(), InsertMI, InsertMI->getDebugLoc(),
                      TII->get(WebAssembly::MEMREF_ADD), AddResReg)
                  .addReg(GVReg)
                  .addReg(OffsetReg); // memref.add GV, Offset
              GVReg = AddResReg;
            }
            // change to Reg
            MO.ChangeToRegister(GVReg, false);
//          }

          Changed = true;
        }
      }
    }
  }
  return Changed;
}
