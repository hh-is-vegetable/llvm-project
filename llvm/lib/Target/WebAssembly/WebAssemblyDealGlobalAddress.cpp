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
#include "Utils/WebAssemblyUtilities.h"
#include "WebAssembly.h"
#include "WebAssemblyDebugValueManager.h"
#include "WebAssemblyMachineFunctionInfo.h"
#include "WebAssemblySubtarget.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/MC/MCContext.h"
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

  for (MachineBasicBlock::iterator I = MF.begin()->begin(),
                                   E = MF.begin()->end();
       I != E;) {
    MachineInstr &MI = *I++;

    unsigned NumOperands = MI.getNumOperands();
    for(unsigned Idx = 0; Idx < NumOperands; Idx++) {
      MachineOperand& MO = MI.getOperand(Idx);
      // try to find the MI, whose Machine Operand is global value
      if(MO.isGlobal()) {
        const auto *GV = MO.getGlobal();
        LLVM_DEBUG(dbgs() << "Global Machine Operand:"; GV->dump());
        StringRef GVName = GV->getName();
        // Global may be "@val + 4", 4 is the offset
        int64_t GVOffset = MO.getOffset();
        MO.setOffset(0);

        // create symbol: .global.Name
//        auto *Sym = MF.getContext().getOrCreateSymbol(GVName);

//        const char *ES = "__stack_pointer";
//        auto *SPSymbol = MF.createExternalSymbolName(ES);
//        BuildMI(MBB, InsertPt, DL, TII->get(getOpcGlobGet(MF)), SPReg)
//            .addExternalSymbol(SPSymbol);
        auto * sym = MF.getContext().getOrCreateSymbol(GVName);
        MCSymbolWasm* Sym = cast<MCSymbolWasm>(sym);
        Sym->setType(wasm::WASM_SYMBOL_TYPE_GLOBAL);

//        // global size and type
//        uint64_t TySize =
//            MF.getDataLayout().getTypeAllocSize(GV->getValueType()).getKnownMinSize();
//        const auto *SizeExpr = MCConstantExpr::create(TySize, MF.getContext());
//        Sym->setSize(SizeExpr);
////        sym->setGlobalSize(TySize);
        wasm::WasmGlobalType GlobalType = {wasm::WASM_TYPE_MEMREF, false};
        Sym->setGlobalType(GlobalType);

        LLVM_DEBUG(dbgs() << "create or get wasm global sym:"; Sym->dump());
        Register Dest = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);

        // current global_get should in correct position, if MI GV, Reg0, Reg1..., then global_get GV should before MO0 in stack
        unsigned RegIdx = Idx+1;
        for(;RegIdx < NumOperands; RegIdx++)if(MI.getOperand(RegIdx).isReg() && !MI.getOperand(RegIdx).isImplicit())break;
        MachineInstr * InsertMI = nullptr;
        if(RegIdx < NumOperands ) {
          InsertMI = MRI.getVRegDef(MI.getOperand(RegIdx).getReg());
        }
        if(InsertMI == nullptr)InsertMI = &MI;

        // build new MI
        BuildMI(*InsertMI->getParent(), InsertMI, InsertMI->getDebugLoc(), TII->get(WebAssembly::GLOBAL_GET_MEMREF), Dest)
            .addSym(sym);
//        BuildMI(*MI.getParent(), &MI, MI.getDebugLoc(), TII->get(WebAssembly::GLOBAL_GET_MEMREF), Dest)
//            .addSym(Sym);

        // deal Offset
        if(GVOffset) {
          // fixme:we can refer to WebAssemblyRegisterInfo.cpp
          //  WebAssembly::getNamedOperandIdx(
          //      MI.getOpcode(), WebAssembly::OpName::addr)

          // i32.const GVOffset
          Register I32Reg = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
          BuildMI(*InsertMI->getParent(), InsertMI, InsertMI->getDebugLoc(), TII->get(WebAssembly::CONST_I32), I32Reg)
              .addImm(GVOffset);
          // memref.add
          Register newReg = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
          BuildMI(*InsertMI->getParent(), InsertMI, InsertMI->getDebugLoc(), TII->get(WebAssembly::MEMREF_ADD), newReg)
            .addReg(Dest)
            .addReg(I32Reg);

          Dest = newReg;
        }

        // change to Reg
        MO.ChangeToRegister(Dest, false);

        Changed = true;
      }
    }

  }

  return Changed;
}
