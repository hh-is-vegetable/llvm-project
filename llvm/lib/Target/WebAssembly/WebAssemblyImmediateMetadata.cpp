//===-- WebAssemblyImmediateMetadata.cpp - Insert Instructions for free --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Some extra metadata can kwon when compiling, we can store them into immediate operand, like load/store.
///
/// there are three information: no check; upper check only; base check only;
///
/// Static Check:
/// If the offset=addr-base and the size is const value, check it in compiler and tell load/store donn't need to check.
/// If the offset=addr-base is const value and offset<0, trap.
/// 
/// Dynamic Check with more information:
/// If we can know the attr, we can tell load/store how to do check--use metadata to check or not use metadata.
/// If the offset=addr-base is const value, if offset < 0 check, else only check the upper bound.
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
#include "llvm/CodeGen/MachineFrameInfo.h"
using namespace llvm;

#define DEBUG_TYPE "wasm-immediate-metadata"

namespace {
class WebAssemblyImmediateMetadata final : public MachineFunctionPass {

  const uint32_t no_check_flag     = 0x0100;     //0000 0001 0000 0000
  const uint32_t lower_check_flag  = 0x0200;     //0000 0010 0000 0000
  const uint32_t upper_check_flag  = 0x0400;     //0000 0100 0000 0000

  StringRef getPassName() const override {
    return "WebAssembly Immediate Metadata";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addPreserved<MachineBlockFrequencyInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  void setFlag(MachineInstr& MI, uint32_t flag) {
    unsigned alignIdx = 0 + (MI.mayLoad() ? 1 : 0);
    uint64_t oldAlign = MI.getOperand(alignIdx).getImm();
    MI.getOperand(alignIdx).setImm(oldAlign | flag);
  }

  void insert2Map(DenseMap<unsigned, SmallVector<MachineInstr*>>&Map, DenseMap<unsigned, MachineInstr*>&Max2Ins,
                  DenseMap<unsigned, MachineInstr*>& Min2Ins, MachineInstr* MI, unsigned key) {
    if (!Map.count(key)) {
      // if not record
      Map[key].push_back(MI);
      Max2Ins[key] = MI;
      Min2Ins[key] = MI;
      return ; // finished to record this MI
    }
    Map[key].push_back(MI);
    // update RegMaxOffIns and RegMinOffIns
    unsigned OffMOIdx = 1 + MI->mayLoad();
    int64_t Off = MI->getOperand(OffMOIdx).getImm();
    if (Off > Max2Ins[key]->getOperand(1+ Max2Ins[key]->mayLoad()).getImm()) {
      Max2Ins[key] = MI;
    }
    if (Off < Min2Ins[key]->getOperand(1+ Min2Ins[key]->mayLoad()).getImm()) {
      Min2Ins[key] = MI;
    }
  }

  bool changeOff(DenseMap<unsigned, SmallVector<MachineInstr*>>& Map, DenseMap<unsigned, MachineInstr*>& Max2Ins,
                 DenseMap<unsigned, MachineInstr*>& Min2Ins) {
//    const uint32_t no_check_flag     = 0x0100;     //0000 0001 0000 0000
//    const uint32_t lower_check_flag  = 0x0200;     //0000 0010 0000 0000
//    const uint32_t upper_check_flag  = 0x0400;     //0000 0100 0000 0000
    bool Changed = false;
    for (auto& it : Map) {
      unsigned key = it.first;
      // if only one use, no need change
      if (it.second.size() <= 1)
        continue ;
      Changed |= true;
      // if min off == max off, and then RegMaxOffIns[reg] == RegMinOffIns[reg], so only one need check
      for (MachineInstr* ins : it.second) {
        unsigned alignIdx = 0 + (ins->mayLoad() ? 1 : 0);
        uint64_t oldAlign = ins->getOperand(alignIdx).getImm();
        if (ins == Max2Ins[key]) {
          if (ins != Min2Ins[key]) // if minOff == maxOff, set NO flag and it will check upper and lower
            ins->getOperand(alignIdx).setImm(oldAlign | upper_check_flag);
        } else if (ins == Min2Ins[key]) {
          ins->getOperand(alignIdx).setImm(oldAlign | lower_check_flag);
        } else {
          ins->getOperand(alignIdx).setImm(oldAlign | no_check_flag);
        }
      }
    }
    return Changed;
  }

  bool isMSLoadOrStore(const MachineInstr &MI, const TargetInstrInfo *TII) {
    StringRef OpName = TII->getName(MI.getOpcode());
    return OpName.contains("MSLOAD") || OpName.contains("MSSTORE");
  }

  unsigned getMemAccessSize(const MachineInstr &MI, const TargetInstrInfo *TII) {
    StringRef OpName = TII->getName(MI.getOpcode());
    if (OpName.contains("8_"))return 1;
    else if (OpName.contains("16_"))return 2;
    else if (OpName.contains("32") || OpName.contains("MEMREF"))return 4;
    else return 8;
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

public:
  static char ID; // Pass identification, replacement for typeid
  WebAssemblyImmediateMetadata() : MachineFunctionPass(ID) {}
};
} // end anonymous namespace

char WebAssemblyImmediateMetadata::ID = 0;
INITIALIZE_PASS(WebAssemblyImmediateMetadata, DEBUG_TYPE,
                "Insert Instructions For Free", false, false)

FunctionPass *llvm::createWebAssemblyImmediateMetadata() {
  return new WebAssemblyImmediateMetadata();
}

bool WebAssemblyImmediateMetadata::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** Immediate Metadata **********\n"
                       "********** Function: "
                    << MF.getName() << '\n');

  bool Changed = false;
//  return Changed;
  MachineRegisterInfo &MRI = MF.getRegInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const auto *TII = MF.getSubtarget<WebAssemblySubtarget>().getInstrInfo();
  for (MachineBasicBlock& MBB : MF) {
    DenseMap<unsigned, SmallVector<MachineInstr*>> RegAddrInstrMap; // addr reg->users in this block
    DenseMap<unsigned, MachineInstr*> RegMaxOffIns;                 // reg->max off
    DenseMap<unsigned, MachineInstr*> RegMinOffIns;                 // reg->min off
    DenseMap<unsigned, SmallVector<MachineInstr*>> FrameAddrInstrMap; // addr reg->users in this block
    DenseMap<unsigned, MachineInstr*> FrameMaxOffIns;                 // reg->max off
    DenseMap<unsigned, MachineInstr*> FrameMinOffIns;                 // reg->min off
    for (MachineInstr& MI : MBB) {
      if (!MI.mayLoadOrStore() || !isMSLoadOrStore(MI, TII))
        continue ;
      // only look up address in store/load
      LLVM_DEBUG(dbgs() << "Load Or Store:"; MI.dump());
      if (MF.getName() == "dlfree" || MF.getName() == "dlmalloc") {
        setFlag(MI, no_check_flag);
        Changed = true;
        continue ;
      }
      unsigned isLoad = MI.mayLoad() ? 1 : 0;
      // %res = msload align, off, mref
      // msload align, off, mref, val
      MachineOperand& MO = MI.getOperand(2+isLoad);
      assert((MO.isFI() || MO.isReg()) && "after deal global addr, load and store address can only be frame object or reg");
      if (MO.isReg()) {
        Register AddrReg =  MO.getReg();
        insert2Map(RegAddrInstrMap, RegMaxOffIns, RegMinOffIns, &MI, AddrReg);
      } else {
        int  FrameIndex =  MO.getIndex();
        // TODO:static check for frame object
//        if (MFI.getObjectSize(FrameIndex) == getMemAccessSize(MI, TII)) {
//          unsigned alignIdx = 0 + (MI.mayLoad() ? 1 : 0);
//          uint64_t oldAlign = MI.getOperand(alignIdx).getImm();
//          MI.getOperand(alignIdx).setImm(oldAlign | no_check_flag);
//        }else
          insert2Map(FrameAddrInstrMap, FrameMaxOffIns, FrameMinOffIns, &MI, FrameIndex);
      }
    }
    // change
    Changed |= changeOff(RegAddrInstrMap, RegMaxOffIns, RegMinOffIns);
    Changed |= changeOff(FrameAddrInstrMap, FrameMaxOffIns, FrameMinOffIns);

  }
  return Changed;
}
