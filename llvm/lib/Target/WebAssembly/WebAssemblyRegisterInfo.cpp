//===-- WebAssemblyRegisterInfo.cpp - WebAssembly Register Information ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the WebAssembly implementation of the
/// TargetRegisterInfo class.
///
//===----------------------------------------------------------------------===//

// fixme:addr and mref can exist at the same time

#include "WebAssemblyRegisterInfo.h"
#include "MCTargetDesc/WebAssemblyMCTargetDesc.h"
#include "WebAssemblyFrameLowering.h"
#include "WebAssemblyInstrInfo.h"
#include "WebAssemblyMachineFunctionInfo.h"
#include "WebAssemblySubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
using namespace llvm;

#define DEBUG_TYPE "wasm-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "WebAssemblyGenRegisterInfo.inc"
#include "llvm/Target/TargetMachine.h"

WebAssemblyRegisterInfo::WebAssemblyRegisterInfo(const Triple &TT)
    : WebAssemblyGenRegisterInfo(0), TT(TT) {}

const MCPhysReg *
WebAssemblyRegisterInfo::getCalleeSavedRegs(const MachineFunction *) const {
  static const MCPhysReg CalleeSavedRegs[] = {0};
  return CalleeSavedRegs;
}

// Reserved Registers are special registers
BitVector
WebAssemblyRegisterInfo::getReservedRegs(const MachineFunction & /*MF*/) const {
  BitVector Reserved(getNumRegs());
  for (auto Reg : {WebAssembly::SP32, WebAssembly::SP64, WebAssembly::FP32,
                   WebAssembly::FP64})
    Reserved.set(Reg);
  return Reserved;
}

void WebAssemblyRegisterInfo::eliminateFrameIndex(
    MachineBasicBlock::iterator II, int SPAdj, unsigned FIOperandNum,
    RegScavenger * /*RS*/) const {
  assert(SPAdj == 0);
  MachineInstr &MI = *II;

  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  int64_t FrameOffset = MFI.getStackSize() + MFI.getObjectOffset(FrameIndex);

  assert(MFI.getObjectSize(FrameIndex) != 0 &&
         "We assume that variable-sized objects have already been lowered, "
         "and don't use FrameIndex operands.");
  Register FrameRegister = getFrameRegister(MF); // TODO:here fp is just copy from sp, fix it if necessary
  const auto *TII = MF.getSubtarget<WebAssemblySubtarget>().getInstrInfo();

  if(MI.getMF()->getTarget().hasWasmMemref()) {
    auto & frameIdx2Reg = MF.getFrameInfo().FrameIdx2Reg;
    if(!frameIdx2Reg.count(FrameIndex)) {
      // we insert alloc instruction after sp define instruction in entry block
      assert(MRI.hasOneDef(FrameRegister) && "SP should has only one def");
      MachineInstr* SpDefInstr = MRI.getOneDef(FrameRegister)->getParent();
      MachineBasicBlock* InsertBB = SpDefInstr->getParent();
      assert(&*MF.begin() == InsertBB && "SP should in entry block");
      MachineBasicBlock::iterator InsertIt = SpDefInstr;
      ++InsertIt;
      // the last instruction should be br or others
      assert(InsertIt != InsertBB->end() && "there should be at least one instruction after sp def instruction");

      Register FrameOffReg = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      Register FrameAddr = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      Register BaseVal;
      Register SizeVal = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      // Register AttrVal = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
      Register FrameObjMemRef = MRI.createVirtualRegister(&WebAssembly::MEMREFRegClass);
      BuildMI(*InsertBB, *InsertIt, InsertIt->getDebugLoc(),
              TII->get(WebAssembly::MEMREF_FIELD), FrameAddr)
          .addImm(0) // <addr, base, size, attr>, addr->0
          .addReg(FrameRegister); // %FrameAddr = memref.field 0, %FrameRegister
      if (FrameOffset != 0) {
        BaseVal = MRI.createVirtualRegister(&WebAssembly::I32RegClass);
        BuildMI(*InsertBB, *InsertIt, InsertIt->getDebugLoc(),
                TII->get(WebAssembly::CONST_I32), FrameOffReg)
            .addImm(FrameOffset); // %FrameOffReg = i32.const FrameOffset
        BuildMI(*InsertBB, *InsertIt, InsertIt->getDebugLoc(),
                TII->get(WebAssembly::ADD_I32), BaseVal)
            .addReg(FrameAddr)
            .addReg(FrameOffReg); // %BaseVal = i32.add %FrameAddr, %FrameOffReg
      } else {
        BaseVal = FrameAddr;
      }
      BuildMI(*InsertBB, *InsertIt, InsertIt->getDebugLoc(),
              TII->get(WebAssembly::CONST_I32), SizeVal)
          .addImm(MFI.getObjectSize(FrameIndex));
      // BuildMI(*InsertBB, *InsertIt, InsertIt->getDebugLoc(),
      //         TII->get(WebAssembly::CONST_I32), AttrVal)
      //     .addImm(0);
      const uint32_t HasMetadataFlag = 0x20; // 0010 0000
      // const uint32_t ValidPointerFlag = 0x10; // 0001 0000
      // const uint32_t HeapVariableFlag = 0x02; // 0000 0010
      // const uint32_t GlobalVariableFlag = 0x01; // 0000 0001
      BuildMI(*InsertBB, *InsertIt, InsertIt->getDebugLoc(),
              TII->get(WebAssembly::MEMREF_ALLOC), FrameObjMemRef)
          .addImm(HasMetadataFlag)
          .addReg(BaseVal)
          .addReg(SizeVal);
      frameIdx2Reg[FrameIndex] = FrameObjMemRef;
    }
    MI.getOperand(FIOperandNum).ChangeToRegister(frameIdx2Reg[FrameIndex], /*isDef=*/false);
    return ;
  }

  // If this is the address operand of a load or store, make it relative to SP
  // and fold the frame offset directly in.
  unsigned AddrOperandNum = WebAssembly::getNamedOperandIdx(
      MI.getOpcode(), WebAssembly::OpName::addr);
  unsigned MemrefOperandNum = WebAssembly::getNamedOperandIdx(
      MI.getOpcode(), WebAssembly::OpName::mref);
  if (AddrOperandNum == FIOperandNum || MemrefOperandNum == FIOperandNum) {
    unsigned OffsetOperandNum = WebAssembly::getNamedOperandIdx(
        MI.getOpcode(), WebAssembly::OpName::off);
    assert(FrameOffset >= 0 && MI.getOperand(OffsetOperandNum).getImm() >= 0);
    int64_t Offset = MI.getOperand(OffsetOperandNum).getImm() + FrameOffset;

    if (static_cast<uint64_t>(Offset) <= std::numeric_limits<uint32_t>::max()) {
      MI.getOperand(OffsetOperandNum).setImm(Offset);
      // fixme: addr and mref is not the same RegisterClass
      MI.getOperand(FIOperandNum)
          .ChangeToRegister(FrameRegister, /*isDef=*/false);
      return;
    }
  }

  // If this is an address/memref being added to a constant, fold the frame offset
  // into the constant.
  if (MI.getOpcode() == WebAssemblyFrameLowering::getOpcAdd(MF)
      || MI.getOpcode() == WebAssemblyFrameLowering::getOpcMemrefAdd()) {
    MachineOperand &OtherMO = MI.getOperand(3 - FIOperandNum);
    if (OtherMO.isReg()) {
      Register OtherMOReg = OtherMO.getReg();
      if (Register::isVirtualRegister(OtherMOReg)) {
        MachineInstr *Def = MF.getRegInfo().getUniqueVRegDef(OtherMOReg);
        // TODO: For now we just opportunistically do this in the case where
        // the CONST_I32/64 happens to have exactly one def and one use. We
        // should generalize this to optimize in more cases.
        if (Def && Def->getOpcode() ==
              WebAssemblyFrameLowering::getOpcConst(MF) &&
            MRI.hasOneNonDBGUse(Def->getOperand(0).getReg())) {
          MachineOperand &ImmMO = Def->getOperand(1);
          if (ImmMO.isImm()) {
            ImmMO.setImm(ImmMO.getImm() + uint32_t(FrameOffset));
            MI.getOperand(FIOperandNum)
                .ChangeToRegister(FrameRegister, /*isDef=*/false);
            return;
          }
        }
      }
    }
  }

  // Otherwise create an i32/64.add SP, offset and make it the operand.

  unsigned FIRegOperand = FrameRegister;
  if (FrameOffset) {
    // Create i32/64.add SP, offset and make it the operand.
    const TargetRegisterClass *PtrRC =
        MRI.getTargetRegisterInfo()->getPointerRegClass(MF);
    Register OffsetOp = MRI.createVirtualRegister(PtrRC);
    BuildMI(MBB, *II, II->getDebugLoc(),
            TII->get(WebAssemblyFrameLowering::getOpcConst(MF)),
            OffsetOp)
        .addImm(FrameOffset);
    FIRegOperand = MRI.createVirtualRegister(PtrRC);
    BuildMI(MBB, *II, II->getDebugLoc(),
            TII->get(MF.getSubtarget<WebAssemblySubtarget>().hasAddr64() ?
                    WebAssemblyFrameLowering::getOpcAdd(MF) : WebAssemblyFrameLowering::getOpcMemrefAdd()),
            FIRegOperand)
        .addReg(FrameRegister)
        .addReg(OffsetOp);
  }
  MI.getOperand(FIOperandNum).ChangeToRegister(FIRegOperand, /*isDef=*/false);
}

Register
WebAssemblyRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  // If the PReg has been replaced by a VReg, return that.
  const auto &MFI = MF.getInfo<WebAssemblyFunctionInfo>();
  if (MFI->isFrameBaseVirtual())
    return MFI->getFrameBaseVreg();
  static const unsigned Regs[2][2] = {
      /*            !isArch64Bit       isArch64Bit      */
      /* !hasFP */ {WebAssembly::SP32, WebAssembly::SP64},
      /*  hasFP */ {WebAssembly::FP32, WebAssembly::FP64}};
  const WebAssemblyFrameLowering *TFI = getFrameLowering(MF);
  return Regs[TFI->hasFP(MF)][TT.isArch64Bit()];
}

const TargetRegisterClass *
WebAssemblyRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                            unsigned Kind) const {
  assert(Kind == 0 && "Only one kind of pointer on WebAssembly");
  if (MF.getSubtarget<WebAssemblySubtarget>().hasAddr64())
    return &WebAssembly::I64RegClass;

  //  return &WebAssembly::I32RegClass; pointer type is memref
  return &WebAssembly::MEMREFRegClass;
}
