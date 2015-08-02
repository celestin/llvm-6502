//===-- Mos6502RegisterInfo.cpp - MOS6502 Register Information ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the MOS6502 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "Mos6502RegisterInfo.h"
#include "Mos6502.h"
#include "Mos6502MachineFunctionInfo.h"
#include "Mos6502Subtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetInstrInfo.h"

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "Mos6502GenRegisterInfo.inc"

static cl::opt<bool>
ReserveAppRegisters("mos6502-reserve-app-registers", cl::Hidden, cl::init(false),
                    cl::desc("Reserve application registers (%g2-%g4)"));

Mos6502RegisterInfo::Mos6502RegisterInfo() : Mos6502GenRegisterInfo(SP::O7) {}

const MCPhysReg*
Mos6502RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

const uint32_t *
Mos6502RegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                        CallingConv::ID CC) const {
  return CSR_RegMask;
}

const uint32_t*
Mos6502RegisterInfo::getRTCallPreservedMask(CallingConv::ID CC) const {
  return RTCSR_RegMask;
}

BitVector Mos6502RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const Mos6502Subtarget &Subtarget = MF.getSubtarget<Mos6502Subtarget>();
  // FIXME: G1 reserved for now for large imm generation by frame code.
  Reserved.set(SP::G1);

  // G1-G4 can be used in applications.
  if (ReserveAppRegisters) {
    Reserved.set(SP::G2);
    Reserved.set(SP::G3);
    Reserved.set(SP::G4);
  }
  // G5 is not reserved in 64 bit mode.
  if (!Subtarget.is64Bit())
    Reserved.set(SP::G5);

  Reserved.set(SP::O6);
  Reserved.set(SP::I6);
  Reserved.set(SP::I7);
  Reserved.set(SP::G0);
  Reserved.set(SP::G6);
  Reserved.set(SP::G7);

  // Unaliased double registers are not available in non-V9 targets.
  if (!Subtarget.isV9()) {
    for (unsigned n = 0; n != 16; ++n) {
      for (MCRegAliasIterator AI(SP::D16 + n, this, true); AI.isValid(); ++AI)
        Reserved.set(*AI);
    }
  }

  return Reserved;
}

const TargetRegisterClass*
Mos6502RegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                      unsigned Kind) const {
  const Mos6502Subtarget &Subtarget = MF.getSubtarget<Mos6502Subtarget>();
  return Subtarget.is64Bit() ? &SP::I64RegsRegClass : &SP::IntRegsRegClass;
}

static void replaceFI(MachineFunction &MF,
                      MachineBasicBlock::iterator II,
                      MachineInstr &MI,
                      DebugLoc dl,
                      unsigned FIOperandNum, int Offset,
                      unsigned FramePtr)
{
  // Replace frame index with a frame pointer reference.
  if (Offset >= -4096 && Offset <= 4095) {
    // If the offset is small enough to fit in the immediate field, directly
    // encode it.
    MI.getOperand(FIOperandNum).ChangeToRegister(FramePtr, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
    return;
  }

  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  // FIXME: it would be better to scavenge a register here instead of
  // reserving G1 all of the time.
  if (Offset >= 0) {
    // Emit nonnegaive immediates with sethi + or.
    // sethi %hi(Offset), %g1
    // add %g1, %fp, %g1
    // Insert G1+%lo(offset) into the user.
    BuildMI(*MI.getParent(), II, dl, TII.get(SP::SETHIi), SP::G1)
      .addImm(HI22(Offset));


    // Emit G1 = G1 + I6
    BuildMI(*MI.getParent(), II, dl, TII.get(SP::ADDrr), SP::G1).addReg(SP::G1)
      .addReg(FramePtr);
    // Insert: G1+%lo(offset) into the user.
    MI.getOperand(FIOperandNum).ChangeToRegister(SP::G1, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(LO10(Offset));
    return;
  }

  // Emit Negative numbers with sethi + xor
  // sethi %hix(Offset), %g1
  // xor  %g1, %lox(offset), %g1
  // add %g1, %fp, %g1
  // Insert: G1 + 0 into the user.
  BuildMI(*MI.getParent(), II, dl, TII.get(SP::SETHIi), SP::G1)
    .addImm(HIX22(Offset));
  BuildMI(*MI.getParent(), II, dl, TII.get(SP::XORri), SP::G1)
    .addReg(SP::G1).addImm(LOX10(Offset));

  BuildMI(*MI.getParent(), II, dl, TII.get(SP::ADDrr), SP::G1).addReg(SP::G1)
    .addReg(FramePtr);
  // Insert: G1+%lo(offset) into the user.
  MI.getOperand(FIOperandNum).ChangeToRegister(SP::G1, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(0);
}


void
Mos6502RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, unsigned FIOperandNum,
                                       RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();

  // Addressable stack objects are accessed using neg. offsets from %fp
  MachineFunction &MF = *MI.getParent()->getParent();
  const Mos6502Subtarget &Subtarget = MF.getSubtarget<Mos6502Subtarget>();
  int64_t Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex) +
                   MI.getOperand(FIOperandNum + 1).getImm() +
                   Subtarget.getStackPointerBias();
  Mos6502MachineFunctionInfo *FuncInfo = MF.getInfo<Mos6502MachineFunctionInfo>();
  unsigned FramePtr = SP::I6;
  if (FuncInfo->isLeafProc()) {
    // Use %sp and adjust offset if needed.
    FramePtr = SP::O6;
    int stackSize = MF.getFrameInfo()->getStackSize();
    Offset += (stackSize) ? Subtarget.getAdjustedFrameSize(stackSize) : 0 ;
  }

  if (!Subtarget.isV9() || !Subtarget.hasHardQuad()) {
    if (MI.getOpcode() == SP::STQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      unsigned SrcReg = MI.getOperand(2).getReg();
      unsigned SrcEvenReg = getSubReg(SrcReg, SP::sub_even64);
      unsigned SrcOddReg  = getSubReg(SrcReg, SP::sub_odd64);
      MachineInstr *StMI =
        BuildMI(*MI.getParent(), II, dl, TII.get(SP::STDFri))
        .addReg(FramePtr).addImm(0).addReg(SrcEvenReg);
      replaceFI(MF, II, *StMI, dl, 0, Offset, FramePtr);
      MI.setDesc(TII.get(SP::STDFri));
      MI.getOperand(2).setReg(SrcOddReg);
      Offset += 8;
    } else if (MI.getOpcode() == SP::LDQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      unsigned DestReg     = MI.getOperand(0).getReg();
      unsigned DestEvenReg = getSubReg(DestReg, SP::sub_even64);
      unsigned DestOddReg  = getSubReg(DestReg, SP::sub_odd64);
      MachineInstr *StMI =
        BuildMI(*MI.getParent(), II, dl, TII.get(SP::LDDFri), DestEvenReg)
        .addReg(FramePtr).addImm(0);
      replaceFI(MF, II, *StMI, dl, 1, Offset, FramePtr);

      MI.setDesc(TII.get(SP::LDDFri));
      MI.getOperand(0).setReg(DestOddReg);
      Offset += 8;
    }
  }

  replaceFI(MF, II, MI, dl, FIOperandNum, Offset, FramePtr);

}

unsigned Mos6502RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return SP::I6;
}

