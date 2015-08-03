//===-- Mos6502FrameLowering.cpp - Mos6502 Frame Information ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Mos6502 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "Mos6502FrameLowering.h"
#include "Mos6502InstrInfo.h"
#include "Mos6502MachineFunctionInfo.h"
#include "Mos6502Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

static cl::opt<bool>
DisableLeafProc("disable-mos6502-leaf-proc",
                cl::init(false),
                cl::desc("Disable Mos6502 leaf procedure optimization."),
                cl::Hidden);

Mos6502FrameLowering::Mos6502FrameLowering(const Mos6502Subtarget &ST)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown,
                          ST.is64Bit() ? 16 : 8, 0, ST.is64Bit() ? 16 : 8) {}

void Mos6502FrameLowering::emitSPAdjustment(MachineFunction &MF,
                                          MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator MBBI,
                                          int NumBytes,
                                          unsigned ADDrr,
                                          unsigned ADDri) const {

  DebugLoc dl = (MBBI != MBB.end()) ? MBBI->getDebugLoc() : DebugLoc();
  const Mos6502InstrInfo &TII =
      *static_cast<const Mos6502InstrInfo *>(MF.getSubtarget().getInstrInfo());

  if (NumBytes >= -4096 && NumBytes < 4096) {
    BuildMI(MBB, MBBI, dl, TII.get(ADDri), M6502::O6)
      .addReg(M6502::O6).addImm(NumBytes);
    return;
  }

  // Emit this the hard way.  This clobbers G1 which we always know is
  // available here.
  if (NumBytes >= 0) {
    // Emit nonnegative numbers with sethi + or.
    // sethi %hi(NumBytes), %g1
    // or %g1, %lo(NumBytes), %g1
    // add %sp, %g1, %sp
    BuildMI(MBB, MBBI, dl, TII.get(M6502::SETHIi), M6502::G1)
      .addImm(HI22(NumBytes));
    BuildMI(MBB, MBBI, dl, TII.get(M6502::ORri), M6502::G1)
      .addReg(M6502::G1).addImm(LO10(NumBytes));
    BuildMI(MBB, MBBI, dl, TII.get(ADDrr), M6502::O6)
      .addReg(M6502::O6).addReg(M6502::G1);
    return ;
  }

  // Emit negative numbers with sethi + xor.
  // sethi %hix(NumBytes), %g1
  // xor %g1, %lox(NumBytes), %g1
  // add %sp, %g1, %sp
  BuildMI(MBB, MBBI, dl, TII.get(M6502::SETHIi), M6502::G1)
    .addImm(HIX22(NumBytes));
  BuildMI(MBB, MBBI, dl, TII.get(M6502::XORri), M6502::G1)
    .addReg(M6502::G1).addImm(LOX10(NumBytes));
  BuildMI(MBB, MBBI, dl, TII.get(ADDrr), M6502::O6)
    .addReg(M6502::O6).addReg(M6502::G1);
}

void Mos6502FrameLowering::emitPrologue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
  Mos6502MachineFunctionInfo *FuncInfo = MF.getInfo<Mos6502MachineFunctionInfo>();

  assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");
  MachineFrameInfo *MFI = MF.getFrameInfo();
  const Mos6502InstrInfo &TII =
      *static_cast<const Mos6502InstrInfo *>(MF.getSubtarget().getInstrInfo());
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // Get the number of bytes to allocate from the FrameInfo
  int NumBytes = (int) MFI->getStackSize();

  unsigned SAVEri = M6502::SAVEri;
  unsigned SAVErr = M6502::SAVErr;
  if (FuncInfo->isLeafProc()) {
    if (NumBytes == 0)
      return;
    SAVEri = M6502::ADDri;
    SAVErr = M6502::ADDrr;
  }
  NumBytes = -MF.getSubtarget<Mos6502Subtarget>().getAdjustedFrameSize(NumBytes);
  emitSPAdjustment(MF, MBB, MBBI, NumBytes, SAVErr, SAVEri);

  MachineModuleInfo &MMI = MF.getMMI();
  const MCRegisterInfo *MRI = MMI.getContext().getRegisterInfo();
  unsigned regFP = MRI->getDwarfRegNum(M6502::I6, true);

  // Emit ".cfi_def_cfa_register 30".
  unsigned CFIIndex =
      MMI.addFrameInst(MCCFIInstruction::createDefCfaRegister(nullptr, regFP));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  // Emit ".cfi_window_save".
  CFIIndex = MMI.addFrameInst(MCCFIInstruction::createWindowSave(nullptr));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  unsigned regInRA = MRI->getDwarfRegNum(M6502::I7, true);
  unsigned regOutRA = MRI->getDwarfRegNum(M6502::O7, true);
  // Emit ".cfi_register 15, 31".
  CFIIndex = MMI.addFrameInst(
      MCCFIInstruction::createRegister(nullptr, regOutRA, regInRA));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);
}

void Mos6502FrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  if (!hasReservedCallFrame(MF)) {
    MachineInstr &MI = *I;
    int Size = MI.getOperand(0).getImm();
    if (MI.getOpcode() == M6502::ADJCALLSTACKDOWN)
      Size = -Size;

    if (Size)
      emitSPAdjustment(MF, MBB, I, Size, M6502::ADDrr, M6502::ADDri);
  }
  MBB.erase(I);
}


void Mos6502FrameLowering::emitEpilogue(MachineFunction &MF,
                                  MachineBasicBlock &MBB) const {
  Mos6502MachineFunctionInfo *FuncInfo = MF.getInfo<Mos6502MachineFunctionInfo>();
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const Mos6502InstrInfo &TII =
      *static_cast<const Mos6502InstrInfo *>(MF.getSubtarget().getInstrInfo());
  DebugLoc dl = MBBI->getDebugLoc();
  assert(MBBI->getOpcode() == M6502::RETL &&
         "Can only put epilog before 'retl' instruction!");
  if (!FuncInfo->isLeafProc()) {
    BuildMI(MBB, MBBI, dl, TII.get(M6502::RESTORErr), M6502::G0).addReg(M6502::G0)
      .addReg(M6502::G0);
    return;
  }
  MachineFrameInfo *MFI = MF.getFrameInfo();

  int NumBytes = (int) MFI->getStackSize();
  if (NumBytes == 0)
    return;

  NumBytes = MF.getSubtarget<Mos6502Subtarget>().getAdjustedFrameSize(NumBytes);
  emitSPAdjustment(MF, MBB, MBBI, NumBytes, M6502::ADDrr, M6502::ADDri);
}

bool Mos6502FrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // Reserve call frame if there are no variable sized objects on the stack.
  return !MF.getFrameInfo()->hasVarSizedObjects();
}

// hasFP - Return true if the specified function should have a dedicated frame
// pointer register.  This is true if the function has variable sized allocas or
// if frame pointer elimination is disabled.
bool Mos6502FrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
    MFI->hasVarSizedObjects() || MFI->isFrameAddressTaken();
}


static bool LLVM_ATTRIBUTE_UNUSED verifyLeafProcRegUse(MachineRegisterInfo *MRI)
{

  for (unsigned reg = M6502::I0; reg <= M6502::I7; ++reg)
    if (!MRI->reg_nodbg_empty(reg))
      return false;

  for (unsigned reg = M6502::L0; reg <= M6502::L7; ++reg)
    if (!MRI->reg_nodbg_empty(reg))
      return false;

  return true;
}

bool Mos6502FrameLowering::isLeafProc(MachineFunction &MF) const
{

  MachineRegisterInfo &MRI = MF.getRegInfo();
  MachineFrameInfo    *MFI = MF.getFrameInfo();

  return !(MFI->hasCalls()                 // has calls
           || !MRI.reg_nodbg_empty(M6502::L0) // Too many registers needed
           || !MRI.reg_nodbg_empty(M6502::O6) // %SP is used
           || hasFP(MF));                  // need %FP
}

void Mos6502FrameLowering::remapRegsForLeafProc(MachineFunction &MF) const {

  MachineRegisterInfo &MRI = MF.getRegInfo();

  // Remap %i[0-7] to %o[0-7].
  for (unsigned reg = M6502::I0; reg <= M6502::I7; ++reg) {
    if (MRI.reg_nodbg_empty(reg))
      continue;
    unsigned mapped_reg = (reg - M6502::I0 + M6502::O0);
    assert(MRI.reg_nodbg_empty(mapped_reg));

    // Replace I register with O register.
    MRI.replaceRegWith(reg, mapped_reg);
  }

  // Rewrite MBB's Live-ins.
  for (MachineFunction::iterator MBB = MF.begin(), E = MF.end();
       MBB != E; ++MBB) {
    for (unsigned reg = M6502::I0; reg <= M6502::I7; ++reg) {
      if (!MBB->isLiveIn(reg))
        continue;
      MBB->removeLiveIn(reg);
      MBB->addLiveIn(reg - M6502::I0 + M6502::O0);
    }
  }

  assert(verifyLeafProcRegUse(&MRI));
#ifdef XDEBUG
  MF.verify(0, "After LeafProc Remapping");
#endif
}

void Mos6502FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  if (!DisableLeafProc && isLeafProc(MF)) {
    Mos6502MachineFunctionInfo *MFI = MF.getInfo<Mos6502MachineFunctionInfo>();
    MFI->setLeafProc(true);

    remapRegsForLeafProc(MF);
  }

}
