//===-- Mos6502MCTargetDesc.cpp - Mos6502 Target Descriptions -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Mos6502 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "Mos6502MCTargetDesc.h"
#include "InstPrinter/Mos6502InstPrinter.h"
#include "Mos6502MCAsmInfo.h"
#include "Mos6502TargetStreamer.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "Mos6502GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "Mos6502GenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "Mos6502GenRegisterInfo.inc"

static MCAsmInfo *createMos6502MCAsmInfo(const MCRegisterInfo &MRI,
                                       const Triple &TT) {
  MCAsmInfo *MAI = new Mos6502ELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(SP::O6, true);
  MCCFIInstruction Inst = MCCFIInstruction::createDefCfa(nullptr, Reg, 0);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCInstrInfo *createMos6502MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitMos6502MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createMos6502MCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitMos6502MCRegisterInfo(X, SP::O7);
  return X;
}

static MCSubtargetInfo *
createMos6502MCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  if (CPU.empty())
    CPU = "v8";
  return createMos6502MCSubtargetInfoImpl(TT, CPU, FS);
}

// Code models. Some only make sense for 64-bit code.
//
// SunCC  Reloc   CodeModel  Constraints
// abs32  Static  Small      text+data+bss linked below 2^32 bytes
// abs44  Static  Medium     text+data+bss linked below 2^44 bytes
// abs64  Static  Large      text smaller than 2^31 bytes
// pic13  PIC_    Small      GOT < 2^13 bytes
// pic32  PIC_    Medium     GOT < 2^32 bytes
//
// All code models require that the text segment is smaller than 2GB.

static MCCodeGenInfo *createMos6502MCCodeGenInfo(const Triple &TT,
                                               Reloc::Model RM,
                                               CodeModel::Model CM,
                                               CodeGenOpt::Level OL) {
  MCCodeGenInfo *X = new MCCodeGenInfo();

  // The default 32-bit code model is abs32/pic32 and the default 32-bit
  // code model for JIT is abs32.
  switch (CM) {
  default: break;
  case CodeModel::Default:
  case CodeModel::JITDefault: CM = CodeModel::Small; break;
  }

  X->initMCCodeGenInfo(RM, CM, OL);
  return X;
}

static MCTargetStreamer *
createObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new Mos6502TargetELFStreamer(S);
}

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new Mos6502TargetAsmStreamer(S, OS);
}

static MCInstPrinter *createMos6502MCInstPrinter(const Triple &T,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
  return new Mos6502InstPrinter(MAI, MII, MRI);
}

extern "C" void LLVMInitializeMos6502TargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn Z(TheMos6502Target, createMos6502MCAsmInfo);

  for (Target *T : {&TheMos6502Target}) {
    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createMos6502MCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createMos6502MCRegisterInfo);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createMos6502MCSubtargetInfo);

    // Register the MC Code Emitter.
    TargetRegistry::RegisterMCCodeEmitter(*T, createMos6502MCCodeEmitter);

    // Register the asm backend.
    TargetRegistry::RegisterMCAsmBackend(*T, createMos6502AsmBackend);

    // Register the object target streamer.
    TargetRegistry::RegisterObjectTargetStreamer(*T,
                                                 createObjectTargetStreamer);

    // Register the asm streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T, createTargetAsmStreamer);

    // Register the MCInstPrinter
    TargetRegistry::RegisterMCInstPrinter(*T, createMos6502MCInstPrinter);
  }

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(TheMos6502Target,
                                        createMos6502MCCodeGenInfo);
}
