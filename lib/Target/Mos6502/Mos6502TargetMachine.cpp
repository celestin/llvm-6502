//===-- Mos6502TargetMachine.cpp - Define TargetMachine for Mos6502 -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "Mos6502TargetMachine.h"
#include "Mos6502TargetObjectFile.h"
#include "Mos6502.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

extern "C" void LLVMInitializeMos6502Target() {
  // Register the target.
  RegisterTargetMachine<Mos6502V8TargetMachine> X(TheMos6502Target);
  RegisterTargetMachine<Mos6502V9TargetMachine> Y(TheMos6502V9Target);
  RegisterTargetMachine<Mos6502elTargetMachine> Z(TheMos6502elTarget);
}

static std::string computeDataLayout(const Triple &T, bool is64Bit) {
  // Mos6502 is typically big endian, but some are little.
  std::string Ret = T.getArch() == Triple::mos6502el ? "e" : "E";
  Ret += "-m:e";

  // Some ABIs have 32bit pointers.
  if (!is64Bit)
    Ret += "-p:32:32";

  // Alignments for 64 bit integers.
  Ret += "-i64:64";

  // On Mos6502V9 128 floats are aligned to 128 bits, on others only to 64.
  // On Mos6502V9 registers can hold 64 or 32 bits, on others only 32.
  if (is64Bit)
    Ret += "-n32:64";
  else
    Ret += "-f128:64-n32";

  if (is64Bit)
    Ret += "-S128";
  else
    Ret += "-S64";

  return Ret;
}

/// Mos6502TargetMachine ctor - Create an ILP32 architecture model
///
Mos6502TargetMachine::Mos6502TargetMachine(const Target &T, const Triple &TT,
                                       StringRef CPU, StringRef FS,
                                       const TargetOptions &Options,
                                       Reloc::Model RM, CodeModel::Model CM,
                                       CodeGenOpt::Level OL, bool is64bit)
    : LLVMTargetMachine(T, computeDataLayout(TT, is64bit), TT, CPU, FS, Options,
                        RM, CM, OL),
      TLOF(make_unique<Mos6502ELFTargetObjectFile>()),
      Subtarget(TT, CPU, FS, *this, is64bit) {
  initAsmInfo();
}

Mos6502TargetMachine::~Mos6502TargetMachine() {}

namespace {
/// Mos6502 Code Generator Pass Configuration Options.
class Mos6502PassConfig : public TargetPassConfig {
public:
  Mos6502PassConfig(Mos6502TargetMachine *TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  Mos6502TargetMachine &getMos6502TargetMachine() const {
    return getTM<Mos6502TargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *Mos6502TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new Mos6502PassConfig(this, PM);
}

void Mos6502PassConfig::addIRPasses() {
  addPass(createAtomicExpandPass(&getMos6502TargetMachine()));

  TargetPassConfig::addIRPasses();
}

bool Mos6502PassConfig::addInstSelector() {
  addPass(createMos6502ISelDag(getMos6502TargetMachine()));
  return false;
}

void Mos6502PassConfig::addPreEmitPass(){
  addPass(createMos6502DelaySlotFillerPass(getMos6502TargetMachine()));
}

void Mos6502V8TargetMachine::anchor() { }

Mos6502V8TargetMachine::Mos6502V8TargetMachine(const Target &T, const Triple &TT,
                                           StringRef CPU, StringRef FS,
                                           const TargetOptions &Options,
                                           Reloc::Model RM, CodeModel::Model CM,
                                           CodeGenOpt::Level OL)
    : Mos6502TargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, false) {}

void Mos6502V9TargetMachine::anchor() { }

Mos6502V9TargetMachine::Mos6502V9TargetMachine(const Target &T, const Triple &TT,
                                           StringRef CPU, StringRef FS,
                                           const TargetOptions &Options,
                                           Reloc::Model RM, CodeModel::Model CM,
                                           CodeGenOpt::Level OL)
    : Mos6502TargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, true) {}

void Mos6502elTargetMachine::anchor() {}

Mos6502elTargetMachine::Mos6502elTargetMachine(const Target &T, const Triple &TT,
                                           StringRef CPU, StringRef FS,
                                           const TargetOptions &Options,
                                           Reloc::Model RM, CodeModel::Model CM,
                                           CodeGenOpt::Level OL)
    : Mos6502TargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, false) {}
