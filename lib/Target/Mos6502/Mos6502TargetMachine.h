//===-- Mos6502TargetMachine.h - Define TargetMachine for Mos6502 ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the Mos6502 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MOS6502_MOS6502TARGETMACHINE_H
#define LLVM_LIB_TARGET_MOS6502_MOS6502TARGETMACHINE_H

#include "Mos6502InstrInfo.h"
#include "Mos6502Subtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class Mos6502TargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  Mos6502Subtarget Subtarget;
public:
  Mos6502TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                     StringRef FS, const TargetOptions &Options,
                     Reloc::Model RM, CodeModel::Model CM, CodeGenOpt::Level OL,
                     bool is64bit);
  ~Mos6502TargetMachine() override;

  const Mos6502Subtarget *getSubtargetImpl(const Function &) const override {
    return &Subtarget;
  }

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
};

/// Mos6502V8TargetMachine - Mos6502 32-bit target machine
///
class Mos6502V8TargetMachine : public Mos6502TargetMachine {
  virtual void anchor();
public:
  Mos6502V8TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                       StringRef FS, const TargetOptions &Options,
                       Reloc::Model RM, CodeModel::Model CM,
                       CodeGenOpt::Level OL);
};

/// Mos6502V9TargetMachine - Mos6502 64-bit target machine
///
class Mos6502V9TargetMachine : public Mos6502TargetMachine {
  virtual void anchor();
public:
  Mos6502V9TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                       StringRef FS, const TargetOptions &Options,
                       Reloc::Model RM, CodeModel::Model CM,
                       CodeGenOpt::Level OL);
};

class Mos6502elTargetMachine : public Mos6502TargetMachine {
  virtual void anchor();

public:
  Mos6502elTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                       StringRef FS, const TargetOptions &Options,
                       Reloc::Model RM, CodeModel::Model CM,
                       CodeGenOpt::Level OL);
};

} // end namespace llvm

#endif
