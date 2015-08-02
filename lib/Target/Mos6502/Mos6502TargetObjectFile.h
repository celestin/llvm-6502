//===-- Mos6502TargetObjectFile.h - Mos6502 Object Info -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MOS6502_MOS6502TARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_MOS6502_MOS6502TARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {

class MCContext;
class TargetMachine;

class Mos6502ELFTargetObjectFile : public TargetLoweringObjectFileELF {
public:
  Mos6502ELFTargetObjectFile() :
    TargetLoweringObjectFileELF()
  {}

  const MCExpr *
  getTTypeGlobalReference(const GlobalValue *GV, unsigned Encoding,
                          Mangler &Mang, const TargetMachine &TM,
                          MachineModuleInfo *MMI,
                          MCStreamer &Streamer) const override;
};

} // end namespace llvm

#endif
