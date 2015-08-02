//===-- Mos6502TargetStreamer.h - Mos6502 Target Streamer ----------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MOS6502_MOS6502TARGETSTREAMER_H
#define LLVM_LIB_TARGET_MOS6502_MOS6502TARGETSTREAMER_H

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {
class Mos6502TargetStreamer : public MCTargetStreamer {
  virtual void anchor();

public:
  Mos6502TargetStreamer(MCStreamer &S);
  /// Emit ".register <reg>, #ignore".
  virtual void emitMos6502RegisterIgnore(unsigned reg) = 0;
  /// Emit ".register <reg>, #scratch".
  virtual void emitMos6502RegisterScratch(unsigned reg) = 0;
};

// This part is for ascii assembly output
class Mos6502TargetAsmStreamer : public Mos6502TargetStreamer {
  formatted_raw_ostream &OS;

public:
  Mos6502TargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
  void emitMos6502RegisterIgnore(unsigned reg) override;
  void emitMos6502RegisterScratch(unsigned reg) override;

};

// This part is for ELF object output
class Mos6502TargetELFStreamer : public Mos6502TargetStreamer {
public:
  Mos6502TargetELFStreamer(MCStreamer &S);
  MCELFStreamer &getStreamer();
  void emitMos6502RegisterIgnore(unsigned reg) override {}
  void emitMos6502RegisterScratch(unsigned reg) override {}
};
} // end namespace llvm

#endif
