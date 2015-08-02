//===-- Mos6502MCAsmInfo.h - Mos6502 asm properties ----------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the Mos6502MCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MOS6502_MCTARGETDESC_MOS6502MCASMINFO_H
#define LLVM_LIB_TARGET_MOS6502_MCTARGETDESC_MOS6502MCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
class Triple;

class Mos6502ELFMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;
public:
  explicit Mos6502ELFMCAsmInfo(const Triple &TheTriple);
  const MCExpr*
  getExprForPersonalitySymbol(const MCSymbol *Sym, unsigned Encoding,
                              MCStreamer &Streamer) const override;
  const MCExpr* getExprForFDESymbol(const MCSymbol *Sym,
                                    unsigned Encoding,
                                    MCStreamer &Streamer) const override;

};

} // namespace llvm

#endif
