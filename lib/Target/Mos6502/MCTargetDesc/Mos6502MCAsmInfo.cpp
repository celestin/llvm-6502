//===-- Mos6502MCAsmInfo.cpp - Mos6502 asm properties -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the Mos6502MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "Mos6502MCAsmInfo.h"
#include "Mos6502MCExpr.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCStreamer.h"

using namespace llvm;

void Mos6502ELFMCAsmInfo::anchor() {}

Mos6502ELFMCAsmInfo::Mos6502ELFMCAsmInfo(const Triple &TheTriple) {
  bool isV9 = false;
  IsLittleEndian = true;

  if (isV9) {
    PointerSize = CalleeSaveStackSlotSize = 8;
  }

  Data16bitsDirective = "\t.half\t";
  Data32bitsDirective = "\t.word\t";
  // .xword is only supported by V9.
  Data64bitsDirective = (isV9) ? "\t.xword\t" : nullptr;
  ZeroDirective = "\t.skip\t";
  CommentString = "!";
  SupportsDebugInformation = true;

  ExceptionsType = ExceptionHandling::DwarfCFI;

  SunStyleELFSectionSwitchSyntax = true;
  UsesELFSectionDirectiveForBSS = true;

  UseIntegratedAssembler = true;
}

const MCExpr*
Mos6502ELFMCAsmInfo::getExprForPersonalitySymbol(const MCSymbol *Sym,
                                               unsigned Encoding,
                                               MCStreamer &Streamer) const {
  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MCContext &Ctx = Streamer.getContext();
    return Mos6502MCExpr::create(Mos6502MCExpr::VK_Mos6502_R_DISP32,
                               MCSymbolRefExpr::create(Sym, Ctx), Ctx);
  }

  return MCAsmInfo::getExprForPersonalitySymbol(Sym, Encoding, Streamer);
}

const MCExpr*
Mos6502ELFMCAsmInfo::getExprForFDESymbol(const MCSymbol *Sym,
                                       unsigned Encoding,
                                       MCStreamer &Streamer) const {
  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MCContext &Ctx = Streamer.getContext();
    return Mos6502MCExpr::create(Mos6502MCExpr::VK_Mos6502_R_DISP32,
                               MCSymbolRefExpr::create(Sym, Ctx), Ctx);
  }
  return MCAsmInfo::getExprForFDESymbol(Sym, Encoding, Streamer);
}
