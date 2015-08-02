//===-- Mos6502TargetStreamer.cpp - Mos6502 Target Streamer Methods -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Mos6502 specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "Mos6502TargetStreamer.h"
#include "InstPrinter/Mos6502InstPrinter.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

// pin vtable to this file
Mos6502TargetStreamer::Mos6502TargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

void Mos6502TargetStreamer::anchor() {}

Mos6502TargetAsmStreamer::Mos6502TargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : Mos6502TargetStreamer(S), OS(OS) {}

void Mos6502TargetAsmStreamer::emitMos6502RegisterIgnore(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(Mos6502InstPrinter::getRegisterName(reg)).lower()
     << ", #ignore\n";
}

void Mos6502TargetAsmStreamer::emitMos6502RegisterScratch(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(Mos6502InstPrinter::getRegisterName(reg)).lower()
     << ", #scratch\n";
}

Mos6502TargetELFStreamer::Mos6502TargetELFStreamer(MCStreamer &S)
    : Mos6502TargetStreamer(S) {}

MCELFStreamer &Mos6502TargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}
