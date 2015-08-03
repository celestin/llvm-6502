//===-- Mos6502MCTargetDesc.h - Mos6502 Target Descriptions ---------*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_MOS6502_MCTARGETDESC_MOS6502MCTARGETDESC_H
#define LLVM_LIB_TARGET_MOS6502_MCTARGETDESC_MOS6502MCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class Target;
class Triple;
class StringRef;
class raw_pwrite_stream;
class raw_ostream;

extern Target TheMos6502Target;

MCCodeEmitter *createMos6502MCCodeEmitter(const MCInstrInfo &MCII,
                                        const MCRegisterInfo &MRI,
                                        MCContext &Ctx);
MCAsmBackend *createMos6502AsmBackend(const Target &T, const MCRegisterInfo &MRI,
                                    const Triple &TT, StringRef CPU);
MCObjectWriter *createMos6502ELFObjectWriter(raw_pwrite_stream &OS, bool Is64Bit,
                                           bool IsLIttleEndian, uint8_t OSABI);
} // End llvm namespace

// Defines symbolic names for Mos6502 registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "Mos6502GenRegisterInfo.inc"

// Defines symbolic names for the Mos6502 instructions.
//
#define GET_INSTRINFO_ENUM
#include "Mos6502GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "Mos6502GenSubtargetInfo.inc"

#endif
