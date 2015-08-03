//===-- Mos6502AsmBackend.cpp - Mos6502 Assembler Backend ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCAsmBackend.h"
#include "MCTargetDesc/Mos6502FixupKinds.h"
#include "MCTargetDesc/Mos6502MCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

static unsigned adjustFixupValue(unsigned Kind, uint64_t Value) {
  switch (Kind) {
  default:
    llvm_unreachable("Unknown fixup kind!");
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    return Value;

  case Mos6502::fixup_mos6502_wplt30:
  case Mos6502::fixup_mos6502_call30:
    return (Value >> 2) & 0x3fffffff;

  case Mos6502::fixup_mos6502_br22:
    return (Value >> 2) & 0x3fffff;

  case Mos6502::fixup_mos6502_br19:
    return (Value >> 2) & 0x7ffff;

  case Mos6502::fixup_mos6502_br16_2:
    return (Value >> 2) & 0xc000;

  case Mos6502::fixup_mos6502_br16_14:
    return (Value >> 2) & 0x3fff;

  case Mos6502::fixup_mos6502_pc22:
  case Mos6502::fixup_mos6502_got22:
  case Mos6502::fixup_mos6502_tls_gd_hi22:
  case Mos6502::fixup_mos6502_tls_ldm_hi22:
  case Mos6502::fixup_mos6502_tls_ie_hi22:
  case Mos6502::fixup_mos6502_hi22:
    return (Value >> 10) & 0x3fffff;

  case Mos6502::fixup_mos6502_pc10:
  case Mos6502::fixup_mos6502_got10:
  case Mos6502::fixup_mos6502_tls_gd_lo10:
  case Mos6502::fixup_mos6502_tls_ldm_lo10:
  case Mos6502::fixup_mos6502_tls_ie_lo10:
  case Mos6502::fixup_mos6502_lo10:
    return Value & 0x3ff;

  case Mos6502::fixup_mos6502_tls_ldo_hix22:
  case Mos6502::fixup_mos6502_tls_le_hix22:
    return (~Value >> 10) & 0x3fffff;

  case Mos6502::fixup_mos6502_tls_ldo_lox10:
  case Mos6502::fixup_mos6502_tls_le_lox10:
    return (~(~Value & 0x3ff)) & 0x1fff;

  case Mos6502::fixup_mos6502_h44:
    return (Value >> 22) & 0x3fffff;

  case Mos6502::fixup_mos6502_m44:
    return (Value >> 12) & 0x3ff;

  case Mos6502::fixup_mos6502_l44:
    return Value & 0xfff;

  case Mos6502::fixup_mos6502_hh:
    return (Value >> 42) & 0x3fffff;

  case Mos6502::fixup_mos6502_hm:
    return (Value >> 32) & 0x3ff;

  case Mos6502::fixup_mos6502_tls_gd_add:
  case Mos6502::fixup_mos6502_tls_gd_call:
  case Mos6502::fixup_mos6502_tls_ldm_add:
  case Mos6502::fixup_mos6502_tls_ldm_call:
  case Mos6502::fixup_mos6502_tls_ldo_add:
  case Mos6502::fixup_mos6502_tls_ie_ld:
  case Mos6502::fixup_mos6502_tls_ie_ldx:
  case Mos6502::fixup_mos6502_tls_ie_add:
    return 0;
  }
}

namespace {
  class Mos6502AsmBackend : public MCAsmBackend {
  protected:
    const Target &TheTarget;
    bool IsLittleEndian;
    bool Is64Bit;

  public:
    Mos6502AsmBackend(const Target &T)
        : MCAsmBackend(), TheTarget(T),
          IsLittleEndian(true),
          Is64Bit(false) {}

    unsigned getNumFixupKinds() const override {
      return Mos6502::NumTargetFixupKinds;
    }

    const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
      const static MCFixupKindInfo InfosBE[Mos6502::NumTargetFixupKinds] = {
        // name                    offset bits  flags
        { "fixup_mos6502_call30",     2,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br22",      10,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br19",      13,     19,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br16_2",    10,      2,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br16_14",   18,     14,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_hi22",      10,     22,  0 },
        { "fixup_mos6502_lo10",      22,     10,  0 },
        { "fixup_mos6502_h44",       10,     22,  0 },
        { "fixup_mos6502_m44",       22,     10,  0 },
        { "fixup_mos6502_l44",       20,     12,  0 },
        { "fixup_mos6502_hh",        10,     22,  0 },
        { "fixup_mos6502_hm",        22,     10,  0 },
        { "fixup_mos6502_pc22",      10,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_pc10",      22,     10,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_got22",     10,     22,  0 },
        { "fixup_mos6502_got10",     22,     10,  0 },
        { "fixup_mos6502_wplt30",     2,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_tls_gd_hi22",   10, 22,  0 },
        { "fixup_mos6502_tls_gd_lo10",   22, 10,  0 },
        { "fixup_mos6502_tls_gd_add",     0,  0,  0 },
        { "fixup_mos6502_tls_gd_call",    0,  0,  0 },
        { "fixup_mos6502_tls_ldm_hi22",  10, 22,  0 },
        { "fixup_mos6502_tls_ldm_lo10",  22, 10,  0 },
        { "fixup_mos6502_tls_ldm_add",    0,  0,  0 },
        { "fixup_mos6502_tls_ldm_call",   0,  0,  0 },
        { "fixup_mos6502_tls_ldo_hix22", 10, 22,  0 },
        { "fixup_mos6502_tls_ldo_lox10", 22, 10,  0 },
        { "fixup_mos6502_tls_ldo_add",    0,  0,  0 },
        { "fixup_mos6502_tls_ie_hi22",   10, 22,  0 },
        { "fixup_mos6502_tls_ie_lo10",   22, 10,  0 },
        { "fixup_mos6502_tls_ie_ld",      0,  0,  0 },
        { "fixup_mos6502_tls_ie_ldx",     0,  0,  0 },
        { "fixup_mos6502_tls_ie_add",     0,  0,  0 },
        { "fixup_mos6502_tls_le_hix22",   0,  0,  0 },
        { "fixup_mos6502_tls_le_lox10",   0,  0,  0 }
      };

      const static MCFixupKindInfo InfosLE[Mos6502::NumTargetFixupKinds] = {
        // name                    offset bits  flags
        { "fixup_mos6502_call30",     0,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br22",       0,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br19",       0,     19,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br16_2",    20,      2,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_br16_14",    0,     14,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_hi22",       0,     22,  0 },
        { "fixup_mos6502_lo10",       0,     10,  0 },
        { "fixup_mos6502_h44",        0,     22,  0 },
        { "fixup_mos6502_m44",        0,     10,  0 },
        { "fixup_mos6502_l44",        0,     12,  0 },
        { "fixup_mos6502_hh",         0,     22,  0 },
        { "fixup_mos6502_hm",         0,     10,  0 },
        { "fixup_mos6502_pc22",       0,     22,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_pc10",       0,     10,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_got22",      0,     22,  0 },
        { "fixup_mos6502_got10",      0,     10,  0 },
        { "fixup_mos6502_wplt30",      0,     30,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_mos6502_tls_gd_hi22",    0, 22,  0 },
        { "fixup_mos6502_tls_gd_lo10",    0, 10,  0 },
        { "fixup_mos6502_tls_gd_add",     0,  0,  0 },
        { "fixup_mos6502_tls_gd_call",    0,  0,  0 },
        { "fixup_mos6502_tls_ldm_hi22",   0, 22,  0 },
        { "fixup_mos6502_tls_ldm_lo10",   0, 10,  0 },
        { "fixup_mos6502_tls_ldm_add",    0,  0,  0 },
        { "fixup_mos6502_tls_ldm_call",   0,  0,  0 },
        { "fixup_mos6502_tls_ldo_hix22",  0, 22,  0 },
        { "fixup_mos6502_tls_ldo_lox10",  0, 10,  0 },
        { "fixup_mos6502_tls_ldo_add",    0,  0,  0 },
        { "fixup_mos6502_tls_ie_hi22",    0, 22,  0 },
        { "fixup_mos6502_tls_ie_lo10",    0, 10,  0 },
        { "fixup_mos6502_tls_ie_ld",      0,  0,  0 },
        { "fixup_mos6502_tls_ie_ldx",     0,  0,  0 },
        { "fixup_mos6502_tls_ie_add",     0,  0,  0 },
        { "fixup_mos6502_tls_le_hix22",   0,  0,  0 },
        { "fixup_mos6502_tls_le_lox10",   0,  0,  0 }
      };

      if (Kind < FirstTargetFixupKind)
        return MCAsmBackend::getFixupKindInfo(Kind);

      assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
             "Invalid kind!");
      if (IsLittleEndian)
        return InfosLE[Kind - FirstTargetFixupKind];

      return InfosBE[Kind - FirstTargetFixupKind];
    }

    void processFixupValue(const MCAssembler &Asm, const MCAsmLayout &Layout,
                           const MCFixup &Fixup, const MCFragment *DF,
                           const MCValue &Target, uint64_t &Value,
                           bool &IsResolved) override {
      switch ((Mos6502::Fixups)Fixup.getKind()) {
      default: break;
      case Mos6502::fixup_mos6502_wplt30:
        if (Target.getSymA()->getSymbol().isTemporary())
          return;
      case Mos6502::fixup_mos6502_tls_gd_hi22:
      case Mos6502::fixup_mos6502_tls_gd_lo10:
      case Mos6502::fixup_mos6502_tls_gd_add:
      case Mos6502::fixup_mos6502_tls_gd_call:
      case Mos6502::fixup_mos6502_tls_ldm_hi22:
      case Mos6502::fixup_mos6502_tls_ldm_lo10:
      case Mos6502::fixup_mos6502_tls_ldm_add:
      case Mos6502::fixup_mos6502_tls_ldm_call:
      case Mos6502::fixup_mos6502_tls_ldo_hix22:
      case Mos6502::fixup_mos6502_tls_ldo_lox10:
      case Mos6502::fixup_mos6502_tls_ldo_add:
      case Mos6502::fixup_mos6502_tls_ie_hi22:
      case Mos6502::fixup_mos6502_tls_ie_lo10:
      case Mos6502::fixup_mos6502_tls_ie_ld:
      case Mos6502::fixup_mos6502_tls_ie_ldx:
      case Mos6502::fixup_mos6502_tls_ie_add:
      case Mos6502::fixup_mos6502_tls_le_hix22:
      case Mos6502::fixup_mos6502_tls_le_lox10:  IsResolved = false; break;
      }
    }

    bool mayNeedRelaxation(const MCInst &Inst) const override {
      // FIXME.
      return false;
    }

    /// fixupNeedsRelaxation - Target specific predicate for whether a given
    /// fixup requires the associated instruction to be relaxed.
    bool fixupNeedsRelaxation(const MCFixup &Fixup,
                              uint64_t Value,
                              const MCRelaxableFragment *DF,
                              const MCAsmLayout &Layout) const override {
      // FIXME.
      llvm_unreachable("fixupNeedsRelaxation() unimplemented");
      return false;
    }
    void relaxInstruction(const MCInst &Inst, MCInst &Res) const override {
      // FIXME.
      llvm_unreachable("relaxInstruction() unimplemented");
    }

    bool writeNopData(uint64_t Count, MCObjectWriter *OW) const override {
      // Cannot emit NOP with size not multiple of 32 bits.
      if (Count % 4 != 0)
        return false;

      uint64_t NumNops = Count / 4;
      for (uint64_t i = 0; i != NumNops; ++i)
        OW->write32(0x01000000);

      return true;
    }
  };

  class ELFMos6502AsmBackend : public Mos6502AsmBackend {
    Triple::OSType OSType;
  public:
    ELFMos6502AsmBackend(const Target &T, Triple::OSType OSType) :
      Mos6502AsmBackend(T), OSType(OSType) { }

    void applyFixup(const MCFixup &Fixup, char *Data, unsigned DataSize,
                    uint64_t Value, bool IsPCRel) const override {

      Value = adjustFixupValue(Fixup.getKind(), Value);
      if (!Value) return;           // Doesn't change encoding.

      unsigned Offset = Fixup.getOffset();

      // For each byte of the fragment that the fixup touches, mask in the bits
      // from the fixup value. The Value has been "split up" into the
      // appropriate bitfields above.
      for (unsigned i = 0; i != 4; ++i) {
        unsigned Idx = IsLittleEndian ? i : 3 - i;
        Data[Offset + Idx] |= uint8_t((Value >> (i * 8)) & 0xff);
      }
    }

    MCObjectWriter *createObjectWriter(raw_pwrite_stream &OS) const override {
      uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(OSType);
      return createMos6502ELFObjectWriter(OS, Is64Bit, IsLittleEndian, OSABI);
    }
  };

} // end anonymous namespace

MCAsmBackend *llvm::createMos6502AsmBackend(const Target &T,
                                          const MCRegisterInfo &MRI,
                                          const Triple &TT, StringRef CPU) {
  return new ELFMos6502AsmBackend(T, TT.getOS());
}
