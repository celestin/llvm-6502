//====- Mos6502MCExpr.h - Mos6502 specific MC expression classes --*- C++ -*-=====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes Mos6502-specific MCExprs, used for modifiers like
// "%hi" or "%lo" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MOS6502_MCTARGETDESC_MOS6502MCEXPR_H
#define LLVM_LIB_TARGET_MOS6502_MCTARGETDESC_MOS6502MCEXPR_H

#include "Mos6502FixupKinds.h"
#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;
class Mos6502MCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_Mos6502_None,
    VK_Mos6502_LO,
    VK_Mos6502_HI,
    VK_Mos6502_H44,
    VK_Mos6502_M44,
    VK_Mos6502_L44,
    VK_Mos6502_HH,
    VK_Mos6502_HM,
    VK_Mos6502_PC22,
    VK_Mos6502_PC10,
    VK_Mos6502_GOT22,
    VK_Mos6502_GOT10,
    VK_Mos6502_WPLT30,
    VK_Mos6502_R_DISP32,
    VK_Mos6502_TLS_GD_HI22,
    VK_Mos6502_TLS_GD_LO10,
    VK_Mos6502_TLS_GD_ADD,
    VK_Mos6502_TLS_GD_CALL,
    VK_Mos6502_TLS_LDM_HI22,
    VK_Mos6502_TLS_LDM_LO10,
    VK_Mos6502_TLS_LDM_ADD,
    VK_Mos6502_TLS_LDM_CALL,
    VK_Mos6502_TLS_LDO_HIX22,
    VK_Mos6502_TLS_LDO_LOX10,
    VK_Mos6502_TLS_LDO_ADD,
    VK_Mos6502_TLS_IE_HI22,
    VK_Mos6502_TLS_IE_LO10,
    VK_Mos6502_TLS_IE_LD,
    VK_Mos6502_TLS_IE_LDX,
    VK_Mos6502_TLS_IE_ADD,
    VK_Mos6502_TLS_LE_HIX22,
    VK_Mos6502_TLS_LE_LOX10
  };

private:
  const VariantKind Kind;
  const MCExpr *Expr;

  explicit Mos6502MCExpr(VariantKind Kind, const MCExpr *Expr)
      : Kind(Kind), Expr(Expr) {}

public:
  /// @name Construction
  /// @{

  static const Mos6502MCExpr *create(VariantKind Kind, const MCExpr *Expr,
                                 MCContext &Ctx);
  /// @}
  /// @name Accessors
  /// @{

  /// getOpcode - Get the kind of this expression.
  VariantKind getKind() const { return Kind; }

  /// getSubExpr - Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  /// getFixupKind - Get the fixup kind of this expression.
  Mos6502::Fixups getFixupKind() const { return getFixupKind(Kind); }

  /// @}
  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res,
                                 const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCSection *findAssociatedSection() const override {
    return getSubExpr()->findAssociatedSection();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const Mos6502MCExpr *) { return true; }

  static VariantKind parseVariantKind(StringRef name);
  static bool printVariantKind(raw_ostream &OS, VariantKind Kind);
  static Mos6502::Fixups getFixupKind(VariantKind Kind);
};

} // end namespace llvm.

#endif
