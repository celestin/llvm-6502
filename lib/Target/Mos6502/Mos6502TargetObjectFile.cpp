//===------- Mos6502TargetObjectFile.cpp - Mos6502 Object Info Impl -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Mos6502TargetObjectFile.h"
#include "MCTargetDesc/Mos6502MCExpr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Target/TargetLowering.h"

using namespace llvm;

const MCExpr *Mos6502ELFTargetObjectFile::getTTypeGlobalReference(
    const GlobalValue *GV, unsigned Encoding, Mangler &Mang,
    const TargetMachine &TM, MachineModuleInfo *MMI,
    MCStreamer &Streamer) const {

  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MachineModuleInfoELF &ELFMMI = MMI->getObjFileInfo<MachineModuleInfoELF>();

    MCSymbol *SSym = getSymbolWithGlobalValueBase(GV, ".DW.stub", Mang, TM);

    // Add information about the stub reference to ELFMMI so that the stub
    // gets emitted by the asmprinter.
    MachineModuleInfoImpl::StubValueTy &StubSym = ELFMMI.getGVStubEntry(SSym);
    if (!StubSym.getPointer()) {
      MCSymbol *Sym = TM.getSymbol(GV, Mang);
      StubSym = MachineModuleInfoImpl::StubValueTy(Sym, !GV->hasLocalLinkage());
    }

    MCContext &Ctx = getContext();
    return Mos6502MCExpr::create(Mos6502MCExpr::VK_Mos6502_R_DISP32,
                               MCSymbolRefExpr::create(SSym, Ctx), Ctx);
  }

  return TargetLoweringObjectFileELF::getTTypeGlobalReference(
      GV, Encoding, Mang, TM, MMI, Streamer);
}
