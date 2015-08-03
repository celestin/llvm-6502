//===-- Mos6502InstPrinter.cpp - Convert Mos6502 MCInst to assembly syntax -----==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an Mos6502 MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "Mos6502InstPrinter.h"
#include "Mos6502.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

// The generated AsmMatcher Mos6502GenAsmWriter uses "Mos6502" as the target
// namespace. But MOS6502 backend uses "M6502" as its namespace.
namespace llvm {
namespace Mos6502 {
  using namespace M6502;
}
}

#define GET_INSTRUCTION_NAME
#define PRINT_ALIAS_INSTR
#include "Mos6502GenAsmWriter.inc"

bool Mos6502InstPrinter::isV9(const MCSubtargetInfo &STI) const {
  return (STI.getFeatureBits()[Mos6502::FeatureV9]) != 0;
}

void Mos6502InstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const
{
  OS << '%' << StringRef(getRegisterName(RegNo)).lower();
}

void Mos6502InstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                 StringRef Annot, const MCSubtargetInfo &STI) {
  if (!printAliasInstr(MI, STI, O) && !printMos6502AliasInstr(MI, STI, O))
    printInstruction(MI, STI, O);
  printAnnotation(O, Annot);
}

bool Mos6502InstPrinter::printMos6502AliasInstr(const MCInst *MI,
                                            const MCSubtargetInfo &STI,
                                            raw_ostream &O) {
  switch (MI->getOpcode()) {
  default: return false;
  case M6502::JMPLrr:
  case M6502::JMPLri: {
    if (MI->getNumOperands() != 3)
      return false;
    if (!MI->getOperand(0).isReg())
      return false;
    switch (MI->getOperand(0).getReg()) {
    default: return false;
    case M6502::G0: // jmp $addr | ret | retl
      if (MI->getOperand(2).isImm() &&
          MI->getOperand(2).getImm() == 8) {
        switch(MI->getOperand(1).getReg()) {
        default: break;
        case M6502::I7: O << "\tret"; return true;
        case M6502::O7: O << "\tretl"; return true;
        }
      }
      O << "\tjmp "; printMemOperand(MI, 1, STI, O);
      return true;
    case M6502::O7: // call $addr
      O << "\tcall "; printMemOperand(MI, 1, STI, O);
      return true;
    }
  }
  case M6502::V9FCMPS:  case M6502::V9FCMPD:  case M6502::V9FCMPQ:
  case M6502::V9FCMPES: case M6502::V9FCMPED: case M6502::V9FCMPEQ: {
    if (isV9(STI)
        || (MI->getNumOperands() != 3)
        || (!MI->getOperand(0).isReg())
        || (MI->getOperand(0).getReg() != M6502::FCC0))
      return false;
    // if V8, skip printing %fcc0.
    switch(MI->getOpcode()) {
    default:
    case M6502::V9FCMPS:  O << "\tfcmps "; break;
    case M6502::V9FCMPD:  O << "\tfcmpd "; break;
    case M6502::V9FCMPQ:  O << "\tfcmpq "; break;
    case M6502::V9FCMPES: O << "\tfcmpes "; break;
    case M6502::V9FCMPED: O << "\tfcmped "; break;
    case M6502::V9FCMPEQ: O << "\tfcmpeq "; break;
    }
    printOperand(MI, 1, STI, O);
    O << ", ";
    printOperand(MI, 2, STI, O);
    return true;
  }
  }
}

void Mos6502InstPrinter::printOperand(const MCInst *MI, int opNum,
                                    const MCSubtargetInfo &STI,
                                    raw_ostream &O) {
  const MCOperand &MO = MI->getOperand (opNum);

  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return ;
  }

  if (MO.isImm()) {
    O << (int)MO.getImm();
    return;
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

void Mos6502InstPrinter::printMemOperand(const MCInst *MI, int opNum,
                                       const MCSubtargetInfo &STI,
                                       raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, STI, O);

  // If this is an ADD operand, emit it like normal operands.
  if (Modifier && !strcmp(Modifier, "arith")) {
    O << ", ";
    printOperand(MI, opNum+1, STI, O);
    return;
  }
  const MCOperand &MO = MI->getOperand(opNum+1);

  if (MO.isReg() && MO.getReg() == M6502::G0)
    return;   // don't print "+%g0"
  if (MO.isImm() && MO.getImm() == 0)
    return;   // don't print "+0"

  O << "+";

  printOperand(MI, opNum+1, STI, O);
}

void Mos6502InstPrinter::printCCOperand(const MCInst *MI, int opNum,
                                      const MCSubtargetInfo &STI,
                                      raw_ostream &O) {
  int CC = (int)MI->getOperand(opNum).getImm();
  switch (MI->getOpcode()) {
  default: break;
  case M6502::FBCOND:
  case M6502::FBCONDA:
  case M6502::BPFCC:
  case M6502::BPFCCA:
  case M6502::BPFCCNT:
  case M6502::BPFCCANT:
  case M6502::MOVFCCrr:  case M6502::V9MOVFCCrr:
  case M6502::MOVFCCri:  case M6502::V9MOVFCCri:
  case M6502::FMOVS_FCC: case M6502::V9FMOVS_FCC:
  case M6502::FMOVD_FCC: case M6502::V9FMOVD_FCC:
  case M6502::FMOVQ_FCC: case M6502::V9FMOVQ_FCC:
    // Make sure CC is a fp conditional flag.
    CC = (CC < 16) ? (CC + 16) : CC;
    break;
  }
  O << MOS6502CondCodeToString((SPCC::CondCodes)CC);
}

bool Mos6502InstPrinter::printGetPCX(const MCInst *MI, unsigned opNum,
                                   const MCSubtargetInfo &STI,
                                   raw_ostream &O) {
  llvm_unreachable("FIXME: Implement Mos6502InstPrinter::printGetPCX.");
  return true;
}
