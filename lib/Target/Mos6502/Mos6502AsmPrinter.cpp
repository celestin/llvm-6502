//===-- Mos6502AsmPrinter.cpp - Mos6502 LLVM assembly writer ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format MOS6502 assembly language.
//
//===----------------------------------------------------------------------===//

#include "Mos6502.h"
#include "InstPrinter/Mos6502InstPrinter.h"
#include "MCTargetDesc/Mos6502MCExpr.h"
#include "Mos6502InstrInfo.h"
#include "Mos6502TargetMachine.h"
#include "Mos6502TargetStreamer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
  class Mos6502AsmPrinter : public AsmPrinter {
    Mos6502TargetStreamer &getTargetStreamer() {
      return static_cast<Mos6502TargetStreamer &>(
          *OutStreamer->getTargetStreamer());
    }
  public:
    explicit Mos6502AsmPrinter(TargetMachine &TM,
                             std::unique_ptr<MCStreamer> Streamer)
        : AsmPrinter(TM, std::move(Streamer)) {}

    const char *getPassName() const override {
      return "Mos6502 Assembly Printer";
    }

    void printOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);
    void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &OS,
                         const char *Modifier = nullptr);
    void printCCOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);

    void EmitFunctionBodyStart() override;
    void EmitInstruction(const MachineInstr *MI) override;

    static const char *getRegisterName(unsigned RegNo) {
      return Mos6502InstPrinter::getRegisterName(RegNo);
    }

    bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                         unsigned AsmVariant, const char *ExtraCode,
                         raw_ostream &O) override;
    bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                               unsigned AsmVariant, const char *ExtraCode,
                               raw_ostream &O) override;

    void LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                   const MCSubtargetInfo &STI);

  };
} // end of anonymous namespace

static MCOperand createMos6502MCOperand(Mos6502MCExpr::VariantKind Kind,
                                      MCSymbol *Sym, MCContext &OutContext) {
  const MCSymbolRefExpr *MCSym = MCSymbolRefExpr::create(Sym,
                                                         OutContext);
  const Mos6502MCExpr *expr = Mos6502MCExpr::create(Kind, MCSym, OutContext);
  return MCOperand::createExpr(expr);

}
static MCOperand createPCXCallOP(MCSymbol *Label,
                                 MCContext &OutContext) {
  return createMos6502MCOperand(Mos6502MCExpr::VK_Mos6502_None, Label, OutContext);
}

static MCOperand createPCXRelExprOp(Mos6502MCExpr::VariantKind Kind,
                                    MCSymbol *GOTLabel, MCSymbol *StartLabel,
                                    MCSymbol *CurLabel,
                                    MCContext &OutContext)
{
  const MCSymbolRefExpr *GOT = MCSymbolRefExpr::create(GOTLabel, OutContext);
  const MCSymbolRefExpr *Start = MCSymbolRefExpr::create(StartLabel,
                                                         OutContext);
  const MCSymbolRefExpr *Cur = MCSymbolRefExpr::create(CurLabel,
                                                       OutContext);

  const MCBinaryExpr *Sub = MCBinaryExpr::createSub(Cur, Start, OutContext);
  const MCBinaryExpr *Add = MCBinaryExpr::createAdd(GOT, Sub, OutContext);
  const Mos6502MCExpr *expr = Mos6502MCExpr::create(Kind,
                                                Add, OutContext);
  return MCOperand::createExpr(expr);
}

static void EmitCall(MCStreamer &OutStreamer,
                     MCOperand &Callee,
                     const MCSubtargetInfo &STI)
{
  MCInst CallInst;
  CallInst.setOpcode(SP::CALL);
  CallInst.addOperand(Callee);
  OutStreamer.EmitInstruction(CallInst, STI);
}

static void EmitSETHI(MCStreamer &OutStreamer,
                      MCOperand &Imm, MCOperand &RD,
                      const MCSubtargetInfo &STI)
{
  MCInst SETHIInst;
  SETHIInst.setOpcode(SP::SETHIi);
  SETHIInst.addOperand(RD);
  SETHIInst.addOperand(Imm);
  OutStreamer.EmitInstruction(SETHIInst, STI);
}

static void EmitBinary(MCStreamer &OutStreamer, unsigned Opcode,
                       MCOperand &RS1, MCOperand &Src2, MCOperand &RD,
                       const MCSubtargetInfo &STI)
{
  MCInst Inst;
  Inst.setOpcode(Opcode);
  Inst.addOperand(RD);
  Inst.addOperand(RS1);
  Inst.addOperand(Src2);
  OutStreamer.EmitInstruction(Inst, STI);
}

static void EmitOR(MCStreamer &OutStreamer,
                   MCOperand &RS1, MCOperand &Imm, MCOperand &RD,
                   const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SP::ORri, RS1, Imm, RD, STI);
}

static void EmitADD(MCStreamer &OutStreamer,
                    MCOperand &RS1, MCOperand &RS2, MCOperand &RD,
                    const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SP::ADDrr, RS1, RS2, RD, STI);
}

static void EmitSHL(MCStreamer &OutStreamer,
                    MCOperand &RS1, MCOperand &Imm, MCOperand &RD,
                    const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SP::SLLri, RS1, Imm, RD, STI);
}


static void EmitHiLo(MCStreamer &OutStreamer,  MCSymbol *GOTSym,
                     Mos6502MCExpr::VariantKind HiKind,
                     Mos6502MCExpr::VariantKind LoKind,
                     MCOperand &RD,
                     MCContext &OutContext,
                     const MCSubtargetInfo &STI) {

  MCOperand hi = createMos6502MCOperand(HiKind, GOTSym, OutContext);
  MCOperand lo = createMos6502MCOperand(LoKind, GOTSym, OutContext);
  EmitSETHI(OutStreamer, hi, RD, STI);
  EmitOR(OutStreamer, RD, lo, RD, STI);
}

void Mos6502AsmPrinter::LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                                const MCSubtargetInfo &STI)
{
  MCSymbol *GOTLabel   =
    OutContext.getOrCreateSymbol(Twine("_GLOBAL_OFFSET_TABLE_"));

  const MachineOperand &MO = MI->getOperand(0);
  assert(MO.getReg() != SP::O7 &&
         "%o7 is assigned as destination for getpcx!");

  MCOperand MCRegOP = MCOperand::createReg(MO.getReg());


  if (TM.getRelocationModel() != Reloc::PIC_) {
    // Just load the address of GOT to MCRegOP.
    switch(TM.getCodeModel()) {
    default:
      llvm_unreachable("Unsupported absolute code model");
    case CodeModel::Small:
      EmitHiLo(*OutStreamer, GOTLabel,
               Mos6502MCExpr::VK_Mos6502_HI, Mos6502MCExpr::VK_Mos6502_LO,
               MCRegOP, OutContext, STI);
      break;
    case CodeModel::Medium: {
      EmitHiLo(*OutStreamer, GOTLabel,
               Mos6502MCExpr::VK_Mos6502_H44, Mos6502MCExpr::VK_Mos6502_M44,
               MCRegOP, OutContext, STI);
      MCOperand imm = MCOperand::createExpr(MCConstantExpr::create(12,
                                                                   OutContext));
      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
      MCOperand lo = createMos6502MCOperand(Mos6502MCExpr::VK_Mos6502_L44,
                                          GOTLabel, OutContext);
      EmitOR(*OutStreamer, MCRegOP, lo, MCRegOP, STI);
      break;
    }
    case CodeModel::Large: {
      EmitHiLo(*OutStreamer, GOTLabel,
               Mos6502MCExpr::VK_Mos6502_HH, Mos6502MCExpr::VK_Mos6502_HM,
               MCRegOP, OutContext, STI);
      MCOperand imm = MCOperand::createExpr(MCConstantExpr::create(32,
                                                                   OutContext));
      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
      // Use register %o7 to load the lower 32 bits.
      MCOperand RegO7 = MCOperand::createReg(SP::O7);
      EmitHiLo(*OutStreamer, GOTLabel,
               Mos6502MCExpr::VK_Mos6502_HI, Mos6502MCExpr::VK_Mos6502_LO,
               RegO7, OutContext, STI);
      EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
    }
    }
    return;
  }

  MCSymbol *StartLabel = OutContext.createTempSymbol();
  MCSymbol *EndLabel   = OutContext.createTempSymbol();
  MCSymbol *SethiLabel = OutContext.createTempSymbol();

  MCOperand RegO7   = MCOperand::createReg(SP::O7);

  // <StartLabel>:
  //   call <EndLabel>
  // <SethiLabel>:
  //     sethi %hi(_GLOBAL_OFFSET_TABLE_+(<SethiLabel>-<StartLabel>)), <MO>
  // <EndLabel>:
  //   or  <MO>, %lo(_GLOBAL_OFFSET_TABLE_+(<EndLabel>-<StartLabel>))), <MO>
  //   add <MO>, %o7, <MO>

  OutStreamer->EmitLabel(StartLabel);
  MCOperand Callee =  createPCXCallOP(EndLabel, OutContext);
  EmitCall(*OutStreamer, Callee, STI);
  OutStreamer->EmitLabel(SethiLabel);
  MCOperand hiImm = createPCXRelExprOp(Mos6502MCExpr::VK_Mos6502_PC22,
                                       GOTLabel, StartLabel, SethiLabel,
                                       OutContext);
  EmitSETHI(*OutStreamer, hiImm, MCRegOP, STI);
  OutStreamer->EmitLabel(EndLabel);
  MCOperand loImm = createPCXRelExprOp(Mos6502MCExpr::VK_Mos6502_PC10,
                                       GOTLabel, StartLabel, EndLabel,
                                       OutContext);
  EmitOR(*OutStreamer, MCRegOP, loImm, MCRegOP, STI);
  EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
}

void Mos6502AsmPrinter::EmitInstruction(const MachineInstr *MI)
{

  switch (MI->getOpcode()) {
  default: break;
  case TargetOpcode::DBG_VALUE:
    // FIXME: Debug Value.
    return;
  case SP::GETPCX:
    LowerGETPCXAndEmitMCInsts(MI, getSubtargetInfo());
    return;
  }
  MachineBasicBlock::const_instr_iterator I = MI;
  MachineBasicBlock::const_instr_iterator E = MI->getParent()->instr_end();
  do {
    MCInst TmpInst;
    LowerMos6502MachineInstrToMCInst(I, TmpInst, *this);
    EmitToStreamer(*OutStreamer, TmpInst);
  } while ((++I != E) && I->isInsideBundle()); // Delay slot check.
}

void Mos6502AsmPrinter::EmitFunctionBodyStart() {
  if (!MF->getSubtarget<Mos6502Subtarget>().is64Bit())
    return;

  const MachineRegisterInfo &MRI = MF->getRegInfo();
  const unsigned globalRegs[] = { SP::G2, SP::G3, SP::G6, SP::G7, 0 };
  for (unsigned i = 0; globalRegs[i] != 0; ++i) {
    unsigned reg = globalRegs[i];
    if (MRI.use_empty(reg))
      continue;

    if  (reg == SP::G6 || reg == SP::G7)
      getTargetStreamer().emitMos6502RegisterIgnore(reg);
    else
      getTargetStreamer().emitMos6502RegisterScratch(reg);
  }
}

void Mos6502AsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                   raw_ostream &O) {
  const DataLayout &DL = getDataLayout();
  const MachineOperand &MO = MI->getOperand (opNum);
  Mos6502MCExpr::VariantKind TF = (Mos6502MCExpr::VariantKind) MO.getTargetFlags();

#ifndef NDEBUG
  // Verify the target flags.
  if (MO.isGlobal() || MO.isSymbol() || MO.isCPI()) {
    if (MI->getOpcode() == SP::CALL)
      assert(TF == Mos6502MCExpr::VK_Mos6502_None &&
             "Cannot handle target flags on call address");
    else if (MI->getOpcode() == SP::SETHIi || MI->getOpcode() == SP::SETHIXi)
      assert((TF == Mos6502MCExpr::VK_Mos6502_HI
              || TF == Mos6502MCExpr::VK_Mos6502_H44
              || TF == Mos6502MCExpr::VK_Mos6502_HH
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_GD_HI22
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LDM_HI22
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LDO_HIX22
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_IE_HI22
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LE_HIX22) &&
             "Invalid target flags for address operand on sethi");
    else if (MI->getOpcode() == SP::TLS_CALL)
      assert((TF == Mos6502MCExpr::VK_Mos6502_None
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_GD_CALL
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LDM_CALL) &&
             "Cannot handle target flags on tls call address");
    else if (MI->getOpcode() == SP::TLS_ADDrr)
      assert((TF == Mos6502MCExpr::VK_Mos6502_TLS_GD_ADD
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LDM_ADD
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LDO_ADD
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_IE_ADD) &&
             "Cannot handle target flags on add for TLS");
    else if (MI->getOpcode() == SP::TLS_LDrr)
      assert(TF == Mos6502MCExpr::VK_Mos6502_TLS_IE_LD &&
             "Cannot handle target flags on ld for TLS");
    else if (MI->getOpcode() == SP::TLS_LDXrr)
      assert(TF == Mos6502MCExpr::VK_Mos6502_TLS_IE_LDX &&
             "Cannot handle target flags on ldx for TLS");
    else if (MI->getOpcode() == SP::XORri || MI->getOpcode() == SP::XORXri)
      assert((TF == Mos6502MCExpr::VK_Mos6502_TLS_LDO_LOX10
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LE_LOX10) &&
             "Cannot handle target flags on xor for TLS");
    else
      assert((TF == Mos6502MCExpr::VK_Mos6502_LO
              || TF == Mos6502MCExpr::VK_Mos6502_M44
              || TF == Mos6502MCExpr::VK_Mos6502_L44
              || TF == Mos6502MCExpr::VK_Mos6502_HM
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_GD_LO10
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_LDM_LO10
              || TF == Mos6502MCExpr::VK_Mos6502_TLS_IE_LO10 ) &&
             "Invalid target flags for small address operand");
  }
#endif


  bool CloseParen = Mos6502MCExpr::printVariantKind(O, TF);

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << "%" << StringRef(getRegisterName(MO.getReg())).lower();
    break;

  case MachineOperand::MO_Immediate:
    O << (int)MO.getImm();
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MO.getMBB()->getSymbol()->print(O, MAI);
    return;
  case MachineOperand::MO_GlobalAddress:
    getSymbol(MO.getGlobal())->print(O, MAI);
    break;
  case MachineOperand::MO_BlockAddress:
    O <<  GetBlockAddressSymbol(MO.getBlockAddress())->getName();
    break;
  case MachineOperand::MO_ExternalSymbol:
    O << MO.getSymbolName();
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    O << DL.getPrivateGlobalPrefix() << "CPI" << getFunctionNumber() << "_"
      << MO.getIndex();
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }
  if (CloseParen) O << ")";
}

void Mos6502AsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                      raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, O);

  // If this is an ADD operand, emit it like normal operands.
  if (Modifier && !strcmp(Modifier, "arith")) {
    O << ", ";
    printOperand(MI, opNum+1, O);
    return;
  }

  if (MI->getOperand(opNum+1).isReg() &&
      MI->getOperand(opNum+1).getReg() == SP::G0)
    return;   // don't print "+%g0"
  if (MI->getOperand(opNum+1).isImm() &&
      MI->getOperand(opNum+1).getImm() == 0)
    return;   // don't print "+0"

  O << "+";
  printOperand(MI, opNum+1, O);
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool Mos6502AsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                      unsigned AsmVariant,
                                      const char *ExtraCode,
                                      raw_ostream &O) {
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0) return true; // Unknown modifier.

    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI, OpNo, AsmVariant, ExtraCode, O);
    case 'r':
     break;
    }
  }

  printOperand(MI, OpNo, O);

  return false;
}

bool Mos6502AsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                            unsigned OpNo, unsigned AsmVariant,
                                            const char *ExtraCode,
                                            raw_ostream &O) {
  if (ExtraCode && ExtraCode[0])
    return true;  // Unknown modifier

  O << '[';
  printMemOperand(MI, OpNo, O);
  O << ']';

  return false;
}

// Force static initialization.
extern "C" void LLVMInitializeMos6502AsmPrinter() {
  RegisterAsmPrinter<Mos6502AsmPrinter> X(TheMos6502Target);
  RegisterAsmPrinter<Mos6502AsmPrinter> Y(TheMos6502V9Target);
  RegisterAsmPrinter<Mos6502AsmPrinter> Z(TheMos6502elTarget);
}
