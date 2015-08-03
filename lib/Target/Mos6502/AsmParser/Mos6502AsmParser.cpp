//===-- Mos6502AsmParser.cpp - Parse Mos6502 assembly to MCInst instructions --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/Mos6502MCTargetDesc.h"
#include "MCTargetDesc/Mos6502MCExpr.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCTargetAsmParser.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

// The generated AsmMatcher Mos6502GenAsmMatcher uses "Mos6502" as the target
// namespace. But MOS6502 backend uses "SP" as its namespace.
namespace llvm {
  namespace Mos6502 {
    using namespace SP;
  }
}

namespace {
class Mos6502Operand;
class Mos6502AsmParser : public MCTargetAsmParser {

  MCSubtargetInfo &STI;
  MCAsmParser &Parser;

  /// @name Auto-generated Match Functions
  /// {

#define GET_ASSEMBLER_HEADER
#include "Mos6502GenAsmMatcher.inc"

  /// }

  // public interface of the MCTargetAsmParser.
  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;
  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;
  bool ParseDirective(AsmToken DirectiveID) override;

  unsigned validateTargetOperandClass(MCParsedAsmOperand &Op,
                                      unsigned Kind) override;

  // Custom parse functions for Mos6502 specific operands.
  OperandMatchResultTy parseMEMOperand(OperandVector &Operands);

  OperandMatchResultTy parseOperand(OperandVector &Operands, StringRef Name);

  OperandMatchResultTy
  parseMos6502AsmOperand(std::unique_ptr<Mos6502Operand> &Operand,
                       bool isCall = false);

  OperandMatchResultTy parseBranchModifiers(OperandVector &Operands);

  // returns true if Tok is matched to a register and returns register in RegNo.
  bool matchRegisterName(const AsmToken &Tok, unsigned &RegNo,
                         unsigned &RegKind);

  bool matchMos6502AsmModifiers(const MCExpr *&EVal, SMLoc &EndLoc);
  bool parseDirectiveWord(unsigned Size, SMLoc L);

  bool is64Bit() const {
    return false;
  }

  void expandSET(MCInst &Inst, SMLoc IDLoc,
                 SmallVectorImpl<MCInst> &Instructions);

public:
  Mos6502AsmParser(MCSubtargetInfo &sti, MCAsmParser &parser,
                const MCInstrInfo &MII,
                const MCTargetOptions &Options)
      : MCTargetAsmParser(Options), STI(sti), Parser(parser) {
    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));
  }

};

  static unsigned IntRegs[32] = {
    Mos6502::G0, Mos6502::G1, Mos6502::G2, Mos6502::G3,
    Mos6502::G4, Mos6502::G5, Mos6502::G6, Mos6502::G7,
    Mos6502::O0, Mos6502::O1, Mos6502::O2, Mos6502::O3,
    Mos6502::O4, Mos6502::O5, Mos6502::O6, Mos6502::O7,
    Mos6502::L0, Mos6502::L1, Mos6502::L2, Mos6502::L3,
    Mos6502::L4, Mos6502::L5, Mos6502::L6, Mos6502::L7,
    Mos6502::I0, Mos6502::I1, Mos6502::I2, Mos6502::I3,
    Mos6502::I4, Mos6502::I5, Mos6502::I6, Mos6502::I7 };

  static unsigned FloatRegs[32] = {
    Mos6502::F0,  Mos6502::F1,  Mos6502::F2,  Mos6502::F3,
    Mos6502::F4,  Mos6502::F5,  Mos6502::F6,  Mos6502::F7,
    Mos6502::F8,  Mos6502::F9,  Mos6502::F10, Mos6502::F11,
    Mos6502::F12, Mos6502::F13, Mos6502::F14, Mos6502::F15,
    Mos6502::F16, Mos6502::F17, Mos6502::F18, Mos6502::F19,
    Mos6502::F20, Mos6502::F21, Mos6502::F22, Mos6502::F23,
    Mos6502::F24, Mos6502::F25, Mos6502::F26, Mos6502::F27,
    Mos6502::F28, Mos6502::F29, Mos6502::F30, Mos6502::F31 };

  static unsigned DoubleRegs[32] = {
    Mos6502::D0,  Mos6502::D1,  Mos6502::D2,  Mos6502::D3,
    Mos6502::D4,  Mos6502::D5,  Mos6502::D6,  Mos6502::D7,
    Mos6502::D8,  Mos6502::D7,  Mos6502::D8,  Mos6502::D9,
    Mos6502::D12, Mos6502::D13, Mos6502::D14, Mos6502::D15,
    Mos6502::D16, Mos6502::D17, Mos6502::D18, Mos6502::D19,
    Mos6502::D20, Mos6502::D21, Mos6502::D22, Mos6502::D23,
    Mos6502::D24, Mos6502::D25, Mos6502::D26, Mos6502::D27,
    Mos6502::D28, Mos6502::D29, Mos6502::D30, Mos6502::D31 };

  static unsigned QuadFPRegs[32] = {
    Mos6502::Q0,  Mos6502::Q1,  Mos6502::Q2,  Mos6502::Q3,
    Mos6502::Q4,  Mos6502::Q5,  Mos6502::Q6,  Mos6502::Q7,
    Mos6502::Q8,  Mos6502::Q9,  Mos6502::Q10, Mos6502::Q11,
    Mos6502::Q12, Mos6502::Q13, Mos6502::Q14, Mos6502::Q15 };

  static unsigned ASRRegs[32] = {
    SP::Y,     SP::ASR1,  SP::ASR2,  SP::ASR3,
    SP::ASR4,  SP::ASR5,  SP::ASR6, SP::ASR7,
    SP::ASR8,  SP::ASR9,  SP::ASR10, SP::ASR11,
    SP::ASR12, SP::ASR13, SP::ASR14, SP::ASR15,
    SP::ASR16, SP::ASR17, SP::ASR18, SP::ASR19,
    SP::ASR20, SP::ASR21, SP::ASR22, SP::ASR23,
    SP::ASR24, SP::ASR25, SP::ASR26, SP::ASR27,
    SP::ASR28, SP::ASR29, SP::ASR30, SP::ASR31};

/// Mos6502Operand - Instances of this class represent a parsed Mos6502 machine
/// instruction.
class Mos6502Operand : public MCParsedAsmOperand {
public:
  enum RegisterKind {
    rk_None,
    rk_IntReg,
    rk_FloatReg,
    rk_DoubleReg,
    rk_QuadReg,
    rk_Special,
  };

private:
  enum KindTy {
    k_Token,
    k_Register,
    k_Immediate,
    k_MemoryReg,
    k_MemoryImm
  } Kind;

  SMLoc StartLoc, EndLoc;

  struct Token {
    const char *Data;
    unsigned Length;
  };

  struct RegOp {
    unsigned RegNum;
    RegisterKind Kind;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  struct MemOp {
    unsigned Base;
    unsigned OffsetReg;
    const MCExpr *Off;
  };

  union {
    struct Token Tok;
    struct RegOp Reg;
    struct ImmOp Imm;
    struct MemOp Mem;
  };
public:
  Mos6502Operand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

  bool isToken() const override { return Kind == k_Token; }
  bool isReg() const override { return Kind == k_Register; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isMem() const override { return isMEMrr() || isMEMri(); }
  bool isMEMrr() const { return Kind == k_MemoryReg; }
  bool isMEMri() const { return Kind == k_MemoryImm; }

  bool isFloatReg() const {
    return (Kind == k_Register && Reg.Kind == rk_FloatReg);
  }

  bool isFloatOrDoubleReg() const {
    return (Kind == k_Register && (Reg.Kind == rk_FloatReg
                                   || Reg.Kind == rk_DoubleReg));
  }


  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    assert((Kind == k_Register) && "Invalid access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate) && "Invalid access!");
    return Imm.Val;
  }

  unsigned getMemBase() const {
    assert((Kind == k_MemoryReg || Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Base;
  }

  unsigned getMemOffsetReg() const {
    assert((Kind == k_MemoryReg) && "Invalid access!");
    return Mem.OffsetReg;
  }

  const MCExpr *getMemOff() const {
    assert((Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Off;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override {
    return StartLoc;
  }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override {
    return EndLoc;
  }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case k_Token:     OS << "Token: " << getToken() << "\n"; break;
    case k_Register:  OS << "Reg: #" << getReg() << "\n"; break;
    case k_Immediate: OS << "Imm: " << getImm() << "\n"; break;
    case k_MemoryReg: OS << "Mem: " << getMemBase() << "+"
                         << getMemOffsetReg() << "\n"; break;
    case k_MemoryImm: assert(getMemOff() != nullptr);
      OS << "Mem: " << getMemBase()
         << "+" << *getMemOff()
         << "\n"; break;
    }
  }

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const{
    // Add as immediate when possible.  Null MCExpr = 0.
    if (!Expr)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addMEMrrOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    assert(getMemOffsetReg() != 0 && "Invalid offset");
    Inst.addOperand(MCOperand::createReg(getMemOffsetReg()));
  }

  void addMEMriOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst, Expr);
  }

  static std::unique_ptr<Mos6502Operand> CreateToken(StringRef Str, SMLoc S) {
    auto Op = make_unique<Mos6502Operand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<Mos6502Operand> CreateReg(unsigned RegNum, unsigned Kind,
                                                 SMLoc S, SMLoc E) {
    auto Op = make_unique<Mos6502Operand>(k_Register);
    Op->Reg.RegNum = RegNum;
    Op->Reg.Kind   = (Mos6502Operand::RegisterKind)Kind;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<Mos6502Operand> CreateImm(const MCExpr *Val, SMLoc S,
                                                 SMLoc E) {
    auto Op = make_unique<Mos6502Operand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static bool MorphToDoubleReg(Mos6502Operand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_FloatReg);
    unsigned regIdx = Reg - Mos6502::F0;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = DoubleRegs[regIdx / 2];
    Op.Reg.Kind = rk_DoubleReg;
    return true;
  }

  static bool MorphToQuadReg(Mos6502Operand &Op) {
    unsigned Reg = Op.getReg();
    unsigned regIdx = 0;
    switch (Op.Reg.Kind) {
    default: llvm_unreachable("Unexpected register kind!");
    case rk_FloatReg:
      regIdx = Reg - Mos6502::F0;
      if (regIdx % 4 || regIdx > 31)
        return false;
      Reg = QuadFPRegs[regIdx / 4];
      break;
    case rk_DoubleReg:
      regIdx =  Reg - Mos6502::D0;
      if (regIdx % 2 || regIdx > 31)
        return false;
      Reg = QuadFPRegs[regIdx / 2];
      break;
    }
    Op.Reg.RegNum = Reg;
    Op.Reg.Kind = rk_QuadReg;
    return true;
  }

  static std::unique_ptr<Mos6502Operand>
  MorphToMEMrr(unsigned Base, std::unique_ptr<Mos6502Operand> Op) {
    unsigned offsetReg = Op->getReg();
    Op->Kind = k_MemoryReg;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = offsetReg;
    Op->Mem.Off = nullptr;
    return Op;
  }

  static std::unique_ptr<Mos6502Operand>
  CreateMEMr(unsigned Base, SMLoc S, SMLoc E) {
    auto Op = make_unique<Mos6502Operand>(k_MemoryReg);
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = Mos6502::G0;  // always 0
    Op->Mem.Off = nullptr;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<Mos6502Operand>
  MorphToMEMri(unsigned Base, std::unique_ptr<Mos6502Operand> Op) {
    const MCExpr *Imm  = Op->getImm();
    Op->Kind = k_MemoryImm;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = 0;
    Op->Mem.Off = Imm;
    return Op;
  }
};

} // end namespace

void Mos6502AsmParser::expandSET(MCInst &Inst, SMLoc IDLoc,
                               SmallVectorImpl<MCInst> &Instructions) {
  MCOperand MCRegOp = Inst.getOperand(0);
  MCOperand MCValOp = Inst.getOperand(1);
  assert(MCRegOp.isReg());
  assert(MCValOp.isImm() || MCValOp.isExpr());

  // the imm operand can be either an expression or an immediate.
  bool IsImm = Inst.getOperand(1).isImm();
  uint64_t ImmValue = IsImm ? MCValOp.getImm() : 0;
  const MCExpr *ValExpr;
  if (IsImm)
    ValExpr = MCConstantExpr::create(ImmValue, getContext());
  else
    ValExpr = MCValOp.getExpr();

  MCOperand PrevReg = MCOperand::createReg(Mos6502::G0);

  if (!IsImm || (ImmValue & ~0x1fff)) {
    MCInst TmpInst;
    const MCExpr *Expr =
        Mos6502MCExpr::create(Mos6502MCExpr::VK_Mos6502_HI, ValExpr, getContext());
    TmpInst.setLoc(IDLoc);
    TmpInst.setOpcode(SP::SETHIi);
    TmpInst.addOperand(MCRegOp);
    TmpInst.addOperand(MCOperand::createExpr(Expr));
    Instructions.push_back(TmpInst);
    PrevReg = MCRegOp;
  }

  if (!IsImm || ((ImmValue & 0x1fff) != 0 || ImmValue == 0)) {
    MCInst TmpInst;
    const MCExpr *Expr =
        Mos6502MCExpr::create(Mos6502MCExpr::VK_Mos6502_LO, ValExpr, getContext());
    TmpInst.setLoc(IDLoc);
    TmpInst.setOpcode(SP::ORri);
    TmpInst.addOperand(MCRegOp);
    TmpInst.addOperand(PrevReg);
    TmpInst.addOperand(MCOperand::createExpr(Expr));
    Instructions.push_back(TmpInst);
  }
}

bool Mos6502AsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                             OperandVector &Operands,
                                             MCStreamer &Out,
                                             uint64_t &ErrorInfo,
                                             bool MatchingInlineAsm) {
  MCInst Inst;
  SmallVector<MCInst, 8> Instructions;
  unsigned MatchResult = MatchInstructionImpl(Operands, Inst, ErrorInfo,
                                              MatchingInlineAsm);
  switch (MatchResult) {
  case Match_Success: {
    switch (Inst.getOpcode()) {
    default:
      Inst.setLoc(IDLoc);
      Instructions.push_back(Inst);
      break;
    case SP::SET:
      expandSET(Inst, IDLoc, Instructions);
      break;
    }

    for (const MCInst &I : Instructions) {
      Out.EmitInstruction(I, STI);
    }
    return false;
  }

  case Match_MissingFeature:
    return Error(IDLoc,
                 "instruction requires a CPU feature not currently enabled");

  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((Mos6502Operand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction");
  }
  case Match_MnemonicFail:
    return Error(IDLoc, "invalid instruction mnemonic");
  }
  llvm_unreachable("Implement any new match types added!");
}

bool Mos6502AsmParser::
ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc)
{
  const AsmToken &Tok = Parser.getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;
  if (getLexer().getKind() != AsmToken::Percent)
    return false;
  Parser.Lex();
  unsigned regKind = Mos6502Operand::rk_None;
  if (matchRegisterName(Tok, RegNo, regKind)) {
    Parser.Lex();
    return false;
  }

  return Error(StartLoc, "invalid register name");
}

static void applyMnemonicAliases(StringRef &Mnemonic, uint64_t Features,
                                 unsigned VariantID);

bool Mos6502AsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                      StringRef Name, SMLoc NameLoc,
                                      OperandVector &Operands) {

  // First operand in MCInst is instruction mnemonic.
  Operands.push_back(Mos6502Operand::CreateToken(Name, NameLoc));

  // apply mnemonic aliases, if any, so that we can parse operands correctly.
  applyMnemonicAliases(Name, getAvailableFeatures(), 0);

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (getLexer().is(AsmToken::Comma)) {
      if (parseBranchModifiers(Operands) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        Parser.eatToEndOfStatement();
        return Error(Loc, "unexpected token");
      }
    }
    if (parseOperand(Operands, Name) != MatchOperand_Success) {
      SMLoc Loc = getLexer().getLoc();
      Parser.eatToEndOfStatement();
      return Error(Loc, "unexpected token");
    }

    while (getLexer().is(AsmToken::Comma)) {
      Parser.Lex(); // Eat the comma.
      // Parse and remember the operand.
      if (parseOperand(Operands, Name) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        Parser.eatToEndOfStatement();
        return Error(Loc, "unexpected token");
      }
    }
  }
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    Parser.eatToEndOfStatement();
    return Error(Loc, "unexpected token");
  }
  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

bool Mos6502AsmParser::
ParseDirective(AsmToken DirectiveID)
{
  StringRef IDVal = DirectiveID.getString();

  if (IDVal == ".byte")
    return parseDirectiveWord(1, DirectiveID.getLoc());

  if (IDVal == ".half")
    return parseDirectiveWord(2, DirectiveID.getLoc());

  if (IDVal == ".word")
    return parseDirectiveWord(4, DirectiveID.getLoc());

  if (IDVal == ".nword")
    return parseDirectiveWord(is64Bit() ? 8 : 4, DirectiveID.getLoc());

  if (is64Bit() && IDVal == ".xword")
    return parseDirectiveWord(8, DirectiveID.getLoc());

  if (IDVal == ".register") {
    // For now, ignore .register directive.
    Parser.eatToEndOfStatement();
    return false;
  }

  // Let the MC layer to handle other directives.
  return true;
}

bool Mos6502AsmParser:: parseDirectiveWord(unsigned Size, SMLoc L) {
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    for (;;) {
      const MCExpr *Value;
      if (getParser().parseExpression(Value))
        return true;

      getParser().getStreamer().EmitValue(Value, Size);

      if (getLexer().is(AsmToken::EndOfStatement))
        break;

      // FIXME: Improve diagnostic.
      if (getLexer().isNot(AsmToken::Comma))
        return Error(L, "unexpected token in directive");
      Parser.Lex();
    }
  }
  Parser.Lex();
  return false;
}

Mos6502AsmParser::OperandMatchResultTy
Mos6502AsmParser::parseMEMOperand(OperandVector &Operands) {

  SMLoc S, E;
  unsigned BaseReg = 0;

  if (ParseRegister(BaseReg, S, E)) {
    return MatchOperand_NoMatch;
  }

  switch (getLexer().getKind()) {
  default: return MatchOperand_NoMatch;

  case AsmToken::Comma:
  case AsmToken::RBrac:
  case AsmToken::EndOfStatement:
    Operands.push_back(Mos6502Operand::CreateMEMr(BaseReg, S, E));
    return MatchOperand_Success;

  case AsmToken:: Plus:
    Parser.Lex(); // Eat the '+'
    break;
  case AsmToken::Minus:
    break;
  }

  std::unique_ptr<Mos6502Operand> Offset;
  OperandMatchResultTy ResTy = parseMos6502AsmOperand(Offset);
  if (ResTy != MatchOperand_Success || !Offset)
    return MatchOperand_NoMatch;

  Operands.push_back(
      Offset->isImm() ? Mos6502Operand::MorphToMEMri(BaseReg, std::move(Offset))
                      : Mos6502Operand::MorphToMEMrr(BaseReg, std::move(Offset)));

  return MatchOperand_Success;
}

Mos6502AsmParser::OperandMatchResultTy
Mos6502AsmParser::parseOperand(OperandVector &Operands, StringRef Mnemonic) {

  OperandMatchResultTy ResTy = MatchOperandParserImpl(Operands, Mnemonic);

  // If there wasn't a custom match, try the generic matcher below. Otherwise,
  // there was a match, but an error occurred, in which case, just return that
  // the operand parsing failed.
  if (ResTy == MatchOperand_Success || ResTy == MatchOperand_ParseFail)
    return ResTy;

  if (getLexer().is(AsmToken::LBrac)) {
    // Memory operand
    Operands.push_back(Mos6502Operand::CreateToken("[",
                                                 Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the [

    if (Mnemonic == "cas" || Mnemonic == "casx") {
      SMLoc S = Parser.getTok().getLoc();
      if (getLexer().getKind() != AsmToken::Percent)
        return MatchOperand_NoMatch;
      Parser.Lex(); // eat %

      unsigned RegNo, RegKind;
      if (!matchRegisterName(Parser.getTok(), RegNo, RegKind))
        return MatchOperand_NoMatch;

      Parser.Lex(); // Eat the identifier token.
      SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer()-1);
      Operands.push_back(Mos6502Operand::CreateReg(RegNo, RegKind, S, E));
      ResTy = MatchOperand_Success;
    } else {
      ResTy = parseMEMOperand(Operands);
    }

    if (ResTy != MatchOperand_Success)
      return ResTy;

    if (!getLexer().is(AsmToken::RBrac))
      return MatchOperand_ParseFail;

    Operands.push_back(Mos6502Operand::CreateToken("]",
                                                 Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the ]

    // Parse an optional address-space identifier after the address.
    if (getLexer().is(AsmToken::Integer)) {
      std::unique_ptr<Mos6502Operand> Op;
      ResTy = parseMos6502AsmOperand(Op, false);
      if (ResTy != MatchOperand_Success || !Op)
        return MatchOperand_ParseFail;
      Operands.push_back(std::move(Op));
    }
    return MatchOperand_Success;
  }

  std::unique_ptr<Mos6502Operand> Op;

  ResTy = parseMos6502AsmOperand(Op, (Mnemonic == "call"));
  if (ResTy != MatchOperand_Success || !Op)
    return MatchOperand_ParseFail;

  // Push the parsed operand into the list of operands
  Operands.push_back(std::move(Op));

  return MatchOperand_Success;
}

Mos6502AsmParser::OperandMatchResultTy
Mos6502AsmParser::parseMos6502AsmOperand(std::unique_ptr<Mos6502Operand> &Op,
                                     bool isCall) {

  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  const MCExpr *EVal;

  Op = nullptr;
  switch (getLexer().getKind()) {
  default:  break;

  case AsmToken::Percent:
    Parser.Lex(); // Eat the '%'.
    unsigned RegNo;
    unsigned RegKind;
    if (matchRegisterName(Parser.getTok(), RegNo, RegKind)) {
      StringRef name = Parser.getTok().getString();
      Parser.Lex(); // Eat the identifier token.
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      switch (RegNo) {
      default:
        Op = Mos6502Operand::CreateReg(RegNo, RegKind, S, E);
        break;
      case Mos6502::PSR:
        Op = Mos6502Operand::CreateToken("%psr", S);
        break;
      case Mos6502::WIM:
        Op = Mos6502Operand::CreateToken("%wim", S);
        break;
      case Mos6502::TBR:
        Op = Mos6502Operand::CreateToken("%tbr", S);
        break;
      case Mos6502::ICC:
        if (name == "xcc")
          Op = Mos6502Operand::CreateToken("%xcc", S);
        else
          Op = Mos6502Operand::CreateToken("%icc", S);
        break;
      }
      break;
    }
    if (matchMos6502AsmModifiers(EVal, E)) {
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      Op = Mos6502Operand::CreateImm(EVal, S, E);
    }
    break;

  case AsmToken::Minus:
  case AsmToken::Integer:
  case AsmToken::LParen:
    if (!getParser().parseExpression(EVal, E))
      Op = Mos6502Operand::CreateImm(EVal, S, E);
    break;

  case AsmToken::Identifier: {
    StringRef Identifier;
    if (!getParser().parseIdentifier(Identifier)) {
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      MCSymbol *Sym = getContext().getOrCreateSymbol(Identifier);

      const MCExpr *Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None,
                                                  getContext());
      if (isCall &&
          getContext().getObjectFileInfo()->getRelocM() == Reloc::PIC_)
        Res = Mos6502MCExpr::create(Mos6502MCExpr::VK_Mos6502_WPLT30, Res,
                                  getContext());
      Op = Mos6502Operand::CreateImm(Res, S, E);
    }
    break;
  }
  }
  return (Op) ? MatchOperand_Success : MatchOperand_ParseFail;
}

Mos6502AsmParser::OperandMatchResultTy
Mos6502AsmParser::parseBranchModifiers(OperandVector &Operands) {

  // parse (,a|,pn|,pt)+

  while (getLexer().is(AsmToken::Comma)) {

    Parser.Lex(); // Eat the comma

    if (!getLexer().is(AsmToken::Identifier))
      return MatchOperand_ParseFail;
    StringRef modName = Parser.getTok().getString();
    if (modName == "a" || modName == "pn" || modName == "pt") {
      Operands.push_back(Mos6502Operand::CreateToken(modName,
                                                   Parser.getTok().getLoc()));
      Parser.Lex(); // eat the identifier.
    }
  }
  return MatchOperand_Success;
}

bool Mos6502AsmParser::matchRegisterName(const AsmToken &Tok,
                                       unsigned &RegNo,
                                       unsigned &RegKind)
{
  int64_t intVal = 0;
  RegNo = 0;
  RegKind = Mos6502Operand::rk_None;
  if (Tok.is(AsmToken::Identifier)) {
    StringRef name = Tok.getString();

    // %fp
    if (name.equals("fp")) {
      RegNo = Mos6502::I6;
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }
    // %sp
    if (name.equals("sp")) {
      RegNo = Mos6502::O6;
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }

    if (name.equals("y")) {
      RegNo = Mos6502::Y;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    if (name.substr(0, 3).equals_lower("asr")
        && !name.substr(3).getAsInteger(10, intVal)
        && intVal > 0 && intVal < 32) {
      RegNo = ASRRegs[intVal];
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    if (name.equals("icc")) {
      RegNo = Mos6502::ICC;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    if (name.equals("psr")) {
      RegNo = Mos6502::PSR;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    if (name.equals("wim")) {
      RegNo = Mos6502::WIM;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    if (name.equals("tbr")) {
      RegNo = Mos6502::TBR;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    if (name.equals("xcc")) {
      // FIXME:: check 64bit.
      RegNo = Mos6502::ICC;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    // %fcc0 - %fcc3
    if (name.substr(0, 3).equals_lower("fcc")
        && !name.substr(3).getAsInteger(10, intVal)
        && intVal < 4) {
      // FIXME: check 64bit and  handle %fcc1 - %fcc3
      RegNo = Mos6502::FCC0 + intVal;
      RegKind = Mos6502Operand::rk_Special;
      return true;
    }

    // %g0 - %g7
    if (name.substr(0, 1).equals_lower("g")
        && !name.substr(1).getAsInteger(10, intVal)
        && intVal < 8) {
      RegNo = IntRegs[intVal];
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }
    // %o0 - %o7
    if (name.substr(0, 1).equals_lower("o")
        && !name.substr(1).getAsInteger(10, intVal)
        && intVal < 8) {
      RegNo = IntRegs[8 + intVal];
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }
    if (name.substr(0, 1).equals_lower("l")
        && !name.substr(1).getAsInteger(10, intVal)
        && intVal < 8) {
      RegNo = IntRegs[16 + intVal];
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }
    if (name.substr(0, 1).equals_lower("i")
        && !name.substr(1).getAsInteger(10, intVal)
        && intVal < 8) {
      RegNo = IntRegs[24 + intVal];
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }
    // %f0 - %f31
    if (name.substr(0, 1).equals_lower("f")
        && !name.substr(1, 2).getAsInteger(10, intVal) && intVal < 32) {
      RegNo = FloatRegs[intVal];
      RegKind = Mos6502Operand::rk_FloatReg;
      return true;
    }
    // %f32 - %f62
    if (name.substr(0, 1).equals_lower("f")
        && !name.substr(1, 2).getAsInteger(10, intVal)
        && intVal >= 32 && intVal <= 62 && (intVal % 2 == 0)) {
      // FIXME: Check V9
      RegNo = DoubleRegs[intVal/2];
      RegKind = Mos6502Operand::rk_DoubleReg;
      return true;
    }

    // %r0 - %r31
    if (name.substr(0, 1).equals_lower("r")
        && !name.substr(1, 2).getAsInteger(10, intVal) && intVal < 31) {
      RegNo = IntRegs[intVal];
      RegKind = Mos6502Operand::rk_IntReg;
      return true;
    }
  }
  return false;
}

// Determine if an expression contains a reference to the symbol
// "_GLOBAL_OFFSET_TABLE_".
static bool hasGOTReference(const MCExpr *Expr) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    if (const Mos6502MCExpr *SE = dyn_cast<Mos6502MCExpr>(Expr))
      return hasGOTReference(SE->getSubExpr());
    break;

  case MCExpr::Constant:
    break;

  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    return hasGOTReference(BE->getLHS()) || hasGOTReference(BE->getRHS());
  }

  case MCExpr::SymbolRef: {
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    return (SymRef.getSymbol().getName() == "_GLOBAL_OFFSET_TABLE_");
  }

  case MCExpr::Unary:
    return hasGOTReference(cast<MCUnaryExpr>(Expr)->getSubExpr());
  }
  return false;
}

bool Mos6502AsmParser::matchMos6502AsmModifiers(const MCExpr *&EVal,
                                            SMLoc &EndLoc)
{
  AsmToken Tok = Parser.getTok();
  if (!Tok.is(AsmToken::Identifier))
    return false;

  StringRef name = Tok.getString();

  Mos6502MCExpr::VariantKind VK = Mos6502MCExpr::parseVariantKind(name);

  if (VK == Mos6502MCExpr::VK_Mos6502_None)
    return false;

  Parser.Lex(); // Eat the identifier.
  if (Parser.getTok().getKind() != AsmToken::LParen)
    return false;

  Parser.Lex(); // Eat the LParen token.
  const MCExpr *subExpr;
  if (Parser.parseParenExpression(subExpr, EndLoc))
    return false;

  bool isPIC = getContext().getObjectFileInfo()->getRelocM() == Reloc::PIC_;

  // Ugly: if a mos6502 assembly expression says "%hi(...)" but the
  // expression within contains _GLOBAL_OFFSET_TABLE_, it REALLY means
  // %pc22. Same with %lo -> %pc10. Worse, if it doesn't contain that,
  // the meaning depends on whether the assembler was invoked with
  // -KPIC or not: if so, it really means %got22/%got10; if not, it
  // actually means what it said! Sigh, historical mistakes...

  switch(VK) {
  default: break;
  case Mos6502MCExpr::VK_Mos6502_LO:
    VK =  (hasGOTReference(subExpr)
           ? Mos6502MCExpr::VK_Mos6502_PC10
           : (isPIC ? Mos6502MCExpr::VK_Mos6502_GOT10 : VK));
    break;
  case Mos6502MCExpr::VK_Mos6502_HI:
    VK =  (hasGOTReference(subExpr)
           ? Mos6502MCExpr::VK_Mos6502_PC22
           : (isPIC ? Mos6502MCExpr::VK_Mos6502_GOT22 : VK));
    break;
  }

  EVal = Mos6502MCExpr::create(VK, subExpr, getContext());
  return true;
}

extern "C" void LLVMInitializeMos6502AsmParser() {
  RegisterMCAsmParser<Mos6502AsmParser> C(TheMos6502Target);
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "Mos6502GenAsmMatcher.inc"

unsigned Mos6502AsmParser::validateTargetOperandClass(MCParsedAsmOperand &GOp,
                                                    unsigned Kind) {
  Mos6502Operand &Op = (Mos6502Operand &)GOp;
  if (Op.isFloatOrDoubleReg()) {
    switch (Kind) {
    default: break;
    case MCK_DFPRegs:
      if (!Op.isFloatReg() || Mos6502Operand::MorphToDoubleReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    case MCK_QFPRegs:
      if (Mos6502Operand::MorphToQuadReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    }
  }
  return Match_InvalidOperand;
}
