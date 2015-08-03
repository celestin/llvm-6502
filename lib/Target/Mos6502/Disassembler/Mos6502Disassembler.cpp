//===- Mos6502Disassembler.cpp - Disassembler for Mos6502 -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is part of the Mos6502 Disassembler.
//
//===----------------------------------------------------------------------===//

#include "Mos6502.h"
#include "Mos6502RegisterInfo.h"
#include "Mos6502Subtarget.h"
#include "llvm/MC/MCDisassembler.h"
#include "llvm/MC/MCFixedLenDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "mos6502-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {

/// A disassembler class for Mos6502.
class Mos6502Disassembler : public MCDisassembler {
public:
  Mos6502Disassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx) {}
  virtual ~Mos6502Disassembler() {}

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &VStream,
                              raw_ostream &CStream) const override;
};
}

namespace llvm {
extern Target TheMos6502Target;
}

static MCDisassembler *createMos6502Disassembler(const Target &T,
                                               const MCSubtargetInfo &STI,
                                               MCContext &Ctx) {
  return new Mos6502Disassembler(STI, Ctx);
}


extern "C" void LLVMInitializeMos6502Disassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(TheMos6502Target,
                                         createMos6502Disassembler);
}

static const unsigned IntRegDecoderTable[] = {
  M6502::G0,  M6502::G1,  M6502::G2,  M6502::G3,
  M6502::G4,  M6502::G5,  M6502::G6,  M6502::G7,
  M6502::O0,  M6502::O1,  M6502::O2,  M6502::O3,
  M6502::O4,  M6502::O5,  M6502::O6,  M6502::O7,
  M6502::L0,  M6502::L1,  M6502::L2,  M6502::L3,
  M6502::L4,  M6502::L5,  M6502::L6,  M6502::L7,
  M6502::I0,  M6502::I1,  M6502::I2,  M6502::I3,
  M6502::I4,  M6502::I5,  M6502::I6,  M6502::I7 };

static const unsigned FPRegDecoderTable[] = {
  M6502::F0,   M6502::F1,   M6502::F2,   M6502::F3,
  M6502::F4,   M6502::F5,   M6502::F6,   M6502::F7,
  M6502::F8,   M6502::F9,   M6502::F10,  M6502::F11,
  M6502::F12,  M6502::F13,  M6502::F14,  M6502::F15,
  M6502::F16,  M6502::F17,  M6502::F18,  M6502::F19,
  M6502::F20,  M6502::F21,  M6502::F22,  M6502::F23,
  M6502::F24,  M6502::F25,  M6502::F26,  M6502::F27,
  M6502::F28,  M6502::F29,  M6502::F30,  M6502::F31 };

static const unsigned DFPRegDecoderTable[] = {
  M6502::D0,   M6502::D16,  M6502::D1,   M6502::D17,
  M6502::D2,   M6502::D18,  M6502::D3,   M6502::D19,
  M6502::D4,   M6502::D20,  M6502::D5,   M6502::D21,
  M6502::D6,   M6502::D22,  M6502::D7,   M6502::D23,
  M6502::D8,   M6502::D24,  M6502::D9,   M6502::D25,
  M6502::D10,  M6502::D26,  M6502::D11,  M6502::D27,
  M6502::D12,  M6502::D28,  M6502::D13,  M6502::D29,
  M6502::D14,  M6502::D30,  M6502::D15,  M6502::D31 };

static const unsigned QFPRegDecoderTable[] = {
  M6502::Q0,  M6502::Q8,   ~0U,  ~0U,
  M6502::Q1,  M6502::Q9,   ~0U,  ~0U,
  M6502::Q2,  M6502::Q10,  ~0U,  ~0U,
  M6502::Q3,  M6502::Q11,  ~0U,  ~0U,
  M6502::Q4,  M6502::Q12,  ~0U,  ~0U,
  M6502::Q5,  M6502::Q13,  ~0U,  ~0U,
  M6502::Q6,  M6502::Q14,  ~0U,  ~0U,
  M6502::Q7,  M6502::Q15,  ~0U,  ~0U } ;

static const unsigned FCCRegDecoderTable[] = {
  M6502::FCC0, M6502::FCC1, M6502::FCC2, M6502::FCC3 };

static const unsigned ASRRegDecoderTable[] = {
  M6502::Y,     M6502::ASR1,  M6502::ASR2,  M6502::ASR3,
  M6502::ASR4,  M6502::ASR5,  M6502::ASR6,  M6502::ASR7,
  M6502::ASR8,  M6502::ASR9,  M6502::ASR10, M6502::ASR11,
  M6502::ASR12, M6502::ASR13, M6502::ASR14, M6502::ASR15,
  M6502::ASR16, M6502::ASR17, M6502::ASR18, M6502::ASR19,
  M6502::ASR20, M6502::ASR21, M6502::ASR22, M6502::ASR23,
  M6502::ASR24, M6502::ASR25, M6502::ASR26, M6502::ASR27,
  M6502::ASR28, M6502::ASR29, M6502::ASR30, M6502::ASR31};

static DecodeStatus DecodeIntRegsRegisterClass(MCInst &Inst,
                                               unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = IntRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeI64RegsRegisterClass(MCInst &Inst,
                                               unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = IntRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}


static DecodeStatus DecodeFPRegsRegisterClass(MCInst &Inst,
                                              unsigned RegNo,
                                              uint64_t Address,
                                              const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = FPRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}


static DecodeStatus DecodeDFPRegsRegisterClass(MCInst &Inst,
                                               unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  unsigned Reg = DFPRegDecoderTable[RegNo];
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}


static DecodeStatus DecodeQFPRegsRegisterClass(MCInst &Inst,
                                               unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;

  unsigned Reg = QFPRegDecoderTable[RegNo];
  if (Reg == ~0U)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(Reg));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeFCCRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 3)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(FCCRegDecoderTable[RegNo]));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeASRRegsRegisterClass(MCInst &Inst, unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(ASRRegDecoderTable[RegNo]));
  return MCDisassembler::Success;
}


static DecodeStatus DecodeLoadInt(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder);
static DecodeStatus DecodeLoadFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const void *Decoder);
static DecodeStatus DecodeLoadDFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder);
static DecodeStatus DecodeLoadQFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder);
static DecodeStatus DecodeStoreInt(MCInst &Inst, unsigned insn,
                                   uint64_t Address, const void *Decoder);
static DecodeStatus DecodeStoreFP(MCInst &Inst, unsigned insn,
                                  uint64_t Address, const void *Decoder);
static DecodeStatus DecodeStoreDFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address, const void *Decoder);
static DecodeStatus DecodeStoreQFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address, const void *Decoder);
static DecodeStatus DecodeCall(MCInst &Inst, unsigned insn,
                               uint64_t Address, const void *Decoder);
static DecodeStatus DecodeSIMM13(MCInst &Inst, unsigned insn,
                                 uint64_t Address, const void *Decoder);
static DecodeStatus DecodeJMPL(MCInst &Inst, unsigned insn, uint64_t Address,
                               const void *Decoder);
static DecodeStatus DecodeReturn(MCInst &MI, unsigned insn, uint64_t Address,
                                 const void *Decoder);
static DecodeStatus DecodeSWAP(MCInst &Inst, unsigned insn, uint64_t Address,
                               const void *Decoder);

#include "Mos6502GenDisassemblerTables.inc"

/// Read four bytes from the ArrayRef and return 32 bit word.
static DecodeStatus readInstruction32(ArrayRef<uint8_t> Bytes, uint64_t Address,
                                      uint64_t &Size, uint32_t &Insn,
                                      bool IsLittleEndian) {
  // We want to read exactly 4 Bytes of data.
  if (Bytes.size() < 4) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  Insn = IsLittleEndian
             ? (Bytes[0] << 0) | (Bytes[1] << 8) | (Bytes[2] << 16) |
                   (Bytes[3] << 24)
             : (Bytes[3] << 0) | (Bytes[2] << 8) | (Bytes[1] << 16) |
                   (Bytes[0] << 24);

  return MCDisassembler::Success;
}

DecodeStatus Mos6502Disassembler::getInstruction(MCInst &Instr, uint64_t &Size,
                                               ArrayRef<uint8_t> Bytes,
                                               uint64_t Address,
                                               raw_ostream &VStream,
                                               raw_ostream &CStream) const {
  uint32_t Insn;
  bool isLittleEndian = getContext().getAsmInfo()->isLittleEndian();
  DecodeStatus Result =
      readInstruction32(Bytes, Address, Size, Insn, isLittleEndian);
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  // Calling the auto-generated decoder function.
  Result =
      decodeInstruction(DecoderTableMos650232, Instr, Insn, Address, this, STI);

  if (Result != MCDisassembler::Fail) {
    Size = 4;
    return Result;
  }

  return MCDisassembler::Fail;
}


typedef DecodeStatus (*DecodeFunc)(MCInst &MI, unsigned insn, uint64_t Address,
                                   const void *Decoder);

static DecodeStatus DecodeMem(MCInst &MI, unsigned insn, uint64_t Address,
                              const void *Decoder,
                              bool isLoad, DecodeFunc DecodeRD) {
  unsigned rd = fieldFromInstruction(insn, 25, 5);
  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  bool isImm = fieldFromInstruction(insn, 13, 1);
  bool hasAsi = fieldFromInstruction(insn, 23, 1); // (in op3 field)
  unsigned asi = fieldFromInstruction(insn, 5, 8);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  DecodeStatus status;
  if (isLoad) {
    status = DecodeRD(MI, rd, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  // Decode rs1.
  status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode imm|rs2.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  if (hasAsi)
    MI.addOperand(MCOperand::createImm(asi));

  if (!isLoad) {
    status = DecodeRD(MI, rd, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeLoadInt(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeIntRegsRegisterClass);
}

static DecodeStatus DecodeLoadFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                 const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeFPRegsRegisterClass);
}

static DecodeStatus DecodeLoadDFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeDFPRegsRegisterClass);
}

static DecodeStatus DecodeLoadQFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, true,
                   DecodeQFPRegsRegisterClass);
}

static DecodeStatus DecodeStoreInt(MCInst &Inst, unsigned insn,
                                   uint64_t Address, const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeIntRegsRegisterClass);
}

static DecodeStatus DecodeStoreFP(MCInst &Inst, unsigned insn, uint64_t Address,
                                  const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeFPRegsRegisterClass);
}

static DecodeStatus DecodeStoreDFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address, const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeDFPRegsRegisterClass);
}

static DecodeStatus DecodeStoreQFP(MCInst &Inst, unsigned insn,
                                   uint64_t Address, const void *Decoder) {
  return DecodeMem(Inst, insn, Address, Decoder, false,
                   DecodeQFPRegsRegisterClass);
}

static bool tryAddingSymbolicOperand(int64_t Value,  bool isBranch,
                                     uint64_t Address, uint64_t Offset,
                                     uint64_t Width, MCInst &MI,
                                     const void *Decoder) {
  const MCDisassembler *Dis = static_cast<const MCDisassembler*>(Decoder);
  return Dis->tryAddingSymbolicOperand(MI, Value, Address, isBranch,
                                       Offset, Width);
}

static DecodeStatus DecodeCall(MCInst &MI, unsigned insn,
                               uint64_t Address, const void *Decoder) {
  unsigned tgt = fieldFromInstruction(insn, 0, 30);
  tgt <<= 2;
  if (!tryAddingSymbolicOperand(tgt+Address, false, Address,
                                0, 30, MI, Decoder))
    MI.addOperand(MCOperand::createImm(tgt));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSIMM13(MCInst &MI, unsigned insn,
                                 uint64_t Address, const void *Decoder) {
  unsigned tgt = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  MI.addOperand(MCOperand::createImm(tgt));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeJMPL(MCInst &MI, unsigned insn, uint64_t Address,
                               const void *Decoder) {

  unsigned rd = fieldFromInstruction(insn, 25, 5);
  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RD.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rd, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1.
  status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1 | SIMM13.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeReturn(MCInst &MI, unsigned insn, uint64_t Address,
                                 const void *Decoder) {

  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RS1.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS2 | SIMM13.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }
  return MCDisassembler::Success;
}

static DecodeStatus DecodeSWAP(MCInst &MI, unsigned insn, uint64_t Address,
                               const void *Decoder) {

  unsigned rd = fieldFromInstruction(insn, 25, 5);
  unsigned rs1 = fieldFromInstruction(insn, 14, 5);
  unsigned isImm = fieldFromInstruction(insn, 13, 1);
  bool hasAsi = fieldFromInstruction(insn, 23, 1); // (in op3 field)
  unsigned asi = fieldFromInstruction(insn, 5, 8);
  unsigned rs2 = 0;
  unsigned simm13 = 0;
  if (isImm)
    simm13 = SignExtend32<13>(fieldFromInstruction(insn, 0, 13));
  else
    rs2 = fieldFromInstruction(insn, 0, 5);

  // Decode RD.
  DecodeStatus status = DecodeIntRegsRegisterClass(MI, rd, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1.
  status = DecodeIntRegsRegisterClass(MI, rs1, Address, Decoder);
  if (status != MCDisassembler::Success)
    return status;

  // Decode RS1 | SIMM13.
  if (isImm)
    MI.addOperand(MCOperand::createImm(simm13));
  else {
    status = DecodeIntRegsRegisterClass(MI, rs2, Address, Decoder);
    if (status != MCDisassembler::Success)
      return status;
  }

  if (hasAsi)
    MI.addOperand(MCOperand::createImm(asi));

  return MCDisassembler::Success;
}
