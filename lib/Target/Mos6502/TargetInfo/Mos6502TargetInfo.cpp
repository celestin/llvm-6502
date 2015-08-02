//===-- Mos6502TargetInfo.cpp - Mos6502 Target Implementation -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Mos6502.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target llvm::TheMos6502Target;
Target llvm::TheMos6502V9Target;
Target llvm::TheMos6502elTarget;

extern "C" void LLVMInitializeMos6502TargetInfo() {
  RegisterTarget<Triple::mos6502, /*HasJIT=*/true> X(TheMos6502Target, "mos6502",
                                                   "Mos6502");
  RegisterTarget<Triple::mos6502v9, /*HasJIT=*/true> Y(TheMos6502V9Target,
                                                     "mos6502v9", "Mos6502 V9");
  RegisterTarget<Triple::mos6502el, /*HasJIT=*/true> Z(TheMos6502elTarget,
                                                     "mos6502el", "Mos6502 LE");
}
