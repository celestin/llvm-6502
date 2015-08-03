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

extern "C" void LLVMInitializeMos6502TargetInfo() {
  RegisterTarget<Triple::mos6502, /*HasJIT=*/true> Z(TheMos6502Target,
                                                     "mos6502", "Mos6502");
}
