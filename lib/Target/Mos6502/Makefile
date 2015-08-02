##===- lib/Target/Mos6502/Makefile -------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

LEVEL = ../../..
LIBRARYNAME = LLVMMos6502CodeGen
TARGET = Mos6502

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = Mos6502GenRegisterInfo.inc Mos6502GenInstrInfo.inc \
		Mos6502GenAsmWriter.inc Mos6502GenAsmMatcher.inc \
		Mos6502GenDAGISel.inc Mos6502GenDisassemblerTables.inc \
		Mos6502GenSubtargetInfo.inc Mos6502GenCallingConv.inc \
		Mos6502GenMCCodeEmitter.inc

DIRS = InstPrinter AsmParser Disassembler TargetInfo MCTargetDesc

include $(LEVEL)/Makefile.common

