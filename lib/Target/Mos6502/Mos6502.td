//===-- Mos6502.td - Describe the Mos6502 Target Machine -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// MOS6502 Subtarget features.
//

def FeatureV9
  : SubtargetFeature<"v9", "IsV9", "true",
                     "Enable MOS6502-V9 instructions">;
def FeatureV8Deprecated
  : SubtargetFeature<"deprecated-v8", "V8DeprecatedInsts", "true",
                     "Enable deprecated V8 instructions in V9 mode">;
def FeatureVIS
  : SubtargetFeature<"vis", "IsVIS", "true",
                     "Enable UltraMOS6502 Visual Instruction Set extensions">;
def FeatureVIS2
  : SubtargetFeature<"vis2", "IsVIS2", "true",
                     "Enable Visual Instruction Set extensions II">;
def FeatureVIS3
  : SubtargetFeature<"vis3", "IsVIS3", "true",
                     "Enable Visual Instruction Set extensions III">;

def FeatureHardQuad
  : SubtargetFeature<"hard-quad-float", "HasHardQuad", "true",
                     "Enable quad-word floating point instructions">;

def UsePopc : SubtargetFeature<"popc", "UsePopc", "true",
                               "Use the popc (population count) instruction">;

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "Mos6502RegisterInfo.td"
include "Mos6502CallingConv.td"
include "Mos6502InstrInfo.td"

def Mos6502InstrInfo : InstrInfo;

def Mos6502AsmParser : AsmParser {
  bit ShouldEmitMatchRegisterName = 0;
}

//===----------------------------------------------------------------------===//
// MOS6502 processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic",         []>;
def : Proc<"v7",              []>;
def : Proc<"v8",              []>;
def : Proc<"supermos6502",      []>;
def : Proc<"mos6502lite",       []>;
def : Proc<"f934",            []>;
def : Proc<"hypermos6502",      []>;
def : Proc<"mos6502lite86x",    []>;
def : Proc<"mos6502let",        []>;
def : Proc<"tsc701",          []>;
def : Proc<"v9",              [FeatureV9]>;
def : Proc<"ultramos6502",      [FeatureV9, FeatureV8Deprecated, FeatureVIS]>;
def : Proc<"ultramos65023",     [FeatureV9, FeatureV8Deprecated, FeatureVIS,
                               FeatureVIS2]>;
def : Proc<"niagara",         [FeatureV9, FeatureV8Deprecated, FeatureVIS,
                               FeatureVIS2]>;
def : Proc<"niagara2",        [FeatureV9, FeatureV8Deprecated, UsePopc,
                               FeatureVIS, FeatureVIS2]>;
def : Proc<"niagara3",        [FeatureV9, FeatureV8Deprecated, UsePopc,
                               FeatureVIS, FeatureVIS2]>;
def : Proc<"niagara4",        [FeatureV9, FeatureV8Deprecated, UsePopc,
                               FeatureVIS, FeatureVIS2, FeatureVIS3]>;


//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def Mos6502AsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  int PassSubtarget = 1;
  int Variant = 0;
}

def Mos6502 : Target {
  // Pull in Instruction Info:
  let InstructionSet = Mos6502InstrInfo;
  let AssemblyParsers  = [Mos6502AsmParser];
  let AssemblyWriters = [Mos6502AsmWriter];
}
