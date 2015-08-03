# LLVM with 6502 backend

This is an [LLVM](http://llvm.org/) fork that tries to implements an [MOS
6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) backend.

## Build

Refer to the [Getting
Started](http://llvm.org/docs/GettingStarted.html#getting-started-quickly-a-summary)
guide from LLVM.

A quick way to start (on Linux):

  * Clone this repository
  * Run inside directory:

```bash
mkdir build
cd build
cmake .. -DLLVM_TARGETS_TO_BUILD:STRING="X86;Mos6502"
make -j4
```

If you want to build more targets, append them to the `LLVM_TARGETS_TO_BUILD`
variable.  Adjust `-j` option of `make` in case you have more processor cores.

## Install

Run `make install` as usual.

## Usage

*TODO: Write how to convert from C/C++ with Clang to IR and 6502 assembly code.*

## Tests

To run regression tests for the MOS 6502 target, run from `build/`

```bash
make check-llvm-codegen-mos6502
```

For more information on the LLVM test suite, refer to the [Testing
Guide](http://llvm.org/docs/TestingGuide.html).
