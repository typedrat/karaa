# karaa

A Haskell GameBoy emulator. It intends to be as accurate as possible to the original hardware.

## Goals

* Full support for the DMG-{A,B,C} and CGB.
* Hardware accuracy down to the T-cycle level.
* Full compatibility with as many licensed games as possible. 
* A flexible design intended to make creating custom frontends easy.
* Proving that accurate emulation with acceptable performance is possible with Haskell.

## Non-Goals:

* SGB support.
* Peripheral emulation.
* Support for poorly-behaved homebrew/unlicensed software.

## Status

Currently, we pass all of Blargg's `cpu_instrs` test ROM. We're unfortunately pretty slow, only running about 450% faster than real hardware.

Further testing is awaiting implementation of the PPU.
