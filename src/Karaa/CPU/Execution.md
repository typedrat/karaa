```haskell
{-|
Module: Karaa.CPU.Execution

The documentation in this file is extensive, but does not map cleanly to anything that Haddock comments
can be attached to, so it is written as Markdown-based Literate Haskell, which can be read in rendered
form [in the GitHub repository](https://github.com/typedrat/karaa/blob/master/src/Karaa/CPU/Execution.md).
-}
module Karaa.CPU.Execution ( execute ) where

import Data.Bits                      ( Bits(..) )
import Data.Word                      ( Word8, Word16 )
import GHC.Stack                      ( HasCallStack )

import Karaa.Core.Monad
import Karaa.Core.Types.BitInByte     ( bitInByte )
import Karaa.CPU.Interrupts.Internal  ( InterruptStatus(..), setInterruptStatus, checkForPendingInterrupt )
import Karaa.CPU.Instructions.Operand ( Operand(..), Mutability(..), zeroFlag, subtractionFlag, halfCarryFlag, carryFlag )
import Karaa.CPU.Instructions.Types   ( Instruction(..), CBInstruction(..), UsesCarry(..) )
import Karaa.CPU.Instructions.Decode  ( decodeCBInstruction )
import Karaa.CPU.LoadStore
import Karaa.CPU.Registers            ( WideRegister(HL, PC) )
```

# Known Inaccuracies:

- [ ] The [`HALT` bug](#the-halt-bug) is currently unimplemented.
- [ ] `STOP` is unimplemented.
- [ ] `EI` doesn't wait an instruction to take effect.

# Single-Byte Instructions

```haskell
execute :: Instruction -> Karaa ()
```

## Load/Store (`LD`)

Even though the SM83 is from the Bad Old CISC Days, the `LD` instruction is simple enough. Thankfully, the hardware limitations of the processor prevent any of the weird addressing modes that were so popular at the time from showing up.

The only point of potential shock is the special case for storing `HL` into the program counter, which is how we internally represent `JP HL`. It's much closer to a 16-bit register-to-register `LD` than it is to the other jump instructions, but it is a special case where the SM83 can actually move a 16-bit value in a single clock cycle.

```haskell
execute (Load8 dst src) = loadByte src >>= storeByte dst

execute (Load16 dst@(WideRegister PC) src@(WideRegister HL)) = do
    loadLower src >>= storeLower dst
    loadUpper src >>= storeUpper dst

execute (Load16 dst src) = do
    loadLower src >>= storeLower dst
    tick
    loadUpper src >>= storeUpper dst
```

## 8-Bit ALU Operations (`ADD`, `ADC`, `SUB`, `SBC`, `AND`, `OR`, `XOR`, `INC`, `DEC`, `CP`)

We start off with some fairly simple operations. Unsurprisingly, the parts of the SM83 that only deal with 8-bit data are the most regular; due to this regularity, most of the work here has been offloaded to some helper functions [defined later in the file](#common-patterns).

```haskell
execute (AddWord8 dst src usesCarry) = do
    storeFlag subtractionFlag False
    storeByte dst =<< alu8Operation (+) (loadByte dst) (loadByte src) (withCarry usesCarry)

execute (SubtractWord8 dst src usesCarry) = do
    storeFlag subtractionFlag True
    storeByte dst =<< alu8Operation (-) (loadByte dst) (loadByte src) (withCarry usesCarry)

execute (AndWord8 dst src) = bitwiseOperation (.&.) dst (loadByte src)
                          >> storeFlag subtractionFlag True
execute (XORWord8 dst src) = bitwiseOperation xor   dst (loadByte src)
execute (OrWord8 dst src)  = bitwiseOperation (.|.) dst (loadByte src)

execute (CompareWord8 dst src) = do
    storeFlag subtractionFlag True
    _ <- alu8Operation (-) (loadByte dst) (loadByte src) (pure False)
    return ()

execute (IncrementWord8 dst) = do
    a <- loadByte dst

    let (out, zero, halfCarry, _) = arithWithCarry (+) a 1 False

    storeFlag zeroFlag        zero
    storeFlag subtractionFlag False
    storeFlag halfCarryFlag   halfCarry
    storeByte dst             out

execute (DecrementWord8 dst) = do
    a <- loadByte dst

    let (out, zero, halfCarry, _) = arithWithCarry (-) a 1 False

    storeFlag zeroFlag        zero
    storeFlag subtractionFlag True
    storeFlag halfCarryFlag   halfCarry
    storeByte dst             out

execute (ComplementWord8 dst) = do
    storeByte dst . complement =<< loadByte dst 
    storeFlag subtractionFlag True
    storeFlag halfCarryFlag   True
```

### `DAA`

This has quite the reputation amongst GameBoy emulator developers, and it's for good reason. It is probably the single hardest instruction to implement to the basic "playing Tetris" standard a lot of emudevs go for, and it involves something that most people who weren't already old enough to be working programmers when the DMG came out have never even heard of: [binary-coded decimal](https://en.wikipedia.org/wiki/Binary-coded_decimal).

Specifically, it serves to "fix up" the results of other arithmetic instructions performed on BCD values, restoring the results to BCD format.

The details of why this algorithm works are beyond what I want to get into without the ability to draw non-ASCII art diagrams, but understanding its purpose is at least enough to clarify the somewhat obscure nature of the carry flag output: it is overloaded to mean a more general "overflow flag", set when the result value is too large to store as one packed-BCD byte.

```haskell
execute (DecimalAdjustWord8 dst) = do
    a           <- loadByte dst 
    subtraction <- loadFlag subtractionFlag
    halfCarry   <- loadFlag halfCarryFlag 
    carry       <- loadFlag carryFlag 

    let fixLowerNeeded = (a .&. 0x0F > 0x09) || halfCarry
        fixUpperNeeded = (a          > 0x90) || carry

        fixLower | fixLowerNeeded = 0x06
                 | otherwise      = 0
        fixUpper | fixUpperNeeded = 0x60
                 | otherwise      = 0

        out = if subtraction
            then a - fixLower - fixUpper
            else a + fixLower + fixUpper

    storeFlag zeroFlag        (out == 0)
    storeFlag halfCarryFlag   False
    storeFlag carryFlag       (out > 0x99)
    storeByte dst             out
```

## 16-Bit Arithmetic Operations (`ADD`, `INC`, `DEC`)

Of note in the implementations of these instructions is the first appearance of manual clock `tick`ing, which is used here because the SM83's ALU can only operate on 8 bits per clock. It is somewhat surprising that `INC r16` and `DEC r16` are each two cycles long, considering that the CPU does have a single-cycle 16-bit increment/decrement unit that it uses for certain addressing modes.

```haskell
execute (AddWord16 dst src) = do
    aLower <- loadLower dst
    bLower <- loadLower src
    let (outLower, _, _, lowerCarry) = arithWithCarry (+) aLower bLower False
    storeLower dst outLower

    tick

    aUpper <- loadUpper dst
    bUpper <- loadUpper src
    let (outUpper, _, halfCarry, carry) = arithWithCarry (+) aUpper bUpper lowerCarry
    storeFlag  halfCarryFlag halfCarry
    storeFlag  carryFlag     carry
    storeUpper dst           outUpper

execute (IncrementWord16 dst) = do
    aLower <- loadLower dst
    let (outLower, _, _, lowerCarry) = arithWithCarry (+) aLower 1 False
    storeLower dst outLower

    tick

    aUpper <- loadUpper dst
    let (outUpper, _, _, _) = arithWithCarry (+) aUpper 0 lowerCarry
    storeUpper dst outUpper

execute (DecrementWord16 dst) = do
    aLower <- loadLower dst
    let (outLower, _, _, lowerCarry) = arithWithCarry (-) aLower 1 False
    storeLower dst outLower

    tick

    aUpper <- loadUpper dst
    let (outUpper, _, _, _) = arithWithCarry (-) aUpper 0 lowerCarry
    storeUpper dst outUpper
```

## Stack Pointer Operations (`ADD SP, i8`, `LD HL, SP+i8`, `PUSH`, `POP`, `LD (u16), SP`)

Somewhat surprisingly, `LD HL, SP+i8` is actually _faster_ than `ADD SP, i8`, an instruction which on the surface seems to do fewer fundamental operations.

First, given that `ADD SP, i8` takes 4 CPU clock cycles to execute, what is a logical (but incorrect) way that the instruction could be executed?

1. Fetch.
2. Load the 8-bit signed immediate into a 16-bit scratch register with sign extension.
3. Add the lower byte of the scratch register to SP.
4. Add the upper byte of the scratch register to SP.

Now, it makes sense why someone would implement `ADD SP, i8` in that way. What (seemingly) doesn't make sense is the fact that they clearly can do it faster, and they know it because they do it for `LD HL, SP+i8`!

1. Fetch.
2. Perform most of the operation!
   1. Fetch the 8-bit signed intermediate
   2. Route it into the ALU along with the value in the lower byte of `SP`. Flags are set here.
   3. Store it to `L`.
3. Do the equivalent of the execute stage of `ADC H, <sign bit x 8>`, but without setting flags.

The explanation is that despite how much stack pointer math is going to happen in any non-trivial program, we need to remember that this is an 8-bit CPU with byte-level addressing and, therefore, the address granularity is the same as the word size. This means that normal stack operations (`PUSH`, `POP`, `CALL`, `RET`, and `RST`) only ever need to increment or decrement `SP` during a micro-op.

For that reason, there is no need for the ALU to be able to output values into `SP`. This is an accumulator-based architecture, and `HL` is the 16-bit accumulator. In reality, the `ADD SP, i8` instruction is likely implemented something like this:

1. Fetch.
2. Perform most of the operation!
   1. Fetch the 8-bit signed intermediate
   2. Route it into the ALU along with the value in the lower byte of `SP`. Flags are set here.
   3. Store it to the lower byte of a hidden scratch register.
3. Do the equivalent of the execute stage of `ADC <upper byte of scratch>, <sign bit x 8>`, but without setting flags.
4. Move the value from the scratch register into `SP`.

```haskell
execute (AddSigned dst src) = do
    aLower <- loadLower dst
    bLower <- loadInt src
    let (outLower, _, halfCarry, carry) = arithWithCarry (+) aLower (fromIntegral bLower) False

    storeFlag halfCarryFlag halfCarry
    storeFlag carryFlag     carry
    tick

    aUpper <- loadUpper dst
    let bUpper = if bLower < 0 then 0xFF else 0x00
        (outUpper, _, _, _) = arithWithCarry (+) aUpper bUpper carry
  
    tick
  
    storeLower dst outLower
    storeUpper dst outUpper

execute (LoadSigned dst src off) = do
    aLower <- loadLower src
    bLower <- loadInt off
    let (outLower, _, halfCarry, carry) = arithWithCarry (+) aLower (fromIntegral bLower) False
    storeLower dst outLower

    storeFlag halfCarryFlag halfCarry
    storeFlag carryFlag     carry
    tick

    aUpper <- loadUpper src
    let bUpper = if bLower < 0 then 0xFF else 0x00
        (outUpper, _, _, _) = arithWithCarry (+) aUpper bUpper carry
    storeUpper dst outUpper
```

Now, with the difficult two out of the way, we are left with the instructions that perform the basic, primitive stack operations. They aren't implemented here, but in [Common Patterns](#push-and-pop) because they are also reused as components of [other instructions](#jumps-and-function-calls-jp-jr-call-ret-reti-rst).

```haskell
execute (Push src) = push src
execute (Pop dst)  = pop  dst

execute (SaveStackPointer dst) = unimplementedInstruction
```

## Accumulator Bitwise Rotations (`RLCA`, `RLA`, `RRCA`, `RRA`)

We'll talk about bitwise rotation operations [later](#rotations-and-shifts-rlc-rl-rrc-rr-sla-sra-srl-swap), when they show up again in the `CB xx` family of instructions.

```haskell
execute (RotateRegALeft usesCarry) = unimplementedInstruction
execute (RotateRegARight usesCarry) = unimplementedInstruction
```

## CPU Control Operations (`CCF`, `SCF`, `NOP`, `HALT`, `STOP`, `EI`, `DI`)

Many of these are self-explanatory, but still necessary.

```haskell
execute ToggleCarryFlag   = (storeFlag carryFlag . not =<< loadFlag carryFlag)
                         >> storeFlag subtractionFlag False
                         >> storeFlag halfCarryFlag   False
execute SetCarryFlag      = storeFlag carryFlag       True
                         >> storeFlag subtractionFlag False
                         >> storeFlag halfCarryFlag   False
execute NoOperation       = return ()
execute EnableInterrupts  = setInterruptStatus InterruptsEnabled
execute DisableInterrupts = setInterruptStatus InterruptsDisabled
```

### The `HALT` Bug

```haskell
execute Halt              = haltLoop
    where
        haltLoop = checkForPendingInterrupt >>= \case
            Just _  -> return ()
            Nothing -> tick >> haltLoop
```

```haskell
execute Stop             = unimplementedInstruction
```

## Jumps and Function Calls (`JP`, `JR`, `CALL`, `RET`, `RETI`, `RST`)

```haskell
execute (AbsoluteJump target) = unimplementedInstruction

execute (ConditionalAbsoluteJump flag target) = unimplementedInstruction
execute (RelativeJump target) = unimplementedInstruction

execute (ConditionalRelativeJump flag target) = unimplementedInstruction
execute (Call target) = unimplementedInstruction
execute (ConditionalCall flag target) = unimplementedInstruction
execute Return = unimplementedInstruction
execute (ConditionalReturn flag) = unimplementedInstruction
execute ReturnAndEnableInterrupts = unimplementedInstruction
execute (Reset target) = unimplementedInstruction
```

## Back Matter

These aren't actually (single-byte) instructions, but we still need to handle them, either by dispatching the real opcode to the execution function for `CB xx` instructions, or by throwing an error when faced with an invalid instruction.

```haskell
execute CBPrefix           = executeCB . decodeCBInstruction =<< loadByte ImmediateWord8
execute InvalidInstruction = unimplementedInstruction

unimplementedInstruction :: HasCallStack => Karaa ()
unimplementedInstruction = error "Unimplemented instruction encountered!"
```

# `CB xx` Instructions

```haskell
executeCB :: CBInstruction -> Karaa ()
```

## Rotations and Shifts (`RLC`, `RL`, `RRC`, `RR`, `SLA`, `SRA`, `SRL`, `SWAP`)

```haskell
executeCB (RotateLeft dst usesCarry) = unimplementedInstruction
executeCB (RotateRight dst usesCarry) = unimplementedInstruction
executeCB (ArithmeticShiftLeft dst) = unimplementedInstruction
executeCB (ArithmeticShiftRight dst) = unimplementedInstruction
executeCB (LogicalShiftRight dst) = unimplementedInstruction

executeCB (SwapNibble dst) = bitwiseOperation rotateR dst (pure 4)
```

## Bit Manipulation Operations (`BIT`, `SET`, `RES`)

```haskell
executeCB (TestBit bib op) = do
    a <- loadByte op

    storeFlag zeroFlag        (testBit a $ bitInByte bib)
    storeFlag subtractionFlag False
    storeFlag halfCarryFlag   True

executeCB (SetBit bib dst)   = storeByte dst . flip setBit   (bitInByte bib) =<< loadByte dst
executeCB (ResetBit bib dst) = storeByte dst . flip clearBit (bitInByte bib) =<< loadByte dst
```

# Common Patterns

These are utility functions that are useful to the actual execution of instructions and common instruction structures that repeat enough to be worth factoring out into separate functions.

`arithWithCarry` is the most interesting function in this section. It is used to simulate the operation of a 8-bit ALU with flag output on a modern processor; rather than using the host CPU's implementation of these features that may or may not be compatible, we extend our bytes into full machine words (which is a no-op in GHC) and operate on them in that format, using the additional bits of precision to determine the flags' values.

```haskell
arithWithCarry :: (Word -> Word -> Word) -> Word8 -> Word8 -> Bool -> (Word8, Bool, Bool, Bool)
arithWithCarry op a b c = (fromIntegral out, zero, halfCarry, carry)
    where
        zero      = out == 0
        halfCarry = halfCarryOut > halfCarryMask
        carry     = out > 0xFF

        a' = fromIntegral a
        b' = fromIntegral b + c'
        c' = if c then 1 else 0
        out = a' `op` b'

        halfCarryMask = 0x0F
        halfCarryOut  = (a' .&. halfCarryMask) `op` (b' .&. halfCarryMask)

bitwiseOperation :: (Word8 -> b -> Word8) -> Operand 'RW Word8 -> Karaa b -> Karaa ()
bitwiseOperation op dst src = do
    a <- loadByte dst
    b <- src

    let out = a `op` b

    storeFlag zeroFlag        (out == 0)
    storeFlag subtractionFlag False
    storeFlag halfCarryFlag   False
    storeFlag carryFlag       False
    storeByte dst             out


alu8Operation :: (Word -> Word -> Word) -> Karaa Word8 -> Karaa Word8 -> Karaa Bool -> Karaa Word8
alu8Operation op dst src useCarry = do
    a <- dst
    b <- src
    c <- useCarry

    let (out, zero, halfCarry, carry) = arithWithCarry op a b c

    storeFlag zeroFlag        zero
    storeFlag halfCarryFlag   halfCarry
    storeFlag carryFlag       carry
    
    return out

withCarry :: UsesCarry -> Karaa Bool
withCarry WithCarry    = loadFlag carryFlag
withCarry WithoutCarry = pure False
```

# `push` and `pop`

```haskell
push :: Operand 'RW Word16 -> Karaa ()
push src = unimplementedInstruction

pop :: Operand 'RW Word16 -> Karaa ()
pop dst = unimplementedInstruction
```
