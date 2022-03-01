module Karaa.CPU.Execution where

import Control.Lens.Operators         ( (^.) )
import Data.Bits                      ( Bits(..) )
import Data.Int                       ( Int8 )
import Data.Word                      ( Word8, Word16 )

import Karaa.Core.Monad
import Karaa.Core.Types.BitInByte     ( bitInByte )
import Karaa.CPU.Interrupts.Internal  ( InterruptStatus(..), setInterruptStatus, checkForPendingInterrupt )
import Karaa.CPU.Instructions.Operand ( Operand(..), Mutability(..), zeroFlag, subtractionFlag, halfCarryFlag, carryFlag )
import Karaa.CPU.Instructions.Types   ( Instruction(..), CBInstruction(..), UsesCarry(..) )
import Karaa.CPU.Instructions.Decode  ( decodeCBInstruction )
import Karaa.CPU.LoadStore            ( load, store )
import Karaa.CPU.Registers            ( WideRegister(PC) )
import Karaa.Util.ByteLenses          ( lower, upper )

execute :: Instruction -> Karaa ()
execute (Load dst src) = load src >>= store dst

execute (AddWord8 dst src usesCarry) = do
    a <- load dst
    b <- load src
    c <- case usesCarry of
        WithCarry    -> load carryFlag
        WithoutCarry -> pure False

    let (out, zero, halfCarry, carry) = arithWithCarry (+) a b c

    store zeroFlag zero
    store subtractionFlag False
    store halfCarryFlag halfCarry
    store carryFlag carry
    store dst out

execute (SubtractWord8 dst src usesCarry) = do
    a <- load dst
    b <- load src
    c <- case usesCarry of
        WithCarry    -> load carryFlag
        WithoutCarry -> pure False

    let (out, zero, halfCarry, carry) = arithWithCarry (-) a b c

    store zeroFlag        zero
    store subtractionFlag True
    store halfCarryFlag   halfCarry
    store carryFlag       carry
    store dst             out

execute (AndWord8 dst src) = bitwiseOperation (.&.) dst (load src)
                          >> store subtractionFlag True
execute (XORWord8 dst src) = bitwiseOperation xor   dst (load src)
execute (OrWord8 dst src)  = bitwiseOperation (.|.) dst (load src)

execute (CompareWord8 dst src) = do
    a <- load dst
    b <- load src

    let (_, zero, halfCarry, carry) = arithWithCarry (-) a b False

    store zeroFlag        zero
    store subtractionFlag True
    store halfCarryFlag   halfCarry
    store carryFlag       carry

execute (IncrementWord8 dst) = do
    a <- load dst

    let (out, zero, halfCarry, _) = arithWithCarry (+) a 1 False

    store zeroFlag        zero
    store subtractionFlag False
    store halfCarryFlag   halfCarry
    store dst             out

execute (DecrementWord8 dst) = do
    a <- load dst

    let (out, zero, halfCarry, _) = arithWithCarry (-) a 1 False

    store zeroFlag        zero
    store subtractionFlag True
    store halfCarryFlag   halfCarry
    store dst             out

execute (DecimalAdjustWord8 dst) = do
    a           <- load dst 
    subtraction <- load subtractionFlag
    halfCarry   <- load halfCarryFlag 
    carry       <- load carryFlag 

    let fixLowerNeeded = (a .&. 0x0F > 0x09) || halfCarry
        fixUpperNeeded = (a          > 0x90) || carry

        fixLower | fixLowerNeeded = 0x06
                 | otherwise      = 0
        fixUpper | fixUpperNeeded = 0x60
                 | otherwise      = 0

        out = if subtraction
            then a - fixLower - fixUpper
            else a + fixLower + fixUpper

    store zeroFlag        (out == 0)
    store halfCarryFlag   False
    store carryFlag       (out > 0x99)
    store dst             out


execute (ComplementWord8 dst) = do
    store dst . complement =<< load dst 
    store subtractionFlag True
    store halfCarryFlag   True

execute (AddWord16 dst src) = do
    a <- load dst
    b <- load src

    let (outLower, _, _,         lowerCarry) = arithWithCarry (+) (a ^. lower) (b ^. lower) False
        (outUpper, _, halfCarry, carry)      = arithWithCarry (+) (a ^. upper) (b ^. upper) lowerCarry
    
    store halfCarryFlag halfCarry
    store carryFlag     carry
    store dst           (bytesToWord16 outLower outUpper)

execute (IncrementWord16 dst) = do
    a <- load dst

    let (outLower, _, _, lowerCarry) = arithWithCarry (+) (a ^. lower) 1 False
        (outUpper, _, _, _)          = arithWithCarry (+) (a ^. upper) 0 lowerCarry
    
    store dst (bytesToWord16 outLower outUpper)

execute (DecrementWord16 dst) = do
    a <- load dst

    let (outLower, _, _, lowerCarry) = arithWithCarry (-) (a ^. lower) 1 False
        (outUpper, _, _, _)          = arithWithCarry (-) (a ^. upper) 0 lowerCarry
    tick
    
    store dst (bytesToWord16 outLower outUpper)

execute (AddSigned dst src) = do
    -- (Incorrect) theory: This instruction is microcoded as something like: 
    -- 1. fetch
    -- 2. load the 8-bit signed immediate into a 16-bit scratch register with sign extension
    -- 3. add the lower byte of the scratch register to SP
    -- 4. add the upper byte of the scratch register to SP
    --
    -- See the case for 'LoadSigned' to find out what's really going on. 
    a <- load dst
    b <- load src

    let (out, halfCarry, carry) = w16PlusI8WithCarry a b
    tick

    store zeroFlag        False
    store subtractionFlag False
    store halfCarryFlag   halfCarry
    store carryFlag       carry
    store dst             out

execute (LoadSigned dst src off) = do
    -- Now, it makes sense why someone would implement 'AddSigned' in that way.
    -- What (seemingly) doesn't make sense is the fact that they clearly can
    -- do it faster, and they know it because they do it here!
    --
    -- 1. fetch
    -- 2. perform most of the operation!
    --    1. fetch the 8-bit signed intermediate
    --    2. route it into the ALU along with the value in the lower byte of SP
    --       (flags are set here)
    --    3. store it to L
    -- 3. do the equivalent of the execute stage of ADC H, <carry bit x 8> but
    --    without setting flags
    --
    -- The explanation is that despite how much SP math is going to happen in
    -- any non-trivial program, we need to remember that this is an 8-bit CPU
    -- with byte-level addressing and, therefore, the address granularity is
    -- the same as the word size. This means that normal stack operations
    -- (PUSH, POP, CALL, RET, and RST) only ever need to increment or decrement
    -- SP during a micro-op. For that reason, there is no need for the ALU to
    -- be able to output values into SP.
    --
    -- In reality, the @ADD, SP, i8@ 
    -- 
    -- This, incidentally, is one of the many reasons why programming the
    -- GameBoy in C is horridly slow. Each time you want to access a local
    -- (and thus stack-allocated) value, you have to @LD HL, SP, @
    a <- load src
    b <- load off

    let (out, halfCarry, carry) = w16PlusI8WithCarry a b

    store zeroFlag        False
    store subtractionFlag False
    store halfCarryFlag   halfCarry
    store carryFlag       carry
    store dst             out

execute (RotateRegALeft usesCarry) = _wq
execute (RotateRegARight usesCarry) = _wr

execute (Push src) = _ws
execute (Pop src) = _wt

execute ToggleCarryFlag   = (store carryFlag . not =<< load carryFlag)
                         >> store subtractionFlag False
                         >> store halfCarryFlag   False
execute SetCarryFlag      = store carryFlag       True
                         >> store subtractionFlag False
                         >> store halfCarryFlag   False
execute NoOperation       = return ()
execute Halt              = haltLoop
    where
        haltLoop = checkForPendingInterrupt >>= \case
            Just _  -> return ()
            Nothing -> tick >> haltLoop

execute Stop              = _wy
execute EnableInterrupts  = setInterruptStatus InterruptsEnabled
execute DisableInterrupts = setInterruptStatus InterruptsDisabled

execute (AbsoluteJump target) = do
    newPC <- load target
    store (WideRegister PC) newPC

execute (ConditionalAbsoluteJump flag target) = _wC
execute (RelativeJump target) = do
    off <- load target
    currentPC <- load (WideRegister PC)

    let w16Off = fromIntegral off
        newPC = currentPC + w16Off

    store (WideRegister PC) newPC

execute (ConditionalRelativeJump flag target) = _wE
execute (Call target) = _wF
execute (ConditionalCall flag target) = _wG
execute Return = _wH
execute (ConditionalReturn flag) = _wI
execute ReturnAndEnableInterrupts = _wJ
execute (Reset target) = _wK

execute CBPrefix = executeCB . decodeCBInstruction =<< load ImmediateWord8
execute InvalidInstruction = _wM

executeCB :: CBInstruction -> Karaa ()
executeCB (RotateLeft dst usesCarry) = _wR
executeCB (RotateRight dst usesCarry) = _wS
executeCB (ArithmeticShiftLeft dst) = _wT
executeCB (ArithmeticShiftRight dst) = _wU
executeCB (LogicalShiftRight dst) = _wV

executeCB (SwapNibble dst) = bitwiseOperation rotateR dst (pure 4)

executeCB (TestBit bib op) = do
    a <- load op

    store zeroFlag        (testBit a $ bitInByte bib)
    store subtractionFlag False
    store halfCarryFlag   True

executeCB (SetBit bib dst)   = store dst . flip setBit   (bitInByte bib) =<< load dst 
executeCB (ResetBit bib dst) = store dst . flip clearBit (bitInByte bib) =<< load dst 

--

bytesToWord16 :: Word8 -> Word8 -> Word16
bytesToWord16 little big = fromIntegral little .|. (fromIntegral big `shiftL` 8)

w16PlusI8WithCarry :: Word16 -> Int8 -> (Word16, Bool, Bool)
w16PlusI8WithCarry w16 i8 = (out, halfCarry, carry)
    where
        i8AsW16 = fromIntegral i8
        (outLower, _, halfCarry, carry) = arithWithCarry (+) (w16 ^. lower) (i8AsW16 ^. lower) False
        (outUpper, _, _,         _)     = arithWithCarry (+) (w16 ^. upper) (i8AsW16 ^. upper) carry
        out = bytesToWord16 outLower outUpper               

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

bitwiseOperation :: (Bits a, Num a) => (a -> b -> a) -> Operand 'RW a -> Karaa b -> Karaa ()
bitwiseOperation op dst src = do
    a <- load dst
    b <- src

    let out = a `op` b

    store zeroFlag        (out == 0)
    store subtractionFlag False
    store halfCarryFlag   False
    store carryFlag       False
    store dst             out
