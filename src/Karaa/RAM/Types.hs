module Karaa.RAM.Types 
    ( Memory
    , MBC(..)
    , VRAM(..)
    , WRAM(..)
    , OAM(..)
    , IORegs(..)
    , HRAM(..)
    , InterruptEnable(..)
    , MemoryMap(..)
    ) where

import qualified Data.Vector.Unboxed.Mutable as MV

import Data.IORef
import Data.Word

type Memory = MV.IOVector Word8

data MBC = NoMBC { _mbcROM :: Memory, _mbcRAM :: Memory }

data VRAM = VRAM { _vramRAM :: Memory, _vramBank :: Int }

data WRAM = WRAP { _wramRAM :: Memory, _wramBank :: Int }

data OAM = OAM

data IORegs = IORegs

newtype HRAM = HRAM { _hramRAM :: Memory }

newtype InterruptEnable = IE { _unIE :: IORef Word8 }

data MemoryMap = MemoryMap
               { _cartridge :: MBC
               , _vram :: VRAM
               , _wram :: WRAM
               , _oam :: OAM
               , _ioRegs :: IORegs
               , _hram :: HRAM
               , _ieReg :: InterruptEnable
               }
