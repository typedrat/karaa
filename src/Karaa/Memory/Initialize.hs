module Karaa.Memory.Initialize ( newMemoryMap ) where

import Lens.Micro

import Karaa.Memory.BootROM
import Karaa.Memory.Cartridge
import Karaa.Memory.Types
import Karaa.Monad.Config
import Karaa.Monad.Error

newMemoryMap :: KaraaConfig -> IO (Either KaraaError MemoryMap)
newMemoryMap c = do
    let 
        cartROM = c ^. cartROMPath
        cartRAM = c ^. cartRAMPath
        gbMode  = c ^. gameboyMode
    
    bootrom <- getBootROM gbMode
    cartridge <- readCartridge cartROM cartRAM
    vram <- VRAM <$> createMemory <*> pure 0
    wram <- WRAM <$> createMemory <*> pure 0
    oam  <- OAM  <$> createMemory
    ioregs <- newIORegisters gbMode
    hram <- HRAM <$> createMemory

    case bootrom of 
        Just br ->
            return $ Right (MemoryMap br cartridge vram wram oam ioregs hram)
        Nothing -> 
            return $ Left (BootROMNotFound gbMode)

newIORegisters :: GameboyMode -> IO IORegs
newIORegisters mode = do
    waveMem <- createMemory

    let
        lcd = LCDController 0x00 0x00 0x00 0x00
                            0x00 0x00 0x00 0x00
                            0x00 0x00 0x00 0x00
                            0x00 0x00 0x00
        dma = DMAController 0x00 0x00 0x00 0x00
        snd = SoundController 0x00 0x00 0x00 0x00
                              0x00 0x00 0x00 0x00
                              0x00 0x00 0x00 0x00
                              0x00 0x00 0x00 0x00
                              0x00 0x00 waveMem
        joy = JoypadRegister 0x00
        ser = SerialController 0x00 0x00
        tim = TimerController 0x00 0x00 0x00 0x00
        int = InterruptController 0x00 0x00
        cgb = CGBRegisters 0x00 0x00 0x00 0x00
                           0x00 0x00 0x00 0x00
                           0x00
    return (IORegs Nothing lcd dma snd joy ser tim int cgb)