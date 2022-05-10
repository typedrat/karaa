module Karaa.Hardware.WorkRAM ( WorkRAM(), makeWorkRAM
                              , readWorkRAM, writeWorkRAM
                              ) where

import Control.Applicative       ( empty )
import Data.Word                 ( Word8, Word16 )

import Karaa.Core.Monad.Base     ( KaraaBase )
import Karaa.Types.MaybeT        ( MaybeT )
import Karaa.Types.Memory        ( RAM, MonadRAM(..) )

newtype WorkRAM = DMGWorkRAM RAM
                deriving (Show)

makeWorkRAM :: (MonadRAM m) => m WorkRAM
makeWorkRAM = DMGWorkRAM <$> newRAM 0x2000

--

readWorkRAM :: WorkRAM -> Word16 -> MaybeT KaraaBase Word8
readWorkRAM wram addr 
    -- Echo RAM:
    | addr >= 0xE000, addr <= 0xFDFF = {-# SCC readWorkRAM #-}
        readWorkRAM wram (addr - 0x2000)
    -- Directly addressed work RAM.
    | addr >= 0xC000, addr <= 0xDFFF = {-# SCC readWorkRAM #-}
        case wram of
            DMGWorkRAM ram -> readRAM ram (addr - 0xC000)
    | otherwise =
        empty
{-# INLINE readWorkRAM #-}

writeWorkRAM :: WorkRAM -> Word16 -> Word8 -> KaraaBase ()
writeWorkRAM wram addr byte
    -- Echo RAM:
    | addr >= 0xE000, addr <= 0xFDFF = {-# SCC writeWorkRAM #-}
        writeWorkRAM wram (addr - 0x2000) byte
    -- Directly addressed work RAM.
    | addr >= 0xC000, addr <= 0xDFFF = {-# SCC writeWorkRAM #-}
        case wram of
            (DMGWorkRAM ram) -> writeRAM ram (addr - 0xC000) byte
    | otherwise =
        return ()
{-# INLINE writeWorkRAM #-}
