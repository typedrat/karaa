module Karaa.Hardware.HighRAM ( HighRAM(), makeHighRAM
                              , readHighRAM, writeHighRAM
                              ) where

import Control.Applicative       ( empty )
import Data.Word                 ( Word8, Word16 )

import Karaa.Types.MaybeT        ( MaybeT )
import Karaa.Types.Memory        ( RAM, MonadRAM(..) )
import Karaa.Core.Monad.Base (KaraaBase)

newtype HighRAM = HighRAM RAM
                deriving (Show)

makeHighRAM :: (MonadRAM m) => m HighRAM
makeHighRAM = HighRAM <$> newRAM 0x80

--

readHighRAM :: HighRAM -> Word16 -> MaybeT KaraaBase Word8
readHighRAM (HighRAM ram) addr 
    | addr >= 0xFF80, addr <= 0xFFFE = {-# SCC readHighRAM #-}
        readRAM ram (addr - 0xFF80)
    | otherwise =
        empty
{-# INLINE readHighRAM #-}

writeHighRAM :: HighRAM -> Word16 -> Word8 -> KaraaBase ()
writeHighRAM (HighRAM ram) addr byte
    | addr >= 0xFF80, addr <= 0xFFFE = {-# SCC writeHighRAM #-}
        writeRAM ram (addr - 0xFF80) byte
    | otherwise =
        return ()
{-# INLINE writeHighRAM #-}
