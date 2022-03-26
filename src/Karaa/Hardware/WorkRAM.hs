module Karaa.Hardware.WorkRAM ( WorkRAM(), makeWorkRAM, HasWorkRAM(..)
                              , readWorkRAM, writeWorkRAM
                              ) where

import Control.Applicative       ( empty )
import Control.Lens.Combinators  ( use )
import Control.Lens.Lens         ( Lens' )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Word                 ( Word8, Word16 )

import Karaa.Types.Memory        ( RAM, MonadRAM(..) )

newtype WorkRAM = DMGWorkRAM RAM
                deriving (Show)

makeWorkRAM :: (MonadRAM m) => m WorkRAM
makeWorkRAM = DMGWorkRAM <$> newRAM 0x2000

class HasWorkRAM s where
    workRAM :: Lens' s WorkRAM

instance HasWorkRAM WorkRAM where
    workRAM = id
    {-# INLINE workRAM #-}

--

readWorkRAM :: (MonadState s m, HasWorkRAM s, MonadRAM m) => Word16 -> MaybeT m Word8
readWorkRAM addr 
    -- Echo RAM:
    | addr >= 0xE000, addr <= 0xFDFF = readWorkRAM (addr - 0x2000)
    -- Directly addressed work RAM.
    | addr >= 0xC000, addr <= 0xDFFF = use workRAM >>= \case
        DMGWorkRAM ram -> readRAM ram (addr - 0xC000)
    | otherwise                      = empty
{-# INLINE readWorkRAM #-}

writeWorkRAM :: (MonadState s m, HasWorkRAM s, MonadRAM m) => Word16 -> Word8 -> m ()
writeWorkRAM addr byte
    -- Echo RAM:
    | addr >= 0xE000, addr <= 0xFDFF = writeWorkRAM (addr - 0x2000) byte
    -- Directly addressed work RAM.
    | addr >= 0xC000, addr <= 0xDFFF = use workRAM >>= \case
        DMGWorkRAM ram -> writeRAM ram (addr - 0xC000) byte
    | otherwise                      = return ()
{-# INLINE writeWorkRAM #-}
