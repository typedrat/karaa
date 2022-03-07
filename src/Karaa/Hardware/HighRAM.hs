module Karaa.Hardware.HighRAM ( HighRAM(), makeHighRAM, HasHighRAM(..)
                              , readHighRAM, writeHighRAM
                              ) where

import Control.Applicative       ( empty )
import Control.Lens.Combinators  ( use )
import Control.Lens.Lens         ( Lens' )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Word                 ( Word8, Word16 )

import Karaa.Core.Types.Memory   ( RAM, MonadRAM(..) )

newtype HighRAM = HighRAM RAM
                deriving (Show)

makeHighRAM :: (MonadRAM m) => m HighRAM
makeHighRAM = HighRAM <$> newRAM 0x80

class HasHighRAM s where
    highRAM :: Lens' s HighRAM

instance HasHighRAM HighRAM where
    highRAM = id
    {-# INLINE highRAM #-}

--

readHighRAM :: (MonadState s m, HasHighRAM s, MonadRAM m) => Word16 -> MaybeT m Word8
readHighRAM addr 
    | addr >= 0xFF80, addr <= 0xFFFE = use highRAM >>= \case
        HighRAM ram -> readRAM ram (addr - 0xFF80)
    | otherwise                      = empty
{-# INLINE readHighRAM #-}

writeHighRAM :: (MonadState s m, HasHighRAM s, MonadRAM m) => Word16 -> Word8 -> m ()
writeHighRAM addr byte
    | addr >= 0xFF80, addr <= 0xFFFE = use highRAM >>= \case
        HighRAM ram -> writeRAM ram (addr - 0xFF80) byte
    | otherwise                      = return ()
{-# INLINE writeHighRAM #-}
