{-|
Module: Karaa.Core.Types.Memory

Types used to represent emulated ROM, RAM, and banked addressing modes thereof.
-}
module Karaa.Core.Types.Memory ( -- * ROM
                                 ROM(), romSize, romFromByteString, readROM, rawReadROM
                                 -- * RAM
                               , RAM(), ramSize, MonadRAM(..)
                                 -- * Banked memory
                               , Banked(), bankSize, bankCount, bankedROM, readBankedROM, bankedRAM, readBankedRAM, writeBankedRAM
                               ) where

import           Control.Monad.IO.Class       ( MonadIO(..) )
import           Control.Monad.Trans          ( MonadTrans(..) )
import           Control.Monad.Trans.Maybe    ( MaybeT )
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BSI
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word                    ( Word8, Word16 )

import           Karaa.Util.Hex               ( showHex )
import           Karaa.Core.Types.WithMonadIO ( WithMonadIO(..) )  

--

-- | An opaque type wrapping the underlying 'V.Vector' that is used to represent ROM in Karaa.
newtype ROM = ROM (V.Vector Word8)

instance Show ROM where
    showsPrec p (ROM v) = showParen (p > 10) $ showString "ROM of size " . showsPrec 11 (V.length v)

-- | Get the size of the ROM in bytes.
romSize :: ROM -> Int
romSize (ROM v) = V.length v

-- | Create a 'ROM' from a 'BS.ByteString' that contains the desired ROM contents.
romFromByteString :: BS.ByteString -> ROM
romFromByteString bs = ROM romVector
    where (fptr, off, len) = BSI.toForeignPtr bs
          romVector = V.unsafeFromForeignPtr fptr off len

-- | @readROM rom addr@ reads the byte at @addr@ from the RAM.
readROM :: ROM -> Word16 -> Word8
readROM (ROM romVector) = (romVector V.!) . fromIntegral

-- | @rawReadROM rom addr@ reads the byte at @addr@ from the ROM. Note that this function does not perform any bounds checking.
rawReadROM :: ROM -> Int -> Word8
rawReadROM (ROM romVector) = V.unsafeIndex romVector

--

-- | An opaque type wrapping the underlying 'MV.IOVector' that is used to represent RAM in Karaa.
newtype RAM = RAM (MV.IOVector Word8)

instance Show RAM where
    showsPrec p (RAM v) = showParen (p > 10) $ showString "RAM of size " . showsPrec 11 (MV.length v)

-- | Get the size of the RAM in bytes.
ramSize :: RAM -> Int
ramSize (RAM v) = MV.length v

-- | This class exists to abstract away the underlying details of how RAM access is implemented, largely so that we can use RAMs
--   in functions that we do not want to allow to access the full power of 'Control.Monad.IO.Class.MonadIO', which this is
--   effectively a constraint synonym for.
class (Monad m) => MonadRAM m where
    -- | Create a 'RAM' of the given size in bytes. It will be filled with zeroes.
    newRAM :: Int -> m RAM
    -- | @readRAM ram addr@ reads the byte at @addr@ from the RAM. 
    readRAM :: RAM -> Word16 -> m Word8
    -- | @writeRAM ram addr byte@ writes the byte @byte@ to @addr@ in the RAM.
    writeRAM :: RAM -> Word16 -> Word8 -> m ()
    -- | @rawReadRAM ram addr@ reads the byte at @addr@ from the RAM. Note that this function does not perform any bounds checking. 
    rawReadRAM :: RAM -> Int -> m Word8
    -- | @rawWriteRAM ram addr byte@ writes the byte @byte@ to @addr@ in the RAM. Note that this function does not perform any
    --   bounds checking.
    rawWriteRAM :: RAM -> Int -> Word8 -> m ()

instance (MonadIO m) => MonadRAM (WithMonadIO m) where
    newRAM = liftIO . fmap RAM . MV.new
    {-# INLINE newRAM #-}

    readRAM (RAM v) addr = liftIO $ MV.read v (fromIntegral addr)
    {-# INLINE readRAM #-}

    writeRAM (RAM v) addr byte = liftIO $ MV.write v (fromIntegral addr) byte 
    {-# INLINE writeRAM #-}

    rawReadRAM (RAM v) addr = liftIO $ MV.unsafeRead v addr  
    {-# INLINE rawReadRAM#-}
    
    rawWriteRAM (RAM v) addr byte = liftIO $ MV.unsafeWrite v addr byte
    {-# INLINE rawWriteRAM #-}

deriving via WithMonadIO IO instance MonadRAM IO

instance (MonadRAM m) => MonadRAM (MaybeT m) where
    newRAM size = lift $ newRAM size
    {-# INLINE newRAM #-}

    readRAM ram addr = lift $ readRAM ram addr
    {-# INLINE readRAM #-}

    writeRAM ram addr byte = lift $ writeRAM ram addr byte 
    {-# INLINE writeRAM #-}

    rawReadRAM ram addr = lift $ rawReadRAM ram addr  
    {-# INLINE rawReadRAM#-}
    
    rawWriteRAM ram addr byte = lift $ rawWriteRAM ram addr byte
    {-# INLINE rawWriteRAM #-}

--

-- | A wrapper type to simplify access to banked memory by storing the bank size and count along with the underlying memory.
data Banked a = Banked { bankedMemory :: !a
                       , bankSize :: !Int  -- ^ Get the size of the banks in this banked view.
                       , bankCount :: !Int -- ^ Get the number of banks in this banked view.
                       }
              deriving (Show)

-- | Create a 'Banked' wrapper to access the given 'ROM' in banks of the given size.
bankedROM :: ROM -> Int -> Banked ROM
bankedROM rom size
    | leftovers /= 0 = error $ concat ["Bank size ", show size, " does not evenly partition ROM size ", show $ romSize rom]
    | otherwise = Banked rom size count
    where
        (count, leftovers) = romSize rom `divMod` size

-- | Create a 'Banked' wrapper to access the given 'RAM' in banks of the given size.
bankedRAM :: RAM -> Int -> Banked RAM
bankedRAM ram size
    | leftovers /= 0 = error $ concat ["Bank size ", show size, " does not evenly partition RAM size ", show $ ramSize ram]
    | otherwise = Banked ram size count
    where
        (count, leftovers) = ramSize ram `divMod` size

wrapBanked :: (a -> Int -> r) -> Banked a -> Int -> Word16 -> r
wrapBanked f (Banked mem size count) bank addr 
    | addr' > size = error $ concat ["Address ", showHex addr, " is larger than bank size ", showHex size]
    | otherwise    = f mem rawAddr
    where
        bank' = bank `mod` count
        addr' = fromIntegral addr
        rawAddr = bank' * size + addr'

-- | @readBankedROM rom bank addr@ reads the value at address @addr@ in bank @bank@.
readBankedROM :: Banked ROM -> Int -> Word16 -> Word8
readBankedROM = wrapBanked rawReadROM

-- | @readBankedRAM ram bank addr@ reads the value at address @addr@ in bank @bank@.
readBankedRAM :: (MonadRAM m) => Banked RAM -> Int -> Word16 -> m Word8
readBankedRAM = wrapBanked rawReadRAM
{-# INLINE readBankedRAM #-}

-- | @writeBankedRAM ram bank addr byte@ writes @byte@ to address @addr@ in bank @bank@.
writeBankedRAM :: (MonadRAM m) => Banked RAM -> Int -> Word16 -> Word8 -> m ()
writeBankedRAM = wrapBanked rawWriteRAM
{-# INLINE writeBankedRAM #-}
