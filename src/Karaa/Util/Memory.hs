module Karaa.Util.Memory ( ROM(), romSize, rawROMSize, romFromByteString, readROM, rawReadROM
                         , RAM(), ramSize, rawRAMSize, MonadRAM(..), readRAM, writeRAM
                         , Banked(..), readBankedROM, readBankedRAM, writeBankedRAM
                         ) where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BSI
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word                    ( Word8, Word16 )

import           Karaa.Util.WithMonadIO       ( MonadIO(..), WithMonadIO )       

--

newtype ROM = ROM (V.Vector Word8)

instance Show ROM where
    show (ROM v) = "ROM of size " ++ show (V.length v)

romSize :: ROM -> Word16
romSize = fromIntegral . rawROMSize

rawROMSize :: ROM -> Int
rawROMSize (ROM v) = V.length v

romFromByteString :: BS.ByteString -> ROM
romFromByteString bs = ROM romVector
    where (fptr, len) = BSI.toForeignPtr0 bs
          romVector = V.unsafeFromForeignPtr0 fptr len
 
readROM :: ROM -> Word16 -> Word8
readROM rom = rawReadROM rom . fromIntegral

rawReadROM :: ROM -> Int -> Word8
rawReadROM (ROM romVector) = (romVector V.!)

--

newtype RAM = RAM (MV.IOVector Word8)

instance Show RAM where
    show (RAM v) = "RAM of size " ++ show (MV.length v)

ramSize :: RAM -> Word16
ramSize = fromIntegral . rawRAMSize

rawRAMSize :: RAM -> Int
rawRAMSize (RAM v) = MV.length v

class (Monad m) => MonadRAM m where
    newRAM :: Int -> m RAM
    rawReadRAM :: RAM -> Int -> m Word8
    rawWriteRAM :: RAM -> Int -> Word8 -> m ()

instance (MonadIO m) => MonadRAM (WithMonadIO m) where
    newRAM = liftIO . fmap RAM . MV.new
    {-# INLINE newRAM #-}

    rawReadRAM (RAM v) addr = liftIO $ MV.read v addr  
    {-# INLINE rawReadRAM#-}
    
    rawWriteRAM (RAM v) addr byte = liftIO $ MV.write v addr byte
    {-# INLINE rawWriteRAM #-}

readRAM :: (MonadRAM m) => RAM -> Word16 -> m Word8 
readRAM ram addr = rawReadRAM ram (fromIntegral addr)
{-# INLINE readRAM #-}

writeRAM :: (MonadRAM m) => RAM -> Word16 -> Word8 -> m ()
writeRAM ram addr = rawWriteRAM ram (fromIntegral addr)
{-# INLINE writeRAM #-}

--

data Banked a = Banked { bankedMemory :: a, bankSize :: Int }
              deriving (Show, Eq)

wrapBanked :: (a -> Int -> r) -> Banked a -> Int -> Word16 -> r
wrapBanked f (Banked mem size) bank addr = f mem rawAddr
    where
        addr' = fromIntegral addr
        rawAddr = bank * size + addr'

readBankedROM :: Banked ROM -> Int -> Word16 -> Word8
readBankedROM = wrapBanked rawReadROM

readBankedRAM :: (MonadRAM m) => Banked RAM -> Int -> Word16 -> m Word8
readBankedRAM = wrapBanked rawReadRAM
{-# INLINE readBankedRAM #-}

writeBankedRAM :: (MonadRAM m) => Banked RAM -> Int -> Word16 -> Word8 -> m ()
writeBankedRAM = wrapBanked rawWriteRAM
{-# INLINE writeBankedRAM #-}
