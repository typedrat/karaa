module Karaa.Hardware.Cartridge.Mappers.ROMOnly ( ROMOnlyCartridge(), makeROMOnlyCartridge, readROMOnlyCartridge ) where

import           Control.Applicative                   ( Alternative(..) )
import qualified Data.ByteString                       as BS
import           Data.Word                             ( Word8, Word16 )

import           Karaa.Hardware.Cartridge.Header.Types
import           Karaa.Types.MaybeT
import           Karaa.Types.Memory

newtype ROMOnlyCartridge = ROMOnlyCartridge ROM
                         deriving (Show)

makeROMOnlyCartridge :: CartridgeHeader -> BS.ByteString -> Maybe ROMOnlyCartridge
makeROMOnlyCartridge (CartridgeHeader { mapperInfo = ROMOnly
                                      , cartridgeROMSize = 32768
                                      , cartridgeRAMSize = 0
                                      })
                     bs
                     | BS.length bs == 32768 = Just $ ROMOnlyCartridge (romFromByteString bs)
makeROMOnlyCartridge _ _ = Nothing

readROMOnlyCartridge :: ROMOnlyCartridge -> Word16 -> MaybeT m Word8
readROMOnlyCartridge (ROMOnlyCartridge rom) addr
    | addr <= 0x7FFF = {-# SCC readROMOnlyCartridge #-}
                       pure (readROM rom addr)
readROMOnlyCartridge _ _ = empty
