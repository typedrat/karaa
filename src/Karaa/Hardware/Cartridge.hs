module Karaa.Hardware.Cartridge ( Cartridge()
                                , CartridgeLoadError(..)
                                , loadCartridgeFromByteString
                                ) where

import           Control.Monad                    ( forever )
import qualified Data.ByteString                  as BS

import           Karaa.Hardware.Cartridge.Header
import           Karaa.Hardware.Cartridge.Mappers
import           Karaa.Types.Hardware             ( Hardware(..), HardwareStatus(..) )
import           Karaa.Types.Scheduled            ( statefulYield )

data Cartridge = ROMOnlyCartridge {-# UNPACK #-} !ROMOnlyCartridge
               | MBC1Cartridge    {-# UNPACK #-} !MBC1Cartridge
               deriving (Show)

--

instance Hardware Cartridge where
    readHardware (ROMOnlyCartridge cart) =
        readROMOnlyCartridge cart
    readHardware (MBC1Cartridge cart) =
        readMBC1Cartridge cart

    writeHardware cart@(ROMOnlyCartridge _) _ _ =
        return (cart, Nothing)
    writeHardware (MBC1Cartridge mbc1) addr byte = do
        mbc1' <- writeMBC1Cartridge mbc1 addr byte
        return (MBC1Cartridge mbc1', Nothing)

    -- Why is this using the 'Hardware' interface? Because MBC3s with RTC in
    -- honest RTC emulation mode will need it.
    emulateHardware = forever $
        statefulYield $ \cart ->
            return (Disabled, cart)

--

-- | Errors that can occur when we try to load a cartridge ROM/save RAM.
data CartridgeLoadError = InvalidHeader                  -- ^ The cartridge header is invalid.
                        | UnsupportedMapper              -- ^ We do not (yet!) support emulating cartridges using this mapper.
                        | UnsupportedMapperConfiguration -- ^ The mapper is supported, but the configuration isn't.

loadCartridgeFromByteString :: BS.ByteString                       -- ^ A bytestring containing the contents of the ROM file that we're loading
                            -> Maybe BS.ByteString                 -- ^ If 'Just', a bytestring containing the contents of the ROM's associated RAM
                            -> Either CartridgeLoadError Cartridge
loadCartridgeFromByteString rom mRAM = do
    header@CartridgeHeader { mapperInfo } <- maybeToEither InvalidHeader $ parseHeader rom
    
    case mapperInfo of
        ROMOnly  -> maybeToEither UnsupportedMapperConfiguration $
            ROMOnlyCartridge <$> makeROMOnlyCartridge header rom
        MBC1 _ _ -> maybeToEither UnsupportedMapperConfiguration $
            MBC1Cartridge <$> makeMBC1Cartridge header rom mRAM
        _        -> Left UnsupportedMapper

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l Nothing  = Left l
maybeToEither _ (Just r) = Right r
