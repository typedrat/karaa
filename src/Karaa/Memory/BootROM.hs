{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Karaa.Memory.BootROM ( hasBootROM, getBootROM ) where

import Control.Arrow 
    ( (&&&) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA
    ( sha1, showDigest )
import Data.FileEmbed
    ( embedDir )
import Data.Maybe
    ( isJust )

import Karaa.Monad.Config
    ( GameboyMode(..), GBCMode(GBCGame) )
import Karaa.Memory.Types
    ( BootROM(..), memoryFromByteString, growMemory )

bootROMs :: [(String, BS.ByteString)]
bootROMs = (toHash &&& id) . snd <$> $(embedDir "data/bootroms")
    where toHash = showDigest . sha1 . BSL.fromStrict

bootROMHash :: GameboyMode -> String
bootROMHash  DMG    = "4ed31ec6b0b175bb109c0eb5fd3d193da823339f"
bootROMHash  MGB    = "4e68f9da03c310e84c523654b9026e51f26ce7f0"
bootROMHash (GBC _) = "1293d68bf9643bc4f36954c1e80e38f39864528d"

hasDMG, hasMGB, hasGBC :: Bool
hasDMG = isJust $ lookup (bootROMHash DMG)           bootROMs
hasMGB = isJust $ lookup (bootROMHash MGB)           bootROMs
hasGBC = isJust $ lookup (bootROMHash (GBC GBCGame)) bootROMs

hasBootROM :: GameboyMode -> Bool
hasBootROM DMG     = hasDMG
hasBootROM MGB     = hasMGB
hasBootROM (GBC _) = hasGBC

getBootROM :: GameboyMode -> IO (Maybe BootROM)
getBootROM mode =
        case lookup hash bootROMs of
            Just bs -> do
                mem <- fromBS bs
                return $ (\m -> BootROM m True) <$> mem

            Nothing -> return Nothing
    where
        fromBS bs
            | (GBC _) <- mode = memoryFromByteString bs
            | otherwise       = memoryFromByteString bs >>= \case
                Just mem -> Just <$> growMemory @0x200 @0x800 mem
                Nothing -> return Nothing
        hash = bootROMHash mode
