{-# LANGUAGE TemplateHaskell #-}
module Karaa.Monad.Config 
    ( GameboyMode(..)
    , GBCMode(..)
    , KaraaConfig
    , gameboyMode, cartROMPath, cartRAMPath
    ) where

import Lens.Micro.TH

data GameboyMode = DMG | MGB | GBC GBCMode
                 deriving (Eq, Show)

data GBCMode = DMGGame | GBCGame
             deriving (Eq, Show)

data KaraaConfig = KaraaConfig
                 { _gameboyMode :: GameboyMode
                 , _cartROMPath :: FilePath
                 , _cartRAMPath :: Maybe FilePath
                 }
                 deriving (Eq, Show)

makeLenses ''KaraaConfig
