{-# LANGUAGE TemplateHaskell #-}
module Karaa.App.Config 
    ( GameboyMode(..)
    , KaraaConfig
    , gameboyMode, romPath
    ) where

import Lens.Micro.TH

data GameboyMode = DMG | GBCWithDMGGame | GBCWithGBCGame
                 deriving (Eq, Show)

data KaraaConfig = KaraaConfig
                 { _gameboyMode :: GameboyMode
                 , _romPath :: FilePath
                 }
                 deriving (Eq, Show)

makeLenses ''KaraaConfig
