{-# OPTIONS_HADDOCK not-home #-}
module Karaa.Types.HardwareStatus ( HardwareStatus(..) ) where

import Karaa.Types.Ticks ( DeltaT )

data HardwareStatus = Enabled  {-# UNPACK #-} !DeltaT
                    | Reset    {-# UNPACK #-} !DeltaT
                    | Disabled
                    deriving (Show, Eq)
