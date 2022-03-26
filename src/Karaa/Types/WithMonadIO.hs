module Karaa.Types.WithMonadIO ( WithMonadIO(..) ) where

import Control.Monad.IO.Class ( MonadIO(..) )

-- | A simple @newtype@ wrapper intended for use with @DerivingVia@.
newtype WithMonadIO m a = WithMonadIO (m a)
                        deriving (Functor, Applicative, Monad, MonadIO)
