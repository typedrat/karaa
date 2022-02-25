module Karaa.Util.WithMonadIO ( WithMonadIO(..), MonadIO(..) ) where

import Control.Monad.IO.Class ( MonadIO(..) )

newtype WithMonadIO m a = WithMonadIO (m a)
                        deriving (Functor, Applicative, Monad, MonadIO)
