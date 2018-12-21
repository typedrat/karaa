{-# LANGUAGE TemplateHaskell #-}
module Karaa.Monad.Internal
    ( Karaa(..), runKaraa, ioToKaraa, tick
    , KaraaState, registers, memoryMap
    , KaraaError(..)
    ) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.IORef
import Lens.Micro
import Lens.Micro.TH

import Karaa.Monad.Config
import Karaa.Monad.Error
import Karaa.CPU.Registers
import Karaa.Memory.Initialize
import Karaa.Memory.Types

data KaraaState = KaraaState
              { _registers :: Registers
              , _memoryMap :: MemoryMap
              }

makeLenses ''KaraaState

newtype Karaa a = Karaa { _unKaraa :: ReaderT (KaraaConfig, IORef KaraaState) (ExceptT KaraaError IO) a }
              deriving (Functor, Applicative, Monad, MonadError KaraaError)

instance MonadReader KaraaConfig Karaa where
    ask = Karaa $ do
        (conf, _) <- ask
        return conf
    local f (Karaa m) = Karaa $ local f' m
        where
            f' (a, b) = (f a, b)

instance MonadState KaraaState Karaa where
    get = Karaa $ do
        (_, ref) <- ask
        liftIO $ readIORef ref
    put st = Karaa $ do
        (_, ref) <- ask
        liftIO $ writeIORef ref st

runKaraa :: KaraaConfig -> Karaa a -> IO (Either (Maybe KaraaState, KaraaError) a)
runKaraa conf (Karaa m) = do
    memoryMap <- newMemoryMap conf
    case memoryMap of
        Left err -> return (Left (Nothing, err))
        Right mmap -> do
            ref <- newIORef $ KaraaState initialRegisters mmap
            out <- runExceptT $ runReaderT m (conf, ref)

            case out of
                Left err -> do
                    state <- readIORef ref
                    return $ Left (Just state, err)
                Right a -> return (Right a)

ioToKaraa :: IO a -> Karaa a
ioToKaraa = Karaa . liftIO

tick :: Karaa a -> Karaa a
tick m = m
