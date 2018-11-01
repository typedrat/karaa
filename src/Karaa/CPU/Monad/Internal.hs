{-# LANGUAGE TemplateHaskell #-}
module Karaa.CPU.Monad.Internal
    ( CPU(..), runCPU, ioToCPU
    , CPUState, registers, memoryMap
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.IORef
import Lens.Micro
import Lens.Micro.TH

import Karaa.App.Config
import Karaa.CPU.Registers
import Karaa.RAM.Types

data CPUState = CPUState
              { _registers :: Registers
              , _memoryMap :: MemoryMap
              }

makeLenses ''CPUState

initialCPUState :: KaraaConfig -> CPUState
initialCPUState _ = CPUState initialRegisters undefined

newtype CPU a = CPU { _unCPU :: ReaderT (KaraaConfig, IORef CPUState) IO a }
              deriving (Functor, Applicative, Monad)

instance MonadReader KaraaConfig CPU where
    ask = CPU $ do
        (conf, _) <- ask
        return conf
    local f (CPU m) = CPU $ local f' m
        where
            f' (a, b) = (f a, b)

instance MonadState CPUState CPU where
    get = CPU $ do
        (_, ref) <- ask
        liftIO $ readIORef ref
    put st = CPU $ do
        (_, ref) <- ask
        liftIO $ writeIORef ref st

runCPU :: KaraaConfig -> CPU a -> IO a
runCPU conf (CPU m) = do
    ref <- newIORef (initialCPUState conf)
    runReaderT m (conf, ref)

ioToCPU :: IO a -> CPU a
ioToCPU = CPU . liftIO
