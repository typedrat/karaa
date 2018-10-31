module Karaa.CPU.Monad.Internal
    ( CPU(..), runCPU
    , CPUState
    , registers
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.IORef
import Lens.Micro

import Karaa.App.Config
import Karaa.CPU.Registers
import Karaa.RAM.Types

data CPUState = CPUState
              { _registers :: Registers
              , _memoryMap :: MemoryMap
              }

initialCPUState :: CPUState
initialCPUState = CPUState initialRegisters

registers :: Lens' CPUState Registers
registers = lens _registers (\cs _registers -> cs { _registers })

newtype CPU a = CPU { _unCPU :: ReaderT (KaraaConfig, IORef CPUState) IO a }
              deriving (Functor, Applicative, Monad)

instance MonadReader KaraaConfig CPU where
    ask = CPU $ fmap fst . ask

instance MonadState CPUState CPU where
    get = CPU $ do
        ref <- ask
        liftIO $ readIORef ref
    put st = CPU $ do
        ref <- ask
        liftIO $ writeIORef ref st

runCPU :: CPU a -> IO a
runCPU (CPU m) = do
    ref <- newIORef initialCPUState
    runReaderT m ref