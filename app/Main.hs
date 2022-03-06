module Main where

import           Control.Lens
import           Control.Monad              ( void )
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict ( runStateT )
import qualified Data.ByteString            as BS
import           Data.Maybe                 ( fromMaybe )
import           Data.Word                  ( Word8 )

import           Karaa.Core.Monad
import           Karaa.CPU.Execution
import           Karaa.CPU.Instructions.Decode
import           Karaa.CPU.Instructions.Mnemonic
import           Karaa.CPU.Instructions.Operand
import           Karaa.CPU.LoadStore
import           Karaa.CPU.Registers
import           Karaa.CPU.State
import           Karaa.Hardware.Cartridge
import           Karaa.Hardware.Serial
import           Karaa.Hardware.State
import           Karaa.Hardware.WorkRAM

main :: IO ()
main = do
    cartBS <- BS.readFile "./testroms/blargg/cpu_instrs/cpu_instrs.gb"

    forceDecoderTables
    case loadCartridgeFromByteString cartBS Nothing of
        Left InvalidHeader                  -> putStrLn "The cartridge header is invalid."
        Left UnsupportedMapper              -> putStrLn "We do not (yet!) support emulating cartridges using this mapper."
        Left UnsupportedMapperConfiguration -> putStrLn "The mapper is supported, but the configuration isn't."
        Right cart -> void $ do
            wram <- makeWorkRAM
            let serialPort = makeSerialPort putCharSerialCallback
                hwState = HardwareState cart serialPort wram
                emuState = EmulatorState initialCPUState hwState
            
            flip runStateT emuState . runKaraa $ do
                firstInstruction <- loadByte (IndirectWithMode (WideRegister PC) PostIncrement)
                cpuLoop 100 firstInstruction

cpuLoop :: Int -> Word8 -> Karaa ()
cpuLoop 0 _      = return () 
cpuLoop n opcode = do
    let instruction = decodeInstruction opcode
        mnemonic = toMnemonic instruction
    regs <- use registerFile
    liftIO $ do
        putStr "Register file: "
        print regs
        putStr "Instruction: "
        case mnemonic of
            Just mnemonic' -> print mnemonic'
            Nothing        -> print instruction
    cpuLoop (n - 1) =<< execute instruction
