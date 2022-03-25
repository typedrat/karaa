module Main where

import           Control.Lens                  hiding ( op )
import           Control.Monad                 ( void )
import           Control.Monad.IO.Class        ( liftIO )
import           Control.Monad.State.Strict    ( StateT(..), get )
import           Control.Monad.Trans           ( lift )
import qualified Data.ByteString               as BS
import           Data.Word                     ( Word8, Word16 )
import           Text.Read                     ( readMaybe )
import           Text.Pretty.Simple            ( pPrint )
import           System.Console.Repline
import           System.Environment            ( getArgs, getProgName )
import           System.Exit                   ( exitFailure )
import           System.FilePath               ( takeFileName, (-<.>) )
import           System.IO

import           Karaa.Core.Monad
import           Karaa.CPU
import           Karaa.CPU.Instructions.Decode
import           Karaa.CPU.Registers
import           Karaa.CPU.State
import           Karaa.Hardware.Cartridge
import           Karaa.Hardware.HighRAM
import           Karaa.Hardware.Serial         ( makeSerialPort, putCharSerialCallback )
import           Karaa.Hardware.State
import           Karaa.Hardware.Timer
import           Karaa.Hardware.WorkRAM
import           Karaa.Util.Hex

type KaraaREPL = HaskelineT (StateT (EmulatorState, Word8) IO)

main :: IO ()
main = forceDecoderTables
    >> getArgs >>= \case
        [path] -> runRepl path
        _ -> do
            name <- getProgName
            putStrLn $ "USAGE: " ++ name ++ " ROM_PATH"
            exitFailure

runRepl :: FilePath -> IO ()
runRepl path = do
    cartBS <- BS.readFile path

    let logPath = takeFileName path -<.> "txt"
    logFile <- openFile logPath WriteMode

    hSetBuffering stdout NoBuffering

    case loadCartridgeFromByteString cartBS Nothing of
        Left InvalidHeader                  -> putStrLn "The cartridge header is invalid."
        Left UnsupportedMapper              -> putStrLn "We do not (yet!) support emulating cartridges using this mapper."
        Left UnsupportedMapperConfiguration -> putStrLn "The mapper is supported, but the configuration isn't."
        Right cart -> void $ do
            hram <- makeHighRAM
            wram <- makeWorkRAM
            
            let serialPort = makeSerialPort putCharSerialCallback
                hwState = HardwareState cart hram serialPort initialTimerState wram
                emuState = EmulatorState initialCPUState hwState

            flip runStateT (emuState, 0x00) $ do
                evalRepl prompt (commands logFile) monitorOptions optionPrefix Nothing (Word $ \_ -> return []) (return ()) (return Exit)

    hClose logFile

prompt :: MultiLine -> KaraaREPL String
prompt _ = pure "γβ> "

commands :: Handle -> String -> KaraaREPL ()
commands logFile cmd = do
    case words cmd of
        ["read", readMaybe -> Just addr] -> commandWrapper $ readCommand addr 1
        ["read", readMaybe -> Just addr
            , readMaybe -> Just off    ]  -> commandWrapper $ readCommand addr off
        ["regs"]  -> commandWrapper regsCommand
        ["state"] -> commandWrapper stateCommand
        ["step"]      -> commandWithNextOpcode (stepCommand $ Just stdout)
        ["tracestep"] -> commandWithNextOpcode (stepCommand $ Just logFile)
        ["run"]       -> commandWithNextOpcode (runCommand   Nothing)
        ["tracerun"]  -> commandWithNextOpcode (runCommand $ Just logFile)
        ["run",      readMaybe -> Just addr] -> commandWithNextOpcode (runUntilCommand Nothing        addr)
        ["tracerun", readMaybe -> Just addr] -> commandWithNextOpcode (runUntilCommand (Just logFile) addr)
        [] -> return ()
        _  -> liftIO $ putStrLn ("Unknown command: " ++ cmd)
    
    liftIO $ hFlush logFile

commandWrapper :: Karaa a -> KaraaREPL a
commandWrapper = lift . zoom _1 . runKaraa

commandWithNextOpcode :: (Word8 -> Karaa Word8) -> KaraaREPL ()
commandWithNextOpcode cmd = do
    opcode <- use _2
    nextOpcode <- commandWrapper (cmd opcode)
    assign _2 nextOpcode

readCommand :: Word16 -> Int -> Karaa ()
readCommand base off
    | off > 8   = readCommand base       8
               >> readCommand (base + 8) (off - 8)
    | otherwise = liftIO . putStrLn . bytes =<< mapM readAddr (addrRange base off)
    where bytes = unwords . fmap showHex

regsCommand :: Karaa ()
regsCommand = logStep stdout

stateCommand :: Karaa ()
stateCommand = do
    state <- get
    let state' = state & wideRegister PC -~ 1
    liftIO $ pPrint state'

logStep :: Handle -> Karaa ()
logStep logFile = do
    regs <- use registerFile
    let regs' = regs & wideRegister PC -~ 1
        regDumps = (\r -> show r ++ ": " ++ showHex (regs' ^. register r)) <$> [A, F, B, C, D, E, H, L]
        wideRegDumps = (\wr -> show wr ++ ": " ++ showHex (regs' ^. wideRegister wr)) <$>  [SP, PC]
        pc = regs' ^. wideRegister PC
    bytes <- mapM (fmap showHex . readAddr) [pc .. pc + 3]
    let bytesDump = " (" ++ unwords bytes ++ ")"

    liftIO $ hPutStrLn logFile (unwords (regDumps ++ wideRegDumps) ++ bytesDump)

stepCommand :: Maybe Handle -> Word8 -> Karaa Word8
stepCommand logFile opcode = do
    maybe (return ()) logStep logFile
    cpuStep opcode

runCommand :: Maybe Handle -> Word8 -> Karaa Word8
runCommand logFile = go
    where go op   = stepCommand logFile op >>= go

runUntilCommand :: Maybe Handle -> Word16 -> Word8 -> Karaa Word8
runUntilCommand logFile breakAddr = go
    where
        go op   = do
            pc <- use (wideRegister PC)

            if (pc - 1) == breakAddr
                then return op
                else stepCommand logFile op >>= go

addrRange :: Word16 -> Int -> [Word16]
addrRange _base 0   = []
addrRange base  off = [base, base + direction .. end]
    where direction = fromIntegral (signum off)
          end = fromIntegral (fromIntegral base + off - 1)

monitorOptions :: Options KaraaREPL
monitorOptions = []

optionPrefix :: Maybe Char
optionPrefix = Nothing
