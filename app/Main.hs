module Main where

import           Control.Lens
import           Control.Monad              ( mapM, void )
import           Control.Monad.Catch        ( handleIf )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.State.Strict ( runStateT )
import           Control.Monad.Trans        ( lift )
import qualified Data.ByteString            as BS
import           Data.List                  ( intercalate )
import           Data.Maybe                 ( fromMaybe )
import           Data.Word                  ( Word8, Word16 )
import           Text.Read                  ( readMaybe )
import           System.Console.Repline
import           System.Directory           ( removeFile )
import           System.Environment         ( getArgs, getProgName )
import           System.Exit                ( exitFailure )
import           System.FilePath            ( takeFileName, (-<.>) )
import           System.IO.Error            ( isDoesNotExistError )

import           Karaa.Core.Monad
import           Karaa.CPU
import           Karaa.CPU.Execution
import           Karaa.CPU.Instructions.Decode
import           Karaa.CPU.Instructions.Mnemonic
import           Karaa.CPU.Instructions.Operand
import           Karaa.CPU.LoadStore
import           Karaa.CPU.Registers
import           Karaa.CPU.State
import           Karaa.Hardware.Cartridge
import           Karaa.Hardware.HighRAM
import           Karaa.Hardware.Serial
import           Karaa.Hardware.State
import           Karaa.Hardware.WorkRAM
import           Karaa.Util.Hex

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
    handleIf isDoesNotExistError (\_ -> return ()) (removeFile logPath)

    case loadCartridgeFromByteString cartBS Nothing of
        Left InvalidHeader                  -> putStrLn "The cartridge header is invalid."
        Left UnsupportedMapper              -> putStrLn "We do not (yet!) support emulating cartridges using this mapper."
        Left UnsupportedMapperConfiguration -> putStrLn "The mapper is supported, but the configuration isn't."
        Right cart -> void $ do
            hram <- makeHighRAM
            wram <- makeWorkRAM
            let serialPort = makeSerialPort putCharSerialCallback
                hwState = HardwareState cart hram serialPort wram
                emuState = EmulatorState initialCPUState hwState
            
            flip runStateT emuState . runKaraa $ do
                evalRepl prompt (commands logPath) monitorOptions optionPrefix Nothing (Word $ \_ -> return []) (return ()) (return Exit) 

prompt :: MultiLine -> HaskelineT Karaa String
prompt _ = pure "γβ> "

commands :: FilePath -> String -> HaskelineT Karaa ()
commands logPath cmd = case words cmd of
    ["read", (readMaybe -> Just addr)] -> lift $ readCommand addr 1
    ["read", (readMaybe -> Just addr)
           , (readMaybe -> Just off)]  -> lift $ readCommand addr off
    ["regs"] -> lift regsCommand
    ["step"] -> lift (stepCommand Nothing)
    ["run"]  -> lift (runCommand  Nothing)
    ["tracestep"] -> lift (stepCommand $ Just logPath)
    ["tracerun"]  -> lift (runCommand  $ Just logPath)
    [] -> return ()
    _  -> liftIO $ putStrLn ("Unknown command: " ++ cmd)

readCommand :: Word16 -> Int -> Karaa ()
readCommand base off
    | off > 8   = readCommand base       8
               >> readCommand (base + 8) (off - 8)
    | otherwise = liftIO . putStrLn . bytes =<< mapM readAddr (addrRange base off)
    where bytes = intercalate " " . fmap showHex

regsCommand :: Karaa ()
regsCommand = do
    regs <- use registerFile
    let regs' = regs & wideRegister PC -~ 1
    liftIO $ print regs'

logStep :: FilePath -> Karaa ()
logStep path = do
    regs <- use registerFile
    let regs' = regs & wideRegister PC -~ 1
        regDumps = (\r -> show r ++ ": " ++ showHex (regs' ^. register r)) <$> [A, F, B, C, D, E, H, L]
        wideRegDumps = (\wr -> show wr ++ ": " ++ showHex (regs' ^. wideRegister wr)) <$>  [SP, PC]
        pc = regs' ^. wideRegister PC
    bytes <- mapM (fmap showHex . readAddr) [pc .. pc + 3]
    let bytesDump = " (" ++ intercalate " " bytes ++ ")"

    liftIO $ appendFile path (intercalate " " (regDumps ++ wideRegDumps) ++ bytesDump ++ "\n")

stepCommand :: Maybe FilePath -> Karaa () 
stepCommand logPath = do
    opcode <- use nextOpcode
    let instruction = decodeInstruction opcode
        mnemonic = toMnemonic instruction
    maybe (return ()) logStep logPath
    assign nextOpcode =<< execute instruction

runCommand :: Maybe FilePath -> Karaa ()
runCommand logPath = go
    where go = stepCommand logPath >> go

addrRange :: Word16 -> Int -> [Word16]
addrRange base 0   = []
addrRange base off = [base, base + direction .. end]
    where direction = fromIntegral (signum off)
          end = fromIntegral (fromIntegral base + off - 1)

monitorOptions :: Options (HaskelineT Karaa)
monitorOptions = []

optionPrefix :: Maybe Char
optionPrefix = Just ':'
