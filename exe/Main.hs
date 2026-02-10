{-# LANGUAGE OverloadedStrings #-}
module Main where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
------------------------------------------------------------------------------
import           GCode.Parse
import           GCode.Render
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Program
import           GCode.Validate
import           Utils
------------------------------------------------------------------------------

data Cmd
  = Parse FilePath
  | Stats FilePath
  | Validate FilePath
  | RoundTrip FilePath FilePath

cmdParser :: Parser Cmd
cmdParser = subparser
  ( command "parse"
    (info (Parse <$> argument str (metavar "FILE"))
      (progDesc "Parse a G-code file and print the AST summary"))
  <> command "stats"
    (info (Stats <$> argument str (metavar "FILE"))
      (progDesc "Print statistics about a G-code file"))
  <> command "validate"
    (info (Validate <$> argument str (metavar "FILE"))
      (progDesc "Validate a G-code file and report warnings"))
  <> command "round-trip"
    (info (RoundTrip
      <$> argument str (metavar "INPUT")
      <*> argument str (metavar "OUTPUT"))
      (progDesc "Parse and re-render a G-code file"))
  )

main :: IO ()
main = do
  cmd <- execParser (info (cmdParser <**> helper)
    (fullDesc <> progDesc "G-code utility" <> header "gcode-examples"))
  case cmd of
    Parse f -> runParse f
    Stats f -> runStats f
    Validate f -> runValidate f
    RoundTrip i o -> runRoundTrip i o

runParse :: FilePath -> IO ()
runParse f = do
  input <- TIO.readFile f
  case parseProgram input of
    Left err -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show err
      exitFailure
    Right prog -> do
      let blocks = _program_blocks prog
      TIO.putStrLn $ "Parsed " <> T.pack (show (length blocks)) <> " blocks"
      mapM_ (TIO.putStrLn . pretty) (take 20 blocks)
      if length blocks > 20
        then TIO.putStrLn $ "... and " <> T.pack (show (length blocks - 20)) <> " more"
        else pure ()

runStats :: FilePath -> IO ()
runStats f = do
  input <- TIO.readFile f
  case parseProgram input of
    Left err -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show err
      exitFailure
    Right prog -> do
      let blocks = _program_blocks prog
          cmds = commandCount prog
          comments = length $ filter isCommentOnly blocks
          empties = length $ filter isEmptyBlock blocks
          gMoves = length $ filter isGMove blocks
      TIO.putStrLn $ "Total blocks: " <> T.pack (show (length blocks))
      TIO.putStrLn $ "Commands: " <> T.pack (show cmds)
      TIO.putStrLn $ "Comments: " <> T.pack (show comments)
      TIO.putStrLn $ "Empty lines: " <> T.pack (show empties)
      TIO.putStrLn $ "Motion commands (G0/G1/G2/G3): " <> T.pack (show gMoves)

isGMove :: Block -> Bool
isGMove b = _block_command b `elem`
  map Just [GCmd G0, GCmd G1, GCmd G2, GCmd G3]

runValidate :: FilePath -> IO ()
runValidate f = do
  input <- TIO.readFile f
  case parseProgram input of
    Left err -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show err
      exitFailure
    Right prog -> do
      let warnings = validate prog
      if null warnings
        then TIO.putStrLn "No warnings found."
        else do
          TIO.putStrLn $ T.pack (show (length warnings)) <> " warning(s):"
          mapM_ (TIO.putStrLn . ("  - " <>) . pretty) warnings

runRoundTrip :: FilePath -> FilePath -> IO ()
runRoundTrip i o = do
  input <- TIO.readFile i
  case parseProgram input of
    Left err -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show err
      exitFailure
    Right prog -> do
      let output = renderProgram prog
      TIO.writeFile o output
      TIO.putStrLn $ "Wrote " <> T.pack (show (T.length output)) <> " characters to " <> T.pack o
