{-# LANGUAGE OverloadedStrings #-}
module GCode.Parse
  ( -- * Parsing functions
    parseProgram
  , parseBlock
  , parseBlocksLazy
    -- * Resolving sticky commands
  , resolveSticky
    -- * Re-export for error handling
  , ParseErrorBundle
  ) where

------------------------------------------------------------------------------
import           Data.Char (isAlpha, toUpper)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
------------------------------------------------------------------------------
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
import           GCode.Types.Program
------------------------------------------------------------------------------

type Parser = Parsec Void Text

-- | Parse an entire G-code program from text.
parseProgram
  :: Text
  -> Either (ParseErrorBundle Text Void) Program
parseProgram input =
  case parse programParser "gcode" input of
    Left err -> Left err
    Right blocks -> Right $ programFromBlocks (filter (not . isEmptyBlock) blocks)

-- | Parse a single line of G-code.
parseBlock
  :: Text
  -> Either (ParseErrorBundle Text Void) Block
parseBlock = parse (blockParser <* eof) "gcode"

-- | Parse blocks lazily, line by line. Each line is parsed independently
-- so a parse error on one line doesn't prevent parsing subsequent lines.
parseBlocksLazy
  :: Text
  -> [Either (ParseErrorBundle Text Void) Block]
parseBlocksLazy input = map parseBlock (T.lines input)

------------------------------------------------------------------------------
-- Internal parsers
------------------------------------------------------------------------------

programParser
  :: Parser [Block]
programParser = blockParser `sepEndBy` newline <* eof

blockParser
  :: Parser Block
blockParser = do
  hspace
  ln <- optional lineNumberParser
  hspace
  cmd <- optional commandParser
  hspace
  ps <- many (paramParser <* hspace)
  cmt1 <- optional commentParser
  cs <- optional checksumParser
  hspace
  cmt2 <- optional commentParser
  let cmt = cmt2 <|> cmt1
  pure $ Block ln cmd ps cmt cs

lineNumberParser
  :: Parser Int
lineNumberParser = do
  _ <- ciChar 'N'
  n <- L.decimal
  pure n

commandParser
  :: Parser Command
commandParser = gCommandParser <|> mCommandParser <|> tCommandParser

gCommandParser
  :: Parser Command
gCommandParser = do
  _ <- ciChar 'G'
  n <- L.decimal
  pure $ GCmd $ gCommandFromCode n

mCommandParser
  :: Parser Command
mCommandParser = do
  _ <- ciChar 'M'
  n <- L.decimal
  pure $ MCmd $ mCommandFromCode n

tCommandParser
  :: Parser Command
tCommandParser = do
  _ <- ciChar 'T'
  n <- L.decimal
  pure $ TCmd n

paramParser
  :: Parser Parameter
paramParser = do
  letter <- satisfy isParamLetter
  val <- signedNumber
  pure $ parameterFromLetter (toUpper letter) val

isParamLetter
  :: Char
  -> Bool
isParamLetter c = isAlpha c && toUpper c `notElem` ['G', 'M', 'N', 'T']

signedNumber
  :: Parser Double
signedNumber = do
  hspace
  sign <- option 1 (char '-' *> pure (-1) <|> char '+' *> pure 1)
  val <- try L.float <|> (fromIntegral <$> (L.decimal :: Parser Int))
  pure (sign * val)

commentParser
  :: Parser Text
commentParser = semicolonComment <|> parenComment

semicolonComment
  :: Parser Text
semicolonComment = do
  _ <- char ';'
  hspace
  rest <- takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
  pure $ T.stripEnd rest

parenComment
  :: Parser Text
parenComment = do
  _ <- char '('
  content <- takeWhileP Nothing (/= ')')
  _ <- char ')'
  pure content

checksumParser
  :: Parser Int
checksumParser = do
  _ <- char '*'
  L.decimal

-- | Case-insensitive single character match.
ciChar
  :: Char
  -> Parser Char
ciChar c =
  let u = toUpper c
      l = toLower c
  in char u <|> char l
  where
    toLower ch
      | ch >= 'A' && ch <= 'Z' = toEnum (fromEnum ch + 32)
      | otherwise = ch

------------------------------------------------------------------------------
-- Sticky command resolution
------------------------------------------------------------------------------

-- | Resolve sticky commands: if a block has no command but has parameters,
-- it inherits the command from the previous block.
resolveSticky
  :: Program
  -> Program
resolveSticky prog = prog
  { _program_blocks = go Nothing (_program_blocks prog)
  }
  where
    go _ [] = []
    go prev (b:bs) =
      let cmd = case _block_command b of
            Just c -> Just c
            Nothing
              | not (null (_block_params b)) -> prev
              | otherwise -> Nothing
          b' = b { _block_command = cmd }
          nextPrev = _block_command b' <|> prev
       in b' : go nextPrev bs
