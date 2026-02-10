{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GCode.Render
  ( -- * Configuration
    RenderConfig(..)
  , CommentStyle(..)
  , defaultRenderConfig
    -- * Rendering
  , renderProgram
  , renderProgramWith
  , renderBlock
  , renderBlockWith
  , renderCommand
  , renderParameter
  , renderParameterWith
  ) where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics hiding (prec)
------------------------------------------------------------------------------
import           GCode.Checksum (computeChecksum)
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
import           GCode.Types.Program
import           Utils
------------------------------------------------------------------------------

-- | Style for rendering comments.
data CommentStyle
  = Semicolon       -- ^ @; comment@
  | Parenthetical   -- ^ @(comment)@
  deriving (Eq, Show, Read, Generic, Data, Enum, Bounded)

instance NFData CommentStyle

-- | Configuration for the G-code renderer.
data RenderConfig = RenderConfig
  { _renderConfig_precision :: Int
  -- ^ Decimal places for coordinate values (default 3)
  , _renderConfig_tempPrecision :: Int
  -- ^ Decimal places for temperature values (default 1)
  , _renderConfig_includeChecksums :: Bool
  -- ^ Whether to append XOR checksums
  , _renderConfig_includeLineNumbers :: Bool
  -- ^ Whether to include line numbers
  , _renderConfig_commentStyle :: CommentStyle
  -- ^ Comment style to use
  } deriving (Eq, Show, Read, Generic, Data)

instance NFData RenderConfig

-- | Default render configuration: 3 decimal places, semicolon comments,
-- no checksums, no line numbers.
defaultRenderConfig
  :: RenderConfig
defaultRenderConfig = RenderConfig
  { _renderConfig_precision = 3
  , _renderConfig_tempPrecision = 1
  , _renderConfig_includeChecksums = False
  , _renderConfig_includeLineNumbers = False
  , _renderConfig_commentStyle = Semicolon
  }

-- | Render a program to text using default configuration.
renderProgram
  :: Program
  -> Text
renderProgram = renderProgramWith defaultRenderConfig

-- | Render a program to text using the given configuration.
renderProgramWith
  :: RenderConfig
  -> Program
  -> Text
renderProgramWith cfg prog =
  T.unlines $ map (renderBlockWith cfg) (_program_blocks prog)

-- | Render a single block using default configuration.
renderBlock
  :: Block
  -> Text
renderBlock = renderBlockWith defaultRenderConfig

-- | Render a single block using the given configuration.
renderBlockWith
  :: RenderConfig
  -> Block
  -> Text
renderBlockWith cfg b =
  let parts = filter (not . T.null)
        [ renderLineNumber cfg b
        , maybe "" renderCommand (_block_command b)
        , renderParams cfg b
        , renderComment cfg b
        ]
      line = T.intercalate " " parts
      withChecksum
        | _renderConfig_includeChecksums cfg && not (T.null line) =
            let cs = computeChecksum line
             in line <> "*" <> T.pack (show cs)
        | otherwise = line
   in withChecksum

renderLineNumber
  :: RenderConfig
  -> Block
  -> Text
renderLineNumber cfg b =
  case (_renderConfig_includeLineNumbers cfg, _block_lineNumber b) of
    (_, Just n) -> "N" <> T.pack (show n)
    _ -> ""

-- | Render a command to its text representation.
renderCommand
  :: Command
  -> Text
renderCommand = pretty

renderParams
  :: RenderConfig
  -> Block
  -> Text
renderParams cfg b =
  T.intercalate " " $ map (renderParameterWith cfg) (_block_params b)

renderComment
  :: RenderConfig
  -> Block
  -> Text
renderComment cfg b = case _block_comment b of
  Nothing -> ""
  Just c -> case _renderConfig_commentStyle cfg of
    Semicolon -> "; " <> c
    Parenthetical -> "(" <> c <> ")"

-- | Render a parameter using default precision.
renderParameter
  :: Parameter
  -> Text
renderParameter = renderParameterWith defaultRenderConfig

-- | Render a parameter using the given configuration's precision.
renderParameterWith
  :: RenderConfig
  -> Parameter
  -> Text
renderParameterWith cfg p =
  let letter = T.singleton $ parameterLetter p
      prec = _renderConfig_precision cfg
   in case p of
        PT v -> letter <> T.pack (show v)
        _ -> letter <> formatParamValue prec (parameterValue p)
