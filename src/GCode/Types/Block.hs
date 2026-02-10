{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GCode.Types.Block where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Control.Lens (makeLenses)
import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
------------------------------------------------------------------------------
import           GCode.Types.Command
import           GCode.Types.Parameter
import           Utils
------------------------------------------------------------------------------

-- | A block is one line of G-code. A block may have:
--
-- * An optional line number (N-word)
-- * An optional command (G, M, or T)
-- * Zero or more parameters
-- * An optional comment
-- * An optional checksum
--
-- A block with no command but with a comment is a comment-only line.
-- A block with no command and only parameters represents "sticky" command
-- continuation (the command from the previous block is implied).
data Block = Block
  { _block_lineNumber :: Maybe Int
  , _block_command :: Maybe Command
  , _block_params :: [Parameter]
  , _block_comment :: Maybe Text
  , _block_checksum :: Maybe Int
  } deriving (Eq, Show, Read, Generic, Data)

makeLenses ''Block

instance NFData Block

-- | An empty block (blank line).
emptyBlock
  :: Block
emptyBlock = Block Nothing Nothing [] Nothing Nothing

-- | Create a block with just a command and no parameters.
commandBlock
  :: Command
  -> Block
commandBlock cmd = emptyBlock { _block_command = Just cmd }

-- | Create a block with a command and parameters.
commandWithParams
  :: Command
  -> [Parameter]
  -> Block
commandWithParams cmd ps = emptyBlock
  { _block_command = Just cmd
  , _block_params = ps
  }

-- | Create a comment-only block.
commentBlock
  :: Text
  -> Block
commentBlock c = emptyBlock { _block_comment = Just c }

-- | Check whether a block is a comment-only line.
isCommentOnly
  :: Block
  -> Bool
isCommentOnly b =
  _block_command b == Nothing
  && null (_block_params b)
  && _block_comment b /= Nothing

-- | Check whether a block is completely empty (blank line).
isEmptyBlock
  :: Block
  -> Bool
isEmptyBlock b =
  _block_command b == Nothing
  && null (_block_params b)
  && _block_comment b == Nothing
  && _block_lineNumber b == Nothing

instance Pretty Block where
  pretty b = T.intercalate " " $ filter (not . T.null) parts
    where
      parts =
        [ maybe "" (\n -> "N" <> T.pack (show n)) (_block_lineNumber b)
        , maybe "" pretty (_block_command b)
        , T.intercalate " " $ map pretty (_block_params b)
        , maybe "" (\c -> "; " <> c) (_block_comment b)
        ]
