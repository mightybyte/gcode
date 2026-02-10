{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GCode.Types.Program where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Control.Lens (makeLenses)
import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
------------------------------------------------------------------------------
import           GCode.Types.Block
import           GCode.Types.Command
import           Utils
------------------------------------------------------------------------------

-- | Metadata extracted from or associated with a G-code program.
data ProgramMetadata = ProgramMetadata
  { _programMetadata_source :: Maybe Text
  -- ^ Slicer that generated this program (e.g. "PrusaSlicer 2.7")
  , _programMetadata_material :: Maybe Text
  -- ^ Material type (e.g. "PLA")
  , _programMetadata_description :: Maybe Text
  -- ^ Freeform description
  } deriving (Eq, Show, Read, Generic, Data)

makeLenses ''ProgramMetadata

instance NFData ProgramMetadata

-- | Empty metadata with all fields as Nothing.
emptyMetadata
  :: ProgramMetadata
emptyMetadata = ProgramMetadata Nothing Nothing Nothing

-- | A complete G-code program: a sequence of blocks plus optional metadata.
data Program = Program
  { _program_blocks :: [Block]
  , _program_metadata :: ProgramMetadata
  } deriving (Eq, Show, Read, Generic, Data)

makeLenses ''Program

instance NFData Program

-- | Create a program from a list of blocks with no metadata.
programFromBlocks
  :: [Block]
  -> Program
programFromBlocks bs = Program bs emptyMetadata

-- | Count the number of non-empty, non-comment blocks in a program.
commandCount
  :: Program
  -> Int
commandCount prog = length $ filter hasCommand (_program_blocks prog)
  where
    hasCommand b = _block_command b /= Nothing

-- | Get all blocks that have a specific command.
blocksWithCommand
  :: Command
  -> Program
  -> [Block]
blocksWithCommand cmd prog =
  filter (\b -> _block_command b == Just cmd) (_program_blocks prog)

instance Pretty Program where
  pretty prog = T.unlines $ map pretty (_program_blocks prog)
