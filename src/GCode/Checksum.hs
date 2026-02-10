{-# LANGUAGE OverloadedStrings #-}
module GCode.Checksum
  ( -- * Checksum computation
    computeChecksum
    -- * Adding checksums to programs
  , addChecksums
  , addLineNumbersAndChecksums
    -- * Verification
  , ChecksumError(..)
  , verifyChecksums
  ) where

------------------------------------------------------------------------------
import           Data.Bits (xor)
import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------
import           GCode.Types.Block
import           GCode.Types.Program
import           Utils
------------------------------------------------------------------------------

-- | Compute the XOR checksum of all characters in the text before any '*'.
-- This is the standard Marlin/RepRap checksum algorithm.
computeChecksum
  :: Text
  -> Int
computeChecksum t =
  let content = T.takeWhile (/= '*') t
   in T.foldl' (\acc c -> acc `xor` ord c) 0 content

-- | Add checksums to all non-empty blocks in a program, using existing
-- line numbers if present.
addChecksums
  :: Program
  -> Program
addChecksums prog = prog
  { _program_blocks = map addBlockChecksum (_program_blocks prog)
  }

addBlockChecksum
  :: Block
  -> Block
addBlockChecksum b
  | isEmptyBlock b = b
  | otherwise =
      let rendered = pretty b
          cs = computeChecksum rendered
       in b { _block_checksum = Just cs }

-- | Add sequential line numbers (starting from 1) and checksums.
addLineNumbersAndChecksums
  :: Program
  -> Program
addLineNumbersAndChecksums prog = prog
  { _program_blocks = zipWith addLnAndCs [1..] (_program_blocks prog)
  }
  where
    addLnAndCs n b
      | isEmptyBlock b = b
      | otherwise =
          let b' = b { _block_lineNumber = Just n }
              rendered = pretty b'
              cs = computeChecksum rendered
           in b' { _block_checksum = Just cs }

-- | An error found when verifying checksums.
data ChecksumError = ChecksumError
  { _checksumError_lineNumber :: Maybe Int
  , _checksumError_expected :: Int
  , _checksumError_actual :: Int
  , _checksumError_blockIndex :: Int
  } deriving (Eq, Show)

instance Pretty ChecksumError where
  pretty e = T.concat
    [ "Checksum error"
    , maybe "" (\n -> " at line N" <> T.pack (show n)) (_checksumError_lineNumber e)
    , " (block " <> T.pack (show (_checksumError_blockIndex e)) <> ")"
    , ": expected " <> T.pack (show (_checksumError_expected e))
    , ", got " <> T.pack (show (_checksumError_actual e))
    ]

-- | Verify all checksums in a program. Returns errors for any blocks
-- where the stored checksum doesn't match the computed one.
verifyChecksums
  :: Program
  -> [ChecksumError]
verifyChecksums prog = concatMap checkOne $ zip [0..] (_program_blocks prog)
  where
    checkOne (idx, b) = case _block_checksum b of
      Nothing -> []
      Just stored ->
        let b' = b { _block_checksum = Nothing }
            rendered = pretty b'
            computed = computeChecksum rendered
         in if stored == computed
              then []
              else [ChecksumError (_block_lineNumber b) stored computed idx]
