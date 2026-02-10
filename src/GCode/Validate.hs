{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GCode.Validate
  ( -- * Warnings
    Warning(..)
  , BoundingBox(..)
  , Position(..)
    -- * Validation
  , validate
  , validateWithBounds
  ) where

------------------------------------------------------------------------------
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           GHC.Generics
------------------------------------------------------------------------------
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
import           GCode.Types.Program
import           Utils
------------------------------------------------------------------------------

-- | A 3D bounding box for bounds checking.
data BoundingBox = BoundingBox
  { _boundingBox_minX :: Double
  , _boundingBox_maxX :: Double
  , _boundingBox_minY :: Double
  , _boundingBox_maxY :: Double
  , _boundingBox_minZ :: Double
  , _boundingBox_maxZ :: Double
  } deriving (Eq, Show, Read, Generic)

-- makeLenses ''BoundingBox

-- | A 3D position for reporting out-of-bounds moves.
data Position = Position
  { _position_x :: Double
  , _position_y :: Double
  , _position_z :: Double
  } deriving (Eq, Show, Read, Generic)

-- makeLenses ''Position

-- | Validation warnings.
data Warning
  = NoHomingBeforeMove
  -- ^ A motion command was issued before any homing (G28)
  | NoTemperatureSet
  -- ^ No hotend or bed temperature was set
  | ExtrudeBeforeHeat
  -- ^ Extrusion happened before setting hotend temperature
  | MoveOutOfBounds BoundingBox Position
  -- ^ A move goes outside the specified bounding box
  | UnknownCommand Command
  -- ^ A GCustom or MCustom command was encountered
  | DuplicateParameter Char Int
  -- ^ Same parameter letter appears multiple times in a block (with block index)
  | MissingFeedrate
  -- ^ A linear/arc move was issued with no feedrate ever set
  deriving (Eq, Show, Generic)

instance Pretty Warning where
  pretty = \case
    NoHomingBeforeMove -> "Motion command before homing (G28)"
    NoTemperatureSet -> "No temperature commands found"
    ExtrudeBeforeHeat -> "Extrusion before hotend temperature set"
    MoveOutOfBounds _ pos -> T.concat
      [ "Move out of bounds at ("
      , T.pack $ show (_position_x pos), ", "
      , T.pack $ show (_position_y pos), ", "
      , T.pack $ show (_position_z pos), ")"
      ]
    UnknownCommand cmd -> "Unknown command: " <> pretty cmd
    DuplicateParameter c idx -> T.concat
      [ "Duplicate parameter '", T.singleton c
      , "' in block ", T.pack (show idx)
      ]
    MissingFeedrate -> "Linear/arc move without feedrate set"

-- | Validate a program, returning a list of warnings.
validate
  :: Program
  -> [Warning]
validate = validateWithBounds Nothing

-- | Validate a program with optional bounding box checking.
validateWithBounds
  :: Maybe BoundingBox
  -> Program
  -> [Warning]
validateWithBounds mBounds prog =
  let blocks = _program_blocks prog
      indexed = zip [0..] blocks
      warnings = concat
        [ checkHomingBeforeMove indexed
        , checkTemperatureSet blocks
        , checkExtrudeBeforeHeat indexed
        , checkUnknownCommands indexed
        , checkDuplicateParams indexed
        , checkFeedrate indexed
        , case mBounds of
            Nothing -> []
            Just bounds -> checkBounds bounds indexed
        ]
   in warnings

------------------------------------------------------------------------------
-- Individual validation checks
------------------------------------------------------------------------------

checkHomingBeforeMove
  :: [(Int, Block)]
  -> [Warning]
checkHomingBeforeMove blocks =
  let isHome b = _block_command b == Just (GCmd G28)
      isMove b = _block_command b `elem` map Just [GCmd G0, GCmd G1, GCmd G2, GCmd G3]
      beforeHome = takeWhile (not . isHome . snd) blocks
      movesBeforeHome = filter (isMove . snd) beforeHome
   in if null movesBeforeHome then [] else [NoHomingBeforeMove]

checkTemperatureSet
  :: [Block]
  -> [Warning]
checkTemperatureSet blocks =
  let tempCmds = [MCmd M104, MCmd M109, MCmd M140, MCmd M190]
      hasTemp = any (\b -> _block_command b `elem` map Just tempCmds) blocks
   in if hasTemp then [] else [NoTemperatureSet]

checkExtrudeBeforeHeat
  :: [(Int, Block)]
  -> [Warning]
checkExtrudeBeforeHeat blocks =
  let hotendCmds = [MCmd M104, MCmd M109]
      isHotendSet b = _block_command b `elem` map Just hotendCmds
      hasExtrusion b = any isEParam (_block_params b)
        && _block_command b `elem` map Just [GCmd G0, GCmd G1]
      isEParam = \case
        PE _ -> True
        _ -> False
      beforeHeat = takeWhile (not . isHotendSet . snd) blocks
      extrudesBeforeHeat = filter (hasExtrusion . snd) beforeHeat
   in if null extrudesBeforeHeat then [] else [ExtrudeBeforeHeat]

checkUnknownCommands
  :: [(Int, Block)]
  -> [Warning]
checkUnknownCommands = mapMaybe $ \(_, b) ->
  case _block_command b of
    Just cmd@(GCmd (GCustom _)) -> Just $ UnknownCommand cmd
    Just cmd@(MCmd (MCustom _)) -> Just $ UnknownCommand cmd
    _ -> Nothing

checkDuplicateParams
  :: [(Int, Block)]
  -> [Warning]
checkDuplicateParams = concatMap $ \(idx, b) ->
  let letters = map parameterLetter (_block_params b)
      dups = findDuplicates letters
   in map (\c -> DuplicateParameter c idx) dups

findDuplicates
  :: Eq a
  => [a]
  -> [a]
findDuplicates [] = []
findDuplicates (x:xs)
  | x `elem` xs = x : findDuplicates (filter (/= x) xs)
  | otherwise = findDuplicates xs

checkFeedrate
  :: [(Int, Block)]
  -> [Warning]
checkFeedrate blocks =
  let needsFeedrate b = _block_command b `elem`
        map Just [GCmd G1, GCmd G2, GCmd G3]
      hasFeedParam b = any isFParam (_block_params b)
      isFParam = \case
        PF _ -> True
        _ -> False
      go _ [] = []
      go hasFeed ((_, b):rest)
        | hasFeedParam b = go True rest
        | needsFeedrate b && not hasFeed = [MissingFeedrate]
        | otherwise = go hasFeed rest
   in go False blocks

checkBounds
  :: BoundingBox
  -> [(Int, Block)]
  -> [Warning]
checkBounds bounds blocks = go 0 0 0 blocks
  where
    go _ _ _ [] = []
    go cx cy cz ((_, b):rest) =
      let isMove = _block_command b `elem`
            map Just [GCmd G0, GCmd G1, GCmd G2, GCmd G3]
          getParam c ps = case filter (\p -> parameterLetter p == c) ps of
            (p:_) -> parameterValue p
            [] -> case c of
              'X' -> cx
              'Y' -> cy
              'Z' -> cz
              _ -> 0
          nx = getParam 'X' (_block_params b)
          ny = getParam 'Y' (_block_params b)
          nz = getParam 'Z' (_block_params b)
          oob = isMove &&
            ( nx < _boundingBox_minX bounds || nx > _boundingBox_maxX bounds
            || ny < _boundingBox_minY bounds || ny > _boundingBox_maxY bounds
            || nz < _boundingBox_minZ bounds || nz > _boundingBox_maxZ bounds
            )
          warn = if oob
            then [MoveOutOfBounds bounds (Position nx ny nz)]
            else []
       in warn ++ go nx ny nz rest
