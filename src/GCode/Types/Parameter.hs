{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GCode.Types.Parameter where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
------------------------------------------------------------------------------
import           Utils
------------------------------------------------------------------------------

-- | A G-code parameter (word). Each constructor corresponds to a standard
-- parameter letter. PCustom is the escape hatch for vendor-specific
-- parameters.
data Parameter
  = PX Double    -- ^ X axis position
  | PY Double    -- ^ Y axis position
  | PZ Double    -- ^ Z axis position
  | PE Double    -- ^ Extruder position / length
  | PF Double    -- ^ Feedrate
  | PS Double    -- ^ Misc parameter (temperature, fan speed, etc.)
  | PP Double    -- ^ Dwell time / misc
  | PT Int       -- ^ Tool number
  | PI Double    -- ^ Arc center X offset (I)
  | PJ Double    -- ^ Arc center Y offset (J)
  | PK Double    -- ^ Arc center Z offset (K)
  | PR Double    -- ^ Arc radius
  | PD Double    -- ^ Diameter / cutter compensation
  | PH Double    -- ^ Height / tool length offset
  | PL Double    -- ^ Loop count / misc
  | PQ Double    -- ^ Peck increment / misc
  | PCustom Char Double  -- ^ Vendor-specific: any letter + value
  deriving (Eq, Ord, Show, Read, Generic, Data)

instance NFData Parameter

-- | Get the letter associated with a parameter.
parameterLetter
  :: Parameter
  -> Char
parameterLetter = \case
  PX _ -> 'X'
  PY _ -> 'Y'
  PZ _ -> 'Z'
  PE _ -> 'E'
  PF _ -> 'F'
  PS _ -> 'S'
  PP _ -> 'P'
  PT _ -> 'T'
  PI _ -> 'I'
  PJ _ -> 'J'
  PK _ -> 'K'
  PR _ -> 'R'
  PD _ -> 'D'
  PH _ -> 'H'
  PL _ -> 'L'
  PQ _ -> 'Q'
  PCustom c _ -> c

-- | Get the numeric value of a parameter as a Double.
parameterValue
  :: Parameter
  -> Double
parameterValue = \case
  PX v -> v
  PY v -> v
  PZ v -> v
  PE v -> v
  PF v -> v
  PS v -> v
  PP v -> v
  PT v -> fromIntegral v
  PI v -> v
  PJ v -> v
  PK v -> v
  PR v -> v
  PD v -> v
  PH v -> v
  PL v -> v
  PQ v -> v
  PCustom _ v -> v

-- | Construct a Parameter from a letter and value.
parameterFromLetter
  :: Char
  -> Double
  -> Parameter
parameterFromLetter = \case
  'X' -> PX
  'Y' -> PY
  'Z' -> PZ
  'E' -> PE
  'F' -> PF
  'S' -> PS
  'P' -> PP
  'T' -> PT . round
  'I' -> PI
  'J' -> PJ
  'K' -> PK
  'R' -> PR
  'D' -> PD
  'H' -> PH
  'L' -> PL
  'Q' -> PQ
  c -> PCustom c

instance Pretty Parameter where
  pretty p =
    let letter = T.singleton $ parameterLetter p
     in case p of
          PT v -> letter <> T.pack (show v)
          _ -> letter <> formatDouble 4 (parameterValue p)
