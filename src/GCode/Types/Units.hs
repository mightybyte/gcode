{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module GCode.Types.Units where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Data
import qualified Data.Text as T
import           GHC.Generics
------------------------------------------------------------------------------
import           Utils
------------------------------------------------------------------------------

-- | Millimeters — used for axis positions and distances.
newtype Mm = Mm { _mm_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Mm where
  pretty (Mm v) = formatDouble 4 v <> "mm"

instance ToJSON Mm where
  toJSON (Mm v) = toJSON v

instance FromJSON Mm where
  parseJSON v = Mm <$> parseJSON v

-- | Millimeters per minute — used for feedrates.
newtype MmPerMin = MmPerMin { _mmPerMin_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty MmPerMin where
  pretty (MmPerMin v) = formatDouble 1 v <> "mm/min"

instance ToJSON MmPerMin where
  toJSON (MmPerMin v) = toJSON v

instance FromJSON MmPerMin where
  parseJSON v = MmPerMin <$> parseJSON v

-- | Degrees Celsius — used for hotend and bed temperatures.
newtype Celsius = Celsius { _celsius_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Celsius where
  pretty (Celsius v) = formatDouble 1 v <> "°C"

instance ToJSON Celsius where
  toJSON (Celsius v) = toJSON v

instance FromJSON Celsius where
  parseJSON v = Celsius <$> parseJSON v

-- | Percentage (0-100) — used for fan speed, flow rate, speed factor.
newtype Percent = Percent { _percent_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Percent where
  pretty (Percent v) = formatDouble 1 v <> "%"

instance ToJSON Percent where
  toJSON (Percent v) = toJSON v

instance FromJSON Percent where
  parseJSON v = Percent <$> parseJSON v

-- | Seconds — used for dwell times.
newtype Seconds = Seconds { _seconds_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Seconds where
  pretty (Seconds v) = formatDouble 3 v <> "s"

instance ToJSON Seconds where
  toJSON (Seconds v) = toJSON v

instance FromJSON Seconds where
  parseJSON v = Seconds <$> parseJSON v

-- | Inches — used for axis positions when in G20 mode.
newtype Inches = Inches { _inches_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Inches where
  pretty (Inches v) = formatDouble 4 v <> "in"

instance ToJSON Inches where
  toJSON (Inches v) = toJSON v

instance FromJSON Inches where
  parseJSON v = Inches <$> parseJSON v

-- | Inches per minute — used for feedrates in inch mode.
newtype InPerMin = InPerMin { _inPerMin_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty InPerMin where
  pretty (InPerMin v) = formatDouble 2 v <> "in/min"

instance ToJSON InPerMin where
  toJSON (InPerMin v) = toJSON v

instance FromJSON InPerMin where
  parseJSON v = InPerMin <$> parseJSON v

-- | Degrees — used for rotary axes.
newtype Degrees = Degrees { _degrees_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Degrees where
  pretty (Degrees v) = formatDouble 3 v <> "°"

instance ToJSON Degrees where
  toJSON (Degrees v) = toJSON v

instance FromJSON Degrees where
  parseJSON v = Degrees <$> parseJSON v

-- | RPM — used for spindle speed.
newtype Rpm = Rpm { _rpm_value :: Double }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Generic, Data, NFData)

instance Pretty Rpm where
  pretty (Rpm v) = formatDouble 0 v <> "rpm"

instance ToJSON Rpm where
  toJSON (Rpm v) = toJSON v

instance FromJSON Rpm where
  parseJSON v = Rpm <$> parseJSON v

------------------------------------------------------------------------------
-- Conversion helpers
------------------------------------------------------------------------------

-- | Convert millimeters to inches.
mmToInches
  :: Mm
  -> Inches
mmToInches (Mm v) = Inches (v / 25.4)

-- | Convert inches to millimeters.
inchesToMm
  :: Inches
  -> Mm
inchesToMm (Inches v) = Mm (v * 25.4)

-- | Convert a fan PWM value (0-255) to a percentage.
pwmToPercent
  :: Double
  -> Percent
pwmToPercent pwm = Percent (pwm / 255.0 * 100.0)

-- | Convert a percentage to a fan PWM value (0-255).
percentToPwm
  :: Percent
  -> Double
percentToPwm (Percent pct) = pct / 100.0 * 255.0
