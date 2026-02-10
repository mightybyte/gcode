{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GCode.Types.Command where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Data
import qualified Data.Text as T
import           Data.Text (Text)
import           GHC.Generics
------------------------------------------------------------------------------
import           Utils
------------------------------------------------------------------------------

-- | Standard G commands (motion, positioning, units).
data GCommand
  = G0        -- ^ Rapid move
  | G1        -- ^ Linear move
  | G2        -- ^ Clockwise arc
  | G3        -- ^ Counter-clockwise arc
  | G4        -- ^ Dwell
  | G10       -- ^ Retract (firmware)
  | G11       -- ^ Unretract (firmware)
  | G20       -- ^ Inches mode
  | G21       -- ^ Millimeters mode
  | G28       -- ^ Home axes
  | G29       -- ^ Auto bed leveling
  | G90       -- ^ Absolute positioning
  | G91       -- ^ Relative positioning
  | G92       -- ^ Set position
  | GCustom Int  -- ^ Any other G code
  deriving (Eq, Ord, Show, Read, Generic, Data)

instance NFData GCommand

-- | Get the numeric code for a GCommand.
gCommandCode
  :: GCommand
  -> Int
gCommandCode = \case
  G0 -> 0
  G1 -> 1
  G2 -> 2
  G3 -> 3
  G4 -> 4
  G10 -> 10
  G11 -> 11
  G20 -> 20
  G21 -> 21
  G28 -> 28
  G29 -> 29
  G90 -> 90
  G91 -> 91
  G92 -> 92
  GCustom n -> n

-- | Parse an integer into a known GCommand, or GCustom.
gCommandFromCode
  :: Int
  -> GCommand
gCommandFromCode = \case
  0 -> G0
  1 -> G1
  2 -> G2
  3 -> G3
  4 -> G4
  10 -> G10
  11 -> G11
  20 -> G20
  21 -> G21
  28 -> G28
  29 -> G29
  90 -> G90
  91 -> G91
  92 -> G92
  n -> GCustom n

instance Pretty GCommand where
  pretty cmd = "G" <> T.pack (show (gCommandCode cmd))

instance ToJSON GCommand where
  toJSON cmd = String $ pretty cmd

instance FromJSON GCommand where
  parseJSON = withText "GCommand" $ \t ->
    case T.uncons t of
      Just ('G', rest) -> case reads (T.unpack rest) of
        [(n, "")] -> pure $ gCommandFromCode n
        _ -> fail $ "Invalid G command: " ++ T.unpack t
      _ -> fail $ "Expected G command: " ++ T.unpack t

------------------------------------------------------------------------------

-- | Standard M commands (miscellaneous machine control).
data MCommand
  = M0        -- ^ Unconditional stop
  | M1        -- ^ Conditional stop
  | M2        -- ^ Program end
  | M3        -- ^ Spindle on clockwise
  | M4        -- ^ Spindle on counter-clockwise
  | M5        -- ^ Spindle off
  | M6        -- ^ Tool change
  | M7        -- ^ Mist coolant on
  | M8        -- ^ Flood coolant on
  | M9        -- ^ Coolant off
  | M17       -- ^ Enable steppers
  | M18       -- ^ Disable steppers
  | M30       -- ^ Program end and reset
  | M82       -- ^ Extruder absolute mode
  | M83       -- ^ Extruder relative mode
  | M84       -- ^ Disable steppers (alias)
  | M104      -- ^ Set hotend temperature (no wait)
  | M105      -- ^ Report temperatures
  | M106      -- ^ Fan on
  | M107      -- ^ Fan off
  | M109      -- ^ Set hotend temperature (wait)
  | M110      -- ^ Set line number
  | M112      -- ^ Emergency stop
  | M114      -- ^ Report position
  | M115      -- ^ Report firmware info
  | M116      -- ^ Wait for all temperatures
  | M117      -- ^ Display message on LCD
  | M119      -- ^ Report endstop status
  | M140      -- ^ Set bed temperature (no wait)
  | M190      -- ^ Set bed temperature (wait)
  | M200      -- ^ Set filament diameter
  | M201      -- ^ Set max acceleration
  | M203      -- ^ Set max feedrate
  | M204      -- ^ Set acceleration
  | M205      -- ^ Set jerk / junction deviation
  | M206      -- ^ Set home offset
  | M207      -- ^ Set retract length
  | M208      -- ^ Set unretract length
  | M209      -- ^ Auto retract
  | M218      -- ^ Set hotend offset
  | M220      -- ^ Set speed factor
  | M221      -- ^ Set flow factor
  | M226      -- ^ Wait for pin state
  | M280      -- ^ Servo position
  | M300      -- ^ Play tone
  | M301      -- ^ Set PID parameters (hotend)
  | M302      -- ^ Allow cold extrusion
  | M303      -- ^ PID autotune
  | M400      -- ^ Wait for moves to finish
  | M420      -- ^ Bed leveling state
  | M500      -- ^ Save settings to EEPROM
  | M501      -- ^ Restore settings from EEPROM
  | M502      -- ^ Reset to factory defaults
  | M503      -- ^ Report settings
  | M600      -- ^ Filament change
  | M900      -- ^ Linear advance
  | MCustom Int  -- ^ Any other M code
  deriving (Eq, Ord, Show, Read, Generic, Data)

instance NFData MCommand

-- | Get the numeric code for an MCommand.
mCommandCode
  :: MCommand
  -> Int
mCommandCode = \case
  M0 -> 0
  M1 -> 1
  M2 -> 2
  M3 -> 3
  M4 -> 4
  M5 -> 5
  M6 -> 6
  M7 -> 7
  M8 -> 8
  M9 -> 9
  M17 -> 17
  M18 -> 18
  M30 -> 30
  M82 -> 82
  M83 -> 83
  M84 -> 84
  M104 -> 104
  M105 -> 105
  M106 -> 106
  M107 -> 107
  M109 -> 109
  M110 -> 110
  M112 -> 112
  M114 -> 114
  M115 -> 115
  M116 -> 116
  M117 -> 117
  M119 -> 119
  M140 -> 140
  M190 -> 190
  M200 -> 200
  M201 -> 201
  M203 -> 203
  M204 -> 204
  M205 -> 205
  M206 -> 206
  M207 -> 207
  M208 -> 208
  M209 -> 209
  M218 -> 218
  M220 -> 220
  M221 -> 221
  M226 -> 226
  M280 -> 280
  M300 -> 300
  M301 -> 301
  M302 -> 302
  M303 -> 303
  M400 -> 400
  M420 -> 420
  M500 -> 500
  M501 -> 501
  M502 -> 502
  M503 -> 503
  M600 -> 600
  M900 -> 900
  MCustom n -> n

-- | Parse an integer into a known MCommand, or MCustom.
mCommandFromCode
  :: Int
  -> MCommand
mCommandFromCode = \case
  0 -> M0
  1 -> M1
  2 -> M2
  3 -> M3
  4 -> M4
  5 -> M5
  6 -> M6
  7 -> M7
  8 -> M8
  9 -> M9
  17 -> M17
  18 -> M18
  30 -> M30
  82 -> M82
  83 -> M83
  84 -> M84
  104 -> M104
  105 -> M105
  106 -> M106
  107 -> M107
  109 -> M109
  110 -> M110
  112 -> M112
  114 -> M114
  115 -> M115
  116 -> M116
  117 -> M117
  119 -> M119
  140 -> M140
  190 -> M190
  200 -> M200
  201 -> M201
  203 -> M203
  204 -> M204
  205 -> M205
  206 -> M206
  207 -> M207
  208 -> M208
  209 -> M209
  218 -> M218
  220 -> M220
  221 -> M221
  226 -> M226
  280 -> M280
  300 -> M300
  301 -> M301
  302 -> M302
  303 -> M303
  400 -> M400
  420 -> M420
  500 -> M500
  501 -> M501
  502 -> M502
  503 -> M503
  600 -> M600
  900 -> M900
  n -> MCustom n

instance Pretty MCommand where
  pretty cmd = "M" <> T.pack (show (mCommandCode cmd))

instance ToJSON MCommand where
  toJSON cmd = String $ pretty cmd

instance FromJSON MCommand where
  parseJSON = withText "MCommand" $ \t ->
    case T.uncons t of
      Just ('M', rest) -> case reads (T.unpack rest) of
        [(n, "")] -> pure $ mCommandFromCode n
        _ -> fail $ "Invalid M command: " ++ T.unpack t
      _ -> fail $ "Expected M command: " ++ T.unpack t

------------------------------------------------------------------------------

-- | A command is either a G command, M command, or tool change.
data Command
  = GCmd GCommand
  | MCmd MCommand
  | TCmd Int          -- ^ Tool change (T0, T1, etc.)
  deriving (Eq, Ord, Show, Read, Generic, Data)

instance NFData Command

instance Pretty Command where
  pretty = \case
    GCmd g -> pretty g
    MCmd m -> pretty m
    TCmd n -> "T" <> T.pack (show n)

instance ToJSON Command where
  toJSON = String . pretty

instance FromJSON Command where
  parseJSON = withText "Command" $ \t ->
    case T.uncons t of
      Just ('G', rest) -> case reads (T.unpack rest) of
        [(n, "")] -> pure $ GCmd $ gCommandFromCode n
        _ -> fail $ "Invalid command: " ++ T.unpack t
      Just ('M', rest) -> case reads (T.unpack rest) of
        [(n, "")] -> pure $ MCmd $ mCommandFromCode n
        _ -> fail $ "Invalid command: " ++ T.unpack t
      Just ('T', rest) -> case reads (T.unpack rest) of
        [(n, "")] -> pure $ TCmd n
        _ -> fail $ "Invalid command: " ++ T.unpack t
      _ -> fail $ "Unknown command: " ++ T.unpack t
