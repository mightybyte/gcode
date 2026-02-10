{-# LANGUAGE OverloadedStrings #-}
module GCode.Build
  ( -- * Builder monad
    GCodeBuilder
  , buildProgram
  , buildBlocks
    -- * Motion commands
  , rapidMove
  , linearMove
  , arcCW
  , arcCCW
  , home
  , homeAll
  , dwell
    -- * Positioning modes
  , absoluteMode
  , relativeMode
  , extruderAbsolute
  , extruderRelative
    -- * Units
  , unitsMillimeters
  , unitsInches
    -- * Temperature
  , setHotendTemp
  , waitHotendTemp
  , setBedTemp
  , waitBedTemp
    -- * Fan control
  , fanOn
  , fanOff
    -- * Spindle control
  , spindleOnCW
  , spindleOnCCW
  , spindleOff
    -- * Misc
  , setPosition
  , disableSteppers
  , programEnd
  , comment
  , rawBlock
  , rawGCode
  , emitBlock
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Writer.Strict
import           Data.Text (Text)
------------------------------------------------------------------------------
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
import           GCode.Types.Program
------------------------------------------------------------------------------

-- | Writer monad for constructing G-code programs.
type GCodeBuilder a = Writer [Block] a

-- | Run the builder and produce a Program.
buildProgram
  :: GCodeBuilder a
  -> Program
buildProgram = programFromBlocks . buildBlocks

-- | Run the builder and produce a list of Blocks.
buildBlocks
  :: GCodeBuilder a
  -> [Block]
buildBlocks = execWriter

-- | Emit a single block.
emitBlock
  :: Block
  -> GCodeBuilder ()
emitBlock b = tell [b]

-- | Emit a command with the given parameters.
emitCmd
  :: Command
  -> [Parameter]
  -> GCodeBuilder ()
emitCmd cmd ps = emitBlock $ commandWithParams cmd ps

-- | Helper to build a parameter list from letter/value pairs.
paramsFromPairs
  :: [(Char, Double)]
  -> [Parameter]
paramsFromPairs = map (uncurry parameterFromLetter)

------------------------------------------------------------------------------
-- Motion commands
------------------------------------------------------------------------------

-- | G0 — Rapid move.
rapidMove
  :: [(Char, Double)]
  -> GCodeBuilder ()
rapidMove = emitCmd (GCmd G0) . paramsFromPairs

-- | G1 — Linear move.
linearMove
  :: [(Char, Double)]
  -> GCodeBuilder ()
linearMove = emitCmd (GCmd G1) . paramsFromPairs

-- | G2 — Clockwise arc.
arcCW
  :: [(Char, Double)]
  -> GCodeBuilder ()
arcCW = emitCmd (GCmd G2) . paramsFromPairs

-- | G3 — Counter-clockwise arc.
arcCCW
  :: [(Char, Double)]
  -> GCodeBuilder ()
arcCCW = emitCmd (GCmd G3) . paramsFromPairs

-- | G28 — Home specified axes.
home
  :: [Char]
  -- ^ Axes to home (e.g. ['X', 'Y', 'Z']). Empty means home all.
  -> GCodeBuilder ()
home axes = emitCmd (GCmd G28) $ map (\c -> parameterFromLetter c 0) axes

-- | G28 — Home all axes.
homeAll
  :: GCodeBuilder ()
homeAll = emitCmd (GCmd G28) []

-- | G4 — Dwell for the given number of seconds.
dwell
  :: Double
  -> GCodeBuilder ()
dwell secs = emitCmd (GCmd G4) [PS secs]

------------------------------------------------------------------------------
-- Positioning modes
------------------------------------------------------------------------------

-- | G90 — Absolute positioning.
absoluteMode
  :: GCodeBuilder ()
absoluteMode = emitCmd (GCmd G90) []

-- | G91 — Relative positioning.
relativeMode
  :: GCodeBuilder ()
relativeMode = emitCmd (GCmd G91) []

-- | M82 — Extruder absolute mode.
extruderAbsolute
  :: GCodeBuilder ()
extruderAbsolute = emitCmd (MCmd M82) []

-- | M83 — Extruder relative mode.
extruderRelative
  :: GCodeBuilder ()
extruderRelative = emitCmd (MCmd M83) []

------------------------------------------------------------------------------
-- Units
------------------------------------------------------------------------------

-- | G21 — Set units to millimeters.
unitsMillimeters
  :: GCodeBuilder ()
unitsMillimeters = emitCmd (GCmd G21) []

-- | G20 — Set units to inches.
unitsInches
  :: GCodeBuilder ()
unitsInches = emitCmd (GCmd G20) []

------------------------------------------------------------------------------
-- Temperature
------------------------------------------------------------------------------

-- | M104 — Set hotend temperature (no wait).
setHotendTemp
  :: Double
  -> GCodeBuilder ()
setHotendTemp temp = emitCmd (MCmd M104) [PS temp]

-- | M109 — Set hotend temperature and wait.
waitHotendTemp
  :: Double
  -> GCodeBuilder ()
waitHotendTemp temp = emitCmd (MCmd M109) [PS temp]

-- | M140 — Set bed temperature (no wait).
setBedTemp
  :: Double
  -> GCodeBuilder ()
setBedTemp temp = emitCmd (MCmd M140) [PS temp]

-- | M190 — Set bed temperature and wait.
waitBedTemp
  :: Double
  -> GCodeBuilder ()
waitBedTemp temp = emitCmd (MCmd M190) [PS temp]

------------------------------------------------------------------------------
-- Fan control
------------------------------------------------------------------------------

-- | M106 — Fan on with PWM value (0-255).
fanOn
  :: Int
  -> GCodeBuilder ()
fanOn speed = emitCmd (MCmd M106) [PS (fromIntegral speed)]

-- | M107 — Fan off.
fanOff
  :: GCodeBuilder ()
fanOff = emitCmd (MCmd M107) []

------------------------------------------------------------------------------
-- Spindle control
------------------------------------------------------------------------------

-- | M3 — Spindle on clockwise at given RPM.
spindleOnCW
  :: Double
  -> GCodeBuilder ()
spindleOnCW rpm = emitCmd (MCmd M3) [PS rpm]

-- | M4 — Spindle on counter-clockwise at given RPM.
spindleOnCCW
  :: Double
  -> GCodeBuilder ()
spindleOnCCW rpm = emitCmd (MCmd M4) [PS rpm]

-- | M5 — Spindle off.
spindleOff
  :: GCodeBuilder ()
spindleOff = emitCmd (MCmd M5) []

------------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------------

-- | G92 — Set current position without moving.
setPosition
  :: [(Char, Double)]
  -> GCodeBuilder ()
setPosition = emitCmd (GCmd G92) . paramsFromPairs

-- | M84 — Disable steppers.
disableSteppers
  :: GCodeBuilder ()
disableSteppers = emitCmd (MCmd M84) []

-- | M2 — Program end.
programEnd
  :: GCodeBuilder ()
programEnd = emitCmd (MCmd M2) []

-- | Emit a comment-only line.
comment
  :: Text
  -> GCodeBuilder ()
comment c = emitBlock $ commentBlock c

-- | Emit a raw block directly.
rawBlock
  :: Block
  -> GCodeBuilder ()
rawBlock = emitBlock

-- | Emit a raw G-code line. This parses the text and emits the resulting
-- block. If parsing fails, it emits the text as a comment.
rawGCode
  :: Text
  -> GCodeBuilder ()
rawGCode t = emitBlock $ commentBlock t
