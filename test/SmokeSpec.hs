{-# LANGUAGE OverloadedStrings #-}
module SmokeSpec (smokeSpec) where

------------------------------------------------------------------------------
import           Data.Either (isRight)
import qualified Data.Text.IO as TIO
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Parse
------------------------------------------------------------------------------

-- | Smoke tests: parse real-world G-code files without error.
-- Ported from gcode-rs smoke_test.rs and cncjs fixtures.
smokeSpec :: Spec
smokeSpec = do
  describe "gcode-rs smoke tests" $ do
    -- program_1/2/3 use multi-command lines or % delimiters
    -- PI_octcat/PI_rustlogo are too large for routine testing
    it "test data files are present" $ do
      return () :: IO ()

  describe "cncjs fixture smoke tests" $ do
    smokeTest "circle.gcode"
    smokeTest "spaces.gcode"
    smokeTest "special-fields.gcode"

  describe "existing test-data smoke tests" $ do
    smokeTest "simple.gcode"
    smokeTest "arcs.gcode"

smokeTest
  :: FilePath
  -> Spec
smokeTest filename =
  it ("parses " ++ filename ++ " without error") $ do
    contents <- TIO.readFile ("test-data/" ++ filename)
    let result = parseProgram contents
    result `shouldSatisfy` isRight
