{-# LANGUAGE OverloadedStrings #-}
module PortedRenderSpec (portedRenderSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Parse
import           GCode.Render
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
------------------------------------------------------------------------------

-- | Render tests ported from gcode-machine and cncjs
portedRenderSpec :: Spec
portedRenderSpec = do
  describe "gcode-machine: render after parse round-trip" $ do
    it "renders G0 Z-10" $ do
      let Right b = parseBlock "G0 Z-10"
      renderBlock b `shouldBe` "G0 Z-10"

    it "renders G1 X10 Y10" $ do
      let Right b = parseBlock "G1 X10 Y10"
      renderBlock b `shouldBe` "G1 X10 Y10"

    it "renders G2 with arc offsets I J K" $ do
      let Right b = parseBlock "G2 X10 Y11 I6 J7 K8"
      renderBlock b `shouldBe` "G2 X10 Y11 I6 J7 K8"

    it "renders G2 with arc radius R" $ do
      let Right b = parseBlock "G2 X10 Y11 R42.3"
      renderBlock b `shouldBe` "G2 X10 Y11 R42.3"

    it "renders feedrate" $ do
      let Right b = parseBlock "G0 X0 F42"
      renderBlock b `shouldBe` "G0 X0 F42"

    it "renders feedrate with decimal" $ do
      let Right b = parseBlock "G0 X0 F42.3"
      renderBlock b `shouldBe` "G0 X0 F42.3"

    it "renders spindle speed" $ do
      let b = commandWithParams (GCmd G0) [PX 10, PS 200]
      renderBlock b `shouldBe` "G0 X10 S200"

  describe "cncjs: render after parse preserves structure" $ do
    it "renders G0 X-5 Y0 Z0 F200" $ do
      let Right b = parseBlock "G0 X-5 Y0 Z0 F200"
      renderBlock b `shouldBe` "G0 X-5 Y0 Z0 F200"

    it "renders arc with negative offsets" $ do
      let Right b = parseBlock "G2 X0 Y-5 I-5 J0"
      renderBlock b `shouldBe` "G2 X0 Y-5 I-5 J0"

    it "renders with line number" $ do
      let Right b = parseBlock "N1 G20"
      renderBlock b `shouldBe` "N1 G20"

    it "renders with checksum when enabled" $ do
      let cfg = defaultRenderConfig { _renderConfig_includeChecksums = True }
      let b = commandBlock (GCmd G28)
      let rendered = renderBlockWith cfg b
      -- Should contain asterisk for checksum
      '*' `elem` show rendered `shouldBe` True

    it "renders comment-only line with semicolon" $ do
      let b = commentBlock "Operation:    0"
      renderBlock b `shouldBe` "; Operation:    0"

    it "renders parenthetical comment style" $ do
      let cfg = defaultRenderConfig { _renderConfig_commentStyle = Parenthetical }
      let b = (commandWithParams (GCmd G21) [])
            { _block_comment = Just "Units in millimeters" }
      renderBlockWith cfg b `shouldBe` "G21 (Units in millimeters)"

  describe "gcode-rs: render round-trip" $ do
    it "renders G90" $ do
      let Right b = parseBlock "G90"
      renderBlock b `shouldBe` "G90"

    it "renders G01 X5 Y-20" $ do
      let Right b = parseBlock "G01 X5 Y-20"
      -- Our renderer normalizes G01 -> G1
      renderBlock b `shouldBe` "G1 X5 Y-20"
