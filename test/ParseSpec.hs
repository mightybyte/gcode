{-# LANGUAGE OverloadedStrings #-}
module ParseSpec (parseSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Parse
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
------------------------------------------------------------------------------

parseSpec :: Spec
parseSpec = do
  describe "parseBlock" $ do
    it "parses a simple rapid move" $ do
      let Right b = parseBlock "G0 X10 Y20 Z0.5"
      _block_command b `shouldBe` Just (GCmd G0)
      _block_params b `shouldBe` [PX 10, PY 20, PZ 0.5]

    it "parses a linear move with feedrate" $ do
      let Right b = parseBlock "G1 X10.5 Y20.3 Z0.2 E1.234 F1200"
      _block_command b `shouldBe` Just (GCmd G1)
      length (_block_params b) `shouldBe` 5

    it "parses M commands" $ do
      let Right b = parseBlock "M104 S200"
      _block_command b `shouldBe` Just (MCmd M104)
      _block_params b `shouldBe` [PS 200]

    it "parses tool changes" $ do
      let Right b = parseBlock "T0"
      _block_command b `shouldBe` Just (TCmd 0)

    it "parses semicolon comments" $ do
      let Right b = parseBlock "G1 X10 ; move to X10"
      _block_command b `shouldBe` Just (GCmd G1)
      _block_comment b `shouldBe` Just "move to X10"

    it "parses parenthetical comments" $ do
      let Right b = parseBlock "G1 X10 (move to X10)"
      _block_command b `shouldBe` Just (GCmd G1)
      _block_comment b `shouldBe` Just "move to X10"

    it "parses comment-only lines" $ do
      let Right b = parseBlock "; this is a comment"
      _block_command b `shouldBe` Nothing
      _block_comment b `shouldBe` Just "this is a comment"

    it "parses line numbers" $ do
      let Right b = parseBlock "N10 G1 X5 Y10"
      _block_lineNumber b `shouldBe` Just 10
      _block_command b `shouldBe` Just (GCmd G1)

    it "parses checksums" $ do
      let Right b = parseBlock "N1 G28*18"
      _block_lineNumber b `shouldBe` Just 1
      _block_command b `shouldBe` Just (GCmd G28)
      _block_checksum b `shouldBe` Just 18

    it "parses negative values" $ do
      let Right b = parseBlock "G1 X-10.5 Y-20"
      _block_params b `shouldBe` [PX (-10.5), PY (-20)]

    it "parses homing with no params" $ do
      let Right b = parseBlock "G28"
      _block_command b `shouldBe` Just (GCmd G28)
      _block_params b `shouldBe` []

    it "handles case insensitivity" $ do
      let Right b = parseBlock "g1 x10 y20"
      _block_command b `shouldBe` Just (GCmd G1)

    it "parses empty lines" $ do
      let Right b = parseBlock ""
      isEmptyBlock b `shouldBe` True

  describe "parseProgram" $ do
    it "parses a multi-line program" $ do
      let input = "G28\nG1 X10 Y20 F1200\nM104 S200\n"
      let Right prog = parseProgram input
      length (_program_blocks prog) `shouldBe` 3

  describe "resolveSticky" $ do
    it "resolves sticky commands" $ do
      let input = "G1 X10 Y20 F1200\nX20 Y30\nX30 Y40\n"
      let Right prog = parseProgram input
      let resolved = resolveSticky prog
          blocks = _program_blocks resolved
      _block_command (blocks !! 1) `shouldBe` Just (GCmd G1)
      _block_command (blocks !! 2) `shouldBe` Just (GCmd G1)
