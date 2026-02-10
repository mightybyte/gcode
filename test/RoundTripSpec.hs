{-# LANGUAGE OverloadedStrings #-}
module RoundTripSpec (roundTripSpec) where

------------------------------------------------------------------------------
import           Data.Either (isRight)
import           Test.Hspec
import           Test.QuickCheck
------------------------------------------------------------------------------
import           GCode.Parse
import           GCode.Render
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
import           GCode.Types.Program
------------------------------------------------------------------------------

roundTripSpec :: Spec
roundTripSpec = do
  describe "render then parse" $ do
    it "round-trips a simple block" $ do
      let b = commandWithParams (GCmd G1) [PX 10.5, PY 20, PF 1200]
      let rendered = renderBlock b
      let Right b' = parseBlock rendered
      _block_command b' `shouldBe` _block_command b
      length (_block_params b') `shouldBe` length (_block_params b)

    it "round-trips a program" $ do
      let prog = programFromBlocks
            [ commandBlock (GCmd G28)
            , commandWithParams (GCmd G1) [PX 10, PY 20, PF 1200]
            , commandWithParams (MCmd M104) [PS 200]
            ]
      let rendered = renderProgram prog
      let Right prog' = parseProgram rendered
      length (_program_blocks prog') `shouldBe` length (_program_blocks prog)

    it "round-trips comment blocks" $ do
      let b = commentBlock "hello world"
      let rendered = renderBlock b
      let Right b' = parseBlock rendered
      _block_comment b' `shouldBe` Just "hello world"

  describe "QuickCheck properties" $ do
    it "rendered blocks can always be parsed" $ property $ \cmd ->
      let code = abs cmd `mod` 100
          b = commandWithParams (GCmd (gCommandFromCode code))
                [PX (fromIntegral (cmd `mod` 1000 :: Int))]
          rendered = renderBlock b
       in isRight (parseBlock rendered)

    it "parse-render-parse is idempotent" $ do
      let input = "G1 X10.5 Y20.3 Z0.2 E1.234 F1200"
      let Right b1 = parseBlock input
      let rendered = renderBlock b1
      let Right b2 = parseBlock rendered
      _block_command b1 `shouldBe` _block_command b2
      length (_block_params b1) `shouldBe` length (_block_params b2)
