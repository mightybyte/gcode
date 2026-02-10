{-# LANGUAGE OverloadedStrings #-}
module RenderSpec (renderSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Render
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
------------------------------------------------------------------------------

renderSpec :: Spec
renderSpec = do
  describe "renderBlock" $ do
    it "renders a simple move" $ do
      let b = commandWithParams (GCmd G1) [PX 10.5, PY 20.3, PF 1200]
      renderBlock b `shouldBe` "G1 X10.5 Y20.3 F1200"

    it "renders integer values without decimals" $ do
      let b = commandWithParams (GCmd G0) [PX 10, PY 20]
      renderBlock b `shouldBe` "G0 X10 Y20"

    it "renders M commands" $ do
      let b = commandWithParams (MCmd M104) [PS 200]
      renderBlock b `shouldBe` "M104 S200"

    it "renders tool changes" $ do
      let b = commandBlock (TCmd 0)
      renderBlock b `shouldBe` "T0"

    it "renders with semicolon comments" $ do
      let b = (commandWithParams (GCmd G1) [PX 10])
            { _block_comment = Just "test" }
      renderBlock b `shouldBe` "G1 X10 ; test"

    it "renders with parenthetical comments" $ do
      let cfg = defaultRenderConfig { _renderConfig_commentStyle = Parenthetical }
      let b = (commandWithParams (GCmd G1) [PX 10])
            { _block_comment = Just "test" }
      renderBlockWith cfg b `shouldBe` "G1 X10 (test)"

    it "renders comment-only lines" $ do
      let b = commentBlock "just a comment"
      renderBlock b `shouldBe` "; just a comment"

    it "renders with line numbers" $ do
      let b = (commandBlock (GCmd G28))
            { _block_lineNumber = Just 1 }
      renderBlock b `shouldBe` "N1 G28"

  describe "renderProgramWith" $ do
    it "renders with checksums" $ do
      let cfg = defaultRenderConfig { _renderConfig_includeChecksums = True }
      let b = commandBlock (GCmd G28)
      let rendered = renderBlockWith cfg b
      -- Should have a * followed by a number
      '*' `elem` show rendered `shouldBe` True

    it "respects precision settings" $ do
      let cfg = defaultRenderConfig { _renderConfig_precision = 1 }
      let b = commandWithParams (GCmd G1) [PX 10.5678]
      renderBlockWith cfg b `shouldBe` "G1 X10.6"
