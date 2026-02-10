{-# LANGUAGE OverloadedStrings #-}
module BuildSpec (buildSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Build
import           GCode.Types.Block
import           GCode.Types.Command
import           GCode.Types.Parameter
import           GCode.Types.Program
------------------------------------------------------------------------------

buildSpec :: Spec
buildSpec = do
  describe "buildProgram" $ do
    it "builds a simple program" $ do
      let prog = buildProgram $ do
            homeAll
            absoluteMode
            linearMove [('X', 10), ('Y', 20), ('F', 1200)]
      length (_program_blocks prog) `shouldBe` 3

    it "builds temperature commands" $ do
      let blocks = buildBlocks $ do
            setBedTemp 60
            waitBedTemp 60
            setHotendTemp 200
            waitHotendTemp 200
      length blocks `shouldBe` 4
      _block_command (blocks !! 0) `shouldBe` Just (MCmd M140)
      _block_command (blocks !! 1) `shouldBe` Just (MCmd M190)
      _block_command (blocks !! 2) `shouldBe` Just (MCmd M104)
      _block_command (blocks !! 3) `shouldBe` Just (MCmd M109)

    it "builds fan commands" $ do
      let blocks = buildBlocks $ do
            fanOn 255
            fanOff
      _block_command (blocks !! 0) `shouldBe` Just (MCmd M106)
      _block_command (blocks !! 1) `shouldBe` Just (MCmd M107)

    it "builds comments" $ do
      let blocks = buildBlocks $ do
            comment "start of program"
            homeAll
      isCommentOnly (head blocks) `shouldBe` True

    it "builds a realistic 3D printing start sequence" $ do
      let prog = buildProgram $ do
            comment "Start G-code"
            setBedTemp 60
            setHotendTemp 200
            homeAll
            absoluteMode
            extruderAbsolute
            waitBedTemp 60
            waitHotendTemp 200
            comment "Priming"
            linearMove [('X', 0.1), ('Y', 20), ('Z', 0.3), ('F', 5000)]
            linearMove [('X', 0.1), ('Y', 200), ('Z', 0.3), ('F', 1500), ('E', 15)]
      commandCount prog `shouldBe` 10
