{-# LANGUAGE OverloadedStrings #-}
module ValidateSpec (validateSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Build
import           GCode.Validate
------------------------------------------------------------------------------

validateSpec :: Spec
validateSpec = do
  describe "validate" $ do
    it "warns about move before homing" $ do
      let prog = buildProgram $ do
            linearMove [('X', 10), ('F', 1200)]
      let warnings = validate prog
      NoHomingBeforeMove `elem` warnings `shouldBe` True

    it "no homing warning when homed first" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10), ('F', 1200)]
      let warnings = validate prog
      NoHomingBeforeMove `elem` warnings `shouldBe` False

    it "warns about no temperature set" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10), ('F', 1200)]
      let warnings = validate prog
      NoTemperatureSet `elem` warnings `shouldBe` True

    it "warns about extrusion before heat" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10), ('E', 1), ('F', 1200)]
            setHotendTemp 200
      let warnings = validate prog
      ExtrudeBeforeHeat `elem` warnings `shouldBe` True

    it "warns about missing feedrate" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10)]
      let warnings = validate prog
      MissingFeedrate `elem` warnings `shouldBe` True

    it "no feedrate warning when F is set" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10), ('F', 1200)]
      let warnings = validate prog
      MissingFeedrate `elem` warnings `shouldBe` False

  describe "validateWithBounds" $ do
    it "warns about out-of-bounds moves" $ do
      let bounds = BoundingBox 0 200 0 200 0 200
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 300), ('F', 1200)]
      let warnings = validateWithBounds (Just bounds) prog
      any isOutOfBounds warnings `shouldBe` True

    it "no bounds warning for in-bounds moves" $ do
      let bounds = BoundingBox 0 200 0 200 0 200
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 100), ('Y', 100), ('F', 1200)]
      let warnings = validateWithBounds (Just bounds) prog
      any isOutOfBounds warnings `shouldBe` False

isOutOfBounds :: Warning -> Bool
isOutOfBounds (MoveOutOfBounds _ _) = True
isOutOfBounds _ = False
