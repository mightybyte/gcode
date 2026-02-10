{-# LANGUAGE OverloadedStrings #-}
module CommandSpec (commandSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Types.Command
import           Utils
------------------------------------------------------------------------------

commandSpec :: Spec
commandSpec = do
  describe "GCommand" $ do
    it "round-trips through code" $ do
      gCommandFromCode (gCommandCode G0) `shouldBe` G0
      gCommandFromCode (gCommandCode G1) `shouldBe` G1
      gCommandFromCode (gCommandCode G28) `shouldBe` G28
      gCommandFromCode (gCommandCode G92) `shouldBe` G92

    it "renders known commands" $ do
      pretty G0 `shouldBe` "G0"
      pretty G1 `shouldBe` "G1"
      pretty G28 `shouldBe` "G28"

    it "handles custom codes" $ do
      gCommandFromCode 999 `shouldBe` GCustom 999
      gCommandCode (GCustom 999) `shouldBe` 999
      pretty (GCustom 999) `shouldBe` "G999"

  describe "MCommand" $ do
    it "round-trips through code" $ do
      mCommandFromCode (mCommandCode M104) `shouldBe` M104
      mCommandFromCode (mCommandCode M109) `shouldBe` M109
      mCommandFromCode (mCommandCode M190) `shouldBe` M190

    it "renders known commands" $ do
      pretty M104 `shouldBe` "M104"
      pretty M107 `shouldBe` "M107"

    it "handles custom codes" $ do
      mCommandFromCode 888 `shouldBe` MCustom 888
      pretty (MCustom 888) `shouldBe` "M888"

  describe "Command" $ do
    it "renders all variants" $ do
      pretty (GCmd G1) `shouldBe` "G1"
      pretty (MCmd M104) `shouldBe` "M104"
      pretty (TCmd 0) `shouldBe` "T0"
      pretty (TCmd 3) `shouldBe` "T3"
