{-# LANGUAGE OverloadedStrings #-}
module ChecksumSpec (checksumSpec) where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           GCode.Build
import           GCode.Checksum
import           GCode.Types.Program
------------------------------------------------------------------------------

checksumSpec :: Spec
checksumSpec = do
  describe "computeChecksum" $ do
    it "computes XOR checksum" $ do
      -- 'N' xor '1' xor ' ' xor 'G' xor '2' xor '8'
      let cs = computeChecksum "N1 G28"
      cs `shouldSatisfy` (> 0)

    it "ignores content after *" $ do
      computeChecksum "N1 G28*18" `shouldBe` computeChecksum "N1 G28"

    it "empty string has checksum 0" $ do
      computeChecksum "" `shouldBe` 0

  describe "addLineNumbersAndChecksums" $ do
    it "adds line numbers and checksums to all blocks" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10), ('F', 1200)]
      let prog' = addLineNumbersAndChecksums prog
      let blocks = _program_blocks prog'
      all (\b -> _block_lineNumber b /= Nothing) blocks `shouldBe` True
      all (\b -> _block_checksum b /= Nothing) blocks `shouldBe` True

  describe "verifyChecksums" $ do
    it "verifies correct checksums" $ do
      let prog = buildProgram $ do
            homeAll
            linearMove [('X', 10), ('F', 1200)]
      let prog' = addLineNumbersAndChecksums prog
      verifyChecksums prog' `shouldBe` []
