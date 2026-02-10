module Main where

------------------------------------------------------------------------------
import           System.Random
import           Test.Hspec
------------------------------------------------------------------------------
import           CommandSpec
import           ParseSpec
import           RenderSpec
import           BuildSpec
import           ValidateSpec
import           ChecksumSpec
import           RoundTripSpec
import           PortedParseSpec
import           PortedRenderSpec
import           SmokeSpec
------------------------------------------------------------------------------

main :: IO ()
main = do
  setStdGen $ mkStdGen 42
  hspec $ do
    describe "CommandSpec" commandSpec
    describe "ParseSpec" parseSpec
    describe "RenderSpec" renderSpec
    describe "BuildSpec" buildSpec
    describe "ValidateSpec" validateSpec
    describe "ChecksumSpec" checksumSpec
    describe "RoundTripSpec" roundTripSpec
    describe "PortedParseSpec" portedParseSpec
    describe "PortedRenderSpec" portedRenderSpec
    describe "SmokeSpec" smokeSpec
