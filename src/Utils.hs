{-# LANGUAGE OverloadedStrings #-}
module Utils where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric (showFFloat)
------------------------------------------------------------------------------

class Pretty a where
  pretty
    :: a -> Text

-- | Format a Double to a given number of decimal places, trimming trailing
-- zeros after the decimal point.
formatDouble
  :: Int
  -> Double
  -> Text
formatDouble prec d = T.pack $ trimTrailingZeros $ showFFloat (Just prec) d ""

-- | Trim trailing zeros after decimal point, and the decimal point itself
-- if no fractional part remains.
trimTrailingZeros
  :: String
  -> String
trimTrailingZeros s =
  case break (== '.') s of
    (_, "") -> s
    (intPart, fracPart) ->
      let trimmed = dropWhileEnd' (== '0') fracPart
       in if trimmed == "."
            then intPart
            else intPart ++ trimmed

-- | dropWhileEnd for lists (available in base >= 4.19 but we define our own
-- for compatibility).
dropWhileEnd'
  :: (Char -> Bool)
  -> String
  -> String
dropWhileEnd' p = reverse . dropWhile p . reverse

-- | Format an Int parameter value. If the value is actually an integer
-- (no fractional part), render without decimal point.
formatParamValue
  :: Int
  -- ^ Precision
  -> Double
  -> Text
formatParamValue prec d =
  if d == fromIntegral (round d :: Int)
    then T.pack $ show (round d :: Int)
    else formatDouble prec d
