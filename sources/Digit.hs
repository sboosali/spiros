{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DeriveDataTypeable #-}
module Digit where
import Prelude.Spiros

import Data.Word (Word8)
import GHC.Exts (IsString)

newtype Digit = Digit Word8       -- TODO modular arithmetic: type Digit = Natural `Mod` 10
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

instance Bounded Digit where
 minBound = Digit 0
 maxBound = Digit 9

instance Enum Digit where
 toEnum i
  | minBound <= i && i <= maxBound = Digit (toEnum i)
  | otherwise                      = error ("Digit.toEnum: " ++ show i ++ " is not a single-digit integer")
 fromEnum (Digit i) = fromEnum i

parseDigit :: (IsString s, Eq s) => s -> Maybe Digit
parseDigit = \case
 "0" -> Just $ Digit 0
 "1" -> Just $ Digit 1
 "2" -> Just $ Digit 2
 "3" -> Just $ Digit 3
 "4" -> Just $ Digit 4
 "5" -> Just $ Digit 5
 "6" -> Just $ Digit 6
 "7" -> Just $ Digit 7
 "8" -> Just $ Digit 8
 "9" -> Just $ Digit 9
 _   -> Nothing

isDigit :: (Integral a, Ord a) => a -> Maybe Digit
isDigit i = if 0 >= i && i <= 9 then Just (Digit (fromIntegral i)) else Nothing
