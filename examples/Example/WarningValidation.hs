{-# LANGUAGE
    CPP,
    NoImplicitPrelude
  #-}

{-# LANGUAGE
    
    PackageImports
  #-}

{-| 

>>> -- errors
>>> example_validateNaturalRatio 1 0
WarningFailure [] ["the denominator must be non-zero"]
>>> example_validateNaturalRatio 1 (-2)
WarningFailure [] ["the ratio must be non-negative"]
>>> -- warnings
>>> example_validateNaturalRatio (-1) (-2)
WarningSuccess ["the numerator and denominator were both negative"] (1 % 2)
>>> -- successes
>>> example_validateNaturalRatio 1 2
WarningSuccess [] (1 % 2)
>>> example_validateNaturalRatio 0 2
WarningFailure [] ["the ratio must be non-negative"]

(Please see the source too)

-}
module Example.WarningValidation where

import Prelude.Spiros
import Spiros.WarningValidation

--------------------

import "base" Data.Ratio

----------------------------------------

example_validateNaturalRatio
  :: Integer
  -> Integer
  -> WarningValidation
     [String]
     [String]
     (Ratio Natural)

example_validateNaturalRatio n d
  | not (d /= 0)               = failure0 "the denominator must be non-zero"
  | not (signum n == signum d) = failure0 "the ratio must be non-negative"
  | otherwise                  = success r <* warning 
     ( if   not ((n >= 0) && (d >= 0))
       then ["the numerator and denominator were both negative"]
       else []
     )
  where
  r  = n' % d' :: Ratio Natural
  n' = fromIntegral (abs n)
  d' = fromIntegral (abs d)

----------------------------------------

{-
validateNaturalNumber :: Integer -> WarningValidation w [String] Natural
validateNaturalNumber i =
  if (i>=0)
  then success n
  else failure0 "must be non-negative"
  where
  n = fromIntegral i

validatePositiveNumber :: Integer -> WarningValidation w [String] Natural
validatePositiveNumber i =
  if (i>=1)
  then success n
  else failure0 "must be positive"
  where
  n = fromIntegral i

validateNonZeroNumber :: Integer -> WarningValidation w [String] Integer
validateNonZeroNumber i =
  if (i/=0)
  then success n
  else failure0 "must be non-zero"
  where
  n = fromIntegral i


  n = n'
  d = d'
  

validateNaturalFraction
  :: Integer
  -> Integer
  -> WarningValidation [String] [String] (Ratio Natural)
validateNaturalFraction n d = do
  go <*> validateNatural n <*> validatePositive d

  where
  go n d = (%) 
  
  if (n>=0) && (n<=)
  then success n  
  warning0 "the numerator and denominator share the same sign, but both are negative"



validateNaturalFraction
  :: Integer
  -> Integer
  -> WarningValidation [String] [String] (Ratio Natural)
validateNaturalFraction n d = do
  
  if   (d==0) 
  then failure0 "the denominator must be non-zero"
  else failure0 "the ratio must be non-negative" 
  warning0 "the numerator and denominator were both negative"
  
  else success n

  -- the numerator and denominator share the same sign, but both were negative
  --  i.e. the numerator and denominator must both share the same sign"



validateNaturalFraction
  :: Integer
  -> Integer
  -> WarningValidation [String] [String] (Ratio Natural)
validateNaturalFraction n d
  | not (d /= 0)         = failure0 "the denominator must be non-zero"
  | not (sig n == sig d) = failure0 "the ratio must be non-negative"
  | (n >= 0) && (d >= 0) = (%) (fromIntegral n) (fromIntegral d)
  
  | otherwise = (%) <$> <*> warning0 "the numerator and denominator were both negative"
  
  else success n


validateNaturalFraction
  :: Integer
  -> Integer
  -> WarningValidation [String] [String] (Ratio Natural)
validateNaturalFraction n d
  | not (d /= 0)               = failure0 "the denominator must be non-zero"
  | not (signum n == signum d) = failure0 "the ratio must be non-negative"
  | otherwise                  = success r *> 
  where
  r = (fromIntegral n % fromIntegral d)
  w = warning $
     if   ((n >= 0) && (d >= 0))
     then []
     else ["the numerator and denominator were both negative"]


-}

----------------------------------------
