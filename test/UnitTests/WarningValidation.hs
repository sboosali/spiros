{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

-- {-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
 -- to test inference

module UnitTests.WarningValidation where

import "base" Data.Ratio (Ratio,(%))

import "spiros" Spiros.WarningValidation
import "spiros" Prelude.Spiros

main = do
  
  putStrLn "\nerrors\n"
  print $ validateNaturalRatio 1 0
  print $ validateNaturalRatio 1 (-2)
  
  putStrLn "\naccumulated errors\n"
  print $ validateNaturalRatio (-1) 0
  
  putStrLn "\nwarnings\n"
  print $ validateNaturalRatio (-1) (-2)
  
  putStrLn "\nsuccesses\n"
  print $ validateNaturalRatio 1 2
  print $ validateNaturalRatio 0 2

validateNaturalRatio
  :: Integer
  -> Integer
  -> WarningValidation
     [String]
     [String]
     (Ratio Natural)

validateNaturalRatio n d
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

{-NOTES

-}
