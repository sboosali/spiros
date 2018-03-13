{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

-- {-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
 -- to test inference

module UnitTests.WarningValidation where

--import "spiros" Spiros.WarningValidation
import qualified "spiros" Example.WarningValidation as Example

import "spiros" Prelude.Spiros

----------------------------------------

main :: IO ()
main = do
  let f = Example.example_validateNaturalRatio
  
  putStrLn "\n[successes]\n"
  print $ f 1 2
  print $ f 0 2
  
  putStrLn "\n[warnings]\n"
  print $ f (-1) (-2)
  
  putStrLn "\n[errors (accumulated)]\n"
  print $ f (-1) 0
  
  putStrLn "\n[errors]\n"
  print $ f 1 (-2)
  print $ f 1 0

----------------------------------------
