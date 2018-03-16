{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedLists #-}

module UnitTests.WarningValidation where

--import "spiros" Spiros.WarningValidation
import qualified "spiros" Example.WarningValidation as Example

import "tasty"       Test.Tasty
import "tasty-hunit" Test.Tasty.HUnit

import "spiros" Prelude.Spiros

----------------------------------------

main = defaultMain $ do
  testCase "Example test case" $ do
    
    2 + 2 @?= 4      -- `actual`   on left
    
    6     @=? 3 + 3  -- `expected` on left
    
    assertBool "the list is not empty" $ null []

{-
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
-}
