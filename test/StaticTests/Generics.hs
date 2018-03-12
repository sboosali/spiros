{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, PackageImports #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
    -- to test inference

module StaticTests.Generics where

import "spiros" Prelude.Spiros
 
data T a = T a (Maybe a)
  deriving (Generic,Eq,Show)

instance (Monoid a) => Monoid (T a) where
  mempty  = memptyGeneric
  mappend = mappendGeneric

-- instance (Semigroup a) => Semigroup (T a) where
--   (<>) = sappendGeneric

-- instance (Semigroup a, Monoid a) => Monoid (T a) where
--   mempty  = memptyGeneric
--   mappend = (<>)

main = do
  assert $ (T "a" (Just "x") `mappend` T "b" Nothing `mappend` mempty) == T "ab" Nothing
  --print $ (T "a" (Just "x") <> T "b" Nothing) == T "ab" Nothing
  --TODO assert $

{-NOTE

assert :: Bool -> a -> a 

If the first argument evaluates to True, then the result is the second argument. Otherwise an AssertionFailed exception is raised, containing a String with the source file and line number of the call to assert.

Assertions can normally be turned on or off with a compiler flag (for GHC, assertions are normally on unless optimisation is turned on with -O or the -fignore-asserts option is given). When assertions are turned off, the first argument to assert is ignored, and the second argument is returned as the result.

-}
