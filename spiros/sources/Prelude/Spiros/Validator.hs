{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards, PackageImports, LambdaCase, PatternSynonyms, BangPatterns #-}
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

{- |

See 'validate'.

'validateNatural' is a demonstration of it that's type-checked and @doctest@ed
(as well as being useful to me). 

-}
module Prelude.Spiros.Validator where

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities
--import Prelude.Spiros.GUI
import Prelude.Spiros.Exception

--

----------------------------------------

{-| Represents a validator as something that injects a type into another type, with the possibility of failure. 

Equivalent to:

@
Validator a b ≡ (a -> 'Possibly' b)
Validator a b ≡ (∀m. ('MonadThrow' m) => Kleisli m a b)
@

Specializations:

@
Validator a b ~ (a -> Maybe                b)
Validator a b ~ (a -> []                   b)
Validator a b ~ (a -> Either SomeException b)
Validator a b ~ (a -> IO                   b)
...
@

Usage:

@
-- x :: a
return x :: Validator a a
@

-}
type Validator a b = forall m. (MonadThrow m) => a -> m b

{-| Represents a value that has possibly failed ("or" will possibly fail).

Specializations:

@
Possibly b ~ Maybe    b
Possibly b ~         [b]
Possibly b ~ Either _ b
Possibly b ~ IO       b
...
@

-}
type Possibly b = forall m. (MonadThrow m) => m b

----------------------------------------

{-| 

Parameters:

@
name = validator name check display cast :: 'Validator' _ _
@

e.g. validating naturals:

@
validateNatural :: 'Validator' Integer Natural
validateNatural = validator 'natural
 (\\i -> i >= 0)
 (\\i -> i ++ " must be non-negative")
 (\\i -> fromIntegral i)
@

is the same as the explicit:

@
validateNatural :: ('MonadThrow' m) => Integer -> m Natural
validateNatural i
 | i >= 0    = return $ fromIntegral i
 | otherwise = 'throwN' \'validateNatural $ "must be non-negative"
@

and as the point-free styled:

@
validateNatural :: Integer -> 'Possibly' Natural
validateNatural = validator 'natural
 (>= 0)
 (++ " must be non-negative")
 (fromIntegral)
@

Wraps 'throwN'.

-}
validator
  :: ( MonadThrow m
     , Show a
     )
  => HaskellName
  -> (a -> Bool)
  -> (String -> String)
  -> (a -> b)
  -> (a -> m b)
validator name check display cast = \x ->
  if check x
  then return $ cast x
  else throwN name $ display (show x)

{-# inline validator #-}

{-| More convenient, but less informative, than 'validator'. 

Parameters:

@
name = validator_ \'name check cast :: 'Validator' _ _
@

e.g. validating naturals:

@
validateNatural :: Integer -> 'Possibly' Natural
validateNatural = validator 'validateNatural (>= 0) fromIntegral
@

Wraps 'throwN_'.

-}
validator_
  :: ( MonadThrow m
     )
  => HaskellName
  -> (a -> Bool)
  -> (a -> b)
  -> (a -> m b)
validator_ name check cast = \x ->
  if   check x
  then return $ cast x
  else throwN_ name 

{-# inline validator_ #-}

----------------------------------------

{-| 

>>> validateNatural 2
2
>>> validateNatural (-2) :: Maybe Natural
Nothing

@
> validateNatural (-2)

*** Exception: 
...
-2 must be non-negative
...
@

Specializations of @i@:

@
validateNatural @Int     :: 'Validator' Int     Natural
validateNatural @Integer :: 'Validator' Integer Natural
validateNatural @Natural :: 'Validator' Natural Natural
...
@

Specializations of @m@:

@
validateNatural @Int @Maybe      :: Integer ->                Maybe Natural
validateNatural @Int @(Either _) :: Integer -> Either SomeException Natural
validateNatural @Int @[]         :: Integer ->                     [Natural]
validateNatural @Int @IO         :: Integer ->                   IO Natural
@

Definition:

@
validateNatural :: forall i m. ...
validateNatural = 'validator' \'validateNatural
 (>= 0)
 (++ " must be non-negative")
 (fromIntegral)
@

-}
validateNatural
  :: forall i m.
     ( Integral i
     , Show i
     )
  => ( MonadThrow m
     )
  => (i -> m Natural)
validateNatural = validator 'validateNatural
 (>= 0)
 (++ " must be non-negative")
 (fromIntegral)

{-# inline validateNatural #-}

----------------------------------------
