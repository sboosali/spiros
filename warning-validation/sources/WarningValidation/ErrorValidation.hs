{-# LANGUAGE CPP #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE
    PackageImports,
    DeriveDataTypeable,
    DeriveGeneric,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
   
    GeneralizedNewtypeDeriving,
    TypeFamilies,
    LambdaCase,
    TypeOperators,
    AutoDeriveTypeable
 #-}

{-| 'ErrorValidation' is a 'WarningValidation' with trivial @w@, and thus has no warnings. 

-}
module Spiros.WarningValidation.ErrorValidation
 ( module Spiros.WarningValidation.ErrorValidation
 ) where

--import qualified Spiros.WarningValidation as V
--import           Spiros.WarningValidation (Warnings,Errors)
import           Spiros.WarningValidation (WarningValidation(..))

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities

----------------------------------------

----------------------------------------

{-| Validation without warnings, with errors only.

The warnings type, which is a factor in a product type, is unit, and thus is ignored. 

a Bifunctor

Naming: 

-}

newtype ErrorValidation e a = ErrorValidation
 { getErrorValidation :: WarningValidation () e a
 }

----------------------------------------
-- public helpers

{-| Succeed. 

-}
success
  :: ( 
     )
  => a
  -> ErrorValidation e a
success = ErrorValidation < WarningSuccess mempty

{-# INLINE success #-}

{-| Fail. 

-}
failure
  :: ( 
     )
  => e
  -> ErrorValidation e a
failure = ErrorValidation < WarningFailure mempty

{-# INLINE failure #-}

----------------------------------------

{-| Fail, by injecting the argument into some container via 'pure'. 

-}
failureF
  :: ( Applicative f
     )
  => e
  -> ErrorValidation (f e) a
failureF e = failure (pure e)

{-| Fail via @[]@. 

-}
failure0
  :: ( 
     )
  => e
  -> ErrorValidation [e] a
failure0 = failureF -- (:[])

{-# INLINE failure0 #-}

{-| Fail via @NonEmpty@.

-}
failure1
  :: ( 
     )
  => e
  -> ErrorValidation (NonEmpty e) a
failure1 = failureF -- (:|[])

{-# INLINE failure1 #-}

----------------------------------------

failureIf
  :: (a -> Bool)
  -> (a -> e)
  -> a
  -> ErrorValidation e a
failureIf predicate render = go
  where
  go a =
    if   predicate a
    then success   a
    else failure (render a)

failureUnless
  :: (a -> Bool)
  -> (a -> e)
  -> a
  -> ErrorValidation e a
failureUnless predicate =
  failureIf (predicate > not)

----------------------------------------

-- either2validation
--   :: Either          e a
--   -> ErrorValidation e a
-- either2validation = either failure success

-- | Converts from 'Either' to 'Validation'.
fromEither
  :: Either          e a
  -> ErrorValidation e a
fromEither = either failure success
  
-- | Converts from 'Validation' to 'Either'.
toEither
  :: ErrorValidation e a
  -> Either          e a
toEither = errorValidation Left Right

-- | Run a function on anything with a Validate instance (usually Either)
-- as if it were a function on Validation
--
asEither
  :: (Either          e a -> Either          e' a')
  -> (ErrorValidation e a -> ErrorValidation e' a')
asEither f = g
  where
  g = toEither > f > fromEither

-- | 'liftError' is useful for converting an 'Either' to an 'Validation'
-- when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
liftError :: (b -> e) -> Either b a -> ErrorValidation e a
liftError f = either (failure . f) success

----------------------------------------

{-
-- | 'validation' is the catamorphism for @Validation@.
validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation ec ac v = case v of
  Failure e -> ec e
  Success a -> ac a
-}

-- | The eliminator, a.k.a. catamorphism, for the 'ErrorValidation'. 
errorValidation
  :: (e                   -> r) -- ^ handle failure
  -> (a                   -> r) -- ^ handle success
  -> (ErrorValidation e a -> r)
errorValidation f g = getErrorValidation > \case
  WarningFailure () e -> f e
  WarningSuccess () a -> g a

----------------------------------------

-- | Return the @a@ or run the given function over the @e@.
--
valueOr
  :: (e -> a)
  -> (ErrorValidation e a -> a)
valueOr k v = v & errorValidation k id 

-- | extract the 'value' out of either side,
-- when the error type is the return type too. 
value :: ErrorValidation a a -> a
value = valueOr id
  
-- | @= 'value'@
--
-- Naming: afaik, it's like the opposite of the "diagonal function":
-- @diagonal x = (x,x)@
-- 
codiagonal :: ErrorValidation a a -> a
codiagonal = value

----------------------------------------

-- | @v `'orElse'` a@ returns @a@ when @v@ is Failure, and the @a@ in @Success a@.
--
orElse
  :: ErrorValidation e a
  -> (a -> a)
orElse v a = v & errorValidation (const a) id 

----------------------------------------

{- | Validates an @a@ with the given predicate @p@, returning @e@ if the predicate does not hold.

@
'fromPredicate' e p a
@

-}
fromPredicate
  :: e
  -> (a -> Bool)
  -> a
  -> ErrorValidation e a
fromPredicate e p = \a ->
  if   p a
  then success a
  else failure e
{-# INLINE fromPredicate #-}

-- | 'ensure' leaves the validation unchanged when the predicate holds, or
-- fails with @e@ otherwise.
--
ensure
  :: e
  -> (a -> Bool)
  -> ErrorValidation e a
  -> ErrorValidation e a
ensure e p =
  errorValidation failure (fromPredicate e p)

----------------------------------------

validate
  :: (a -> Either          e b)
  -> (a -> ErrorValidation e b)
validate check = \a -> fromEither (check a)

{-|

e.g.

@
-- a ~ Integer
-- b ~ Natural

-- e ~ String

-- f ~ NonEmpty

validate1
  :: (Integer -> Either                      String  Natural)
  -> (Integer -> WarningValidation (NonEmpty String) Natural)
@

-}
validate1
  :: ( Applicative f
     )
  => (a -> Either            (e) b)
  -> (a -> ErrorValidation (f e) b)
validate1 check = 
  validate (check > bimap pure id)

----------------------------------------

-- | @bind@ binds through an ErrorValidation, which is useful for
-- composing ErrorValidations sequentially. Note that despite having a bind
-- function of the correct type, ErrorValidation is not a monad.
-- The reason is, this bind does not accumulate errors, so it does not
-- agree with the Applicative instance.
--
-- There is nothing wrong with using this function, it just does not make a
-- valid @Monad@ instance.
bind
  ::       ErrorValidation e a
  -> (a -> ErrorValidation e b)
  ->       ErrorValidation e b
bind v k = errorValidation failure k v

----------------------------------------
