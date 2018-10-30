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

{-| 'SimpleWarningValidation' is a less-polymorphic 'WarningValidation'. 


-}
module Spiros.WarningValidation.Simple
 ( -- module Spiros.WarningValidation.Simple
 ) where


{-

import qualified Spiros.WarningValidation as V
import           Spiros.WarningValidation (Warnings,Errors)
import           Spiros.WarningValidation (WarningValidation(..))

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities
  
----------------------------------------

----------------------------------------

{- |

see 'runWarningValidation':

@
WarningValidation w e a
~
(w, Either e a)
@

NOTE, the 'Monoid'(\/ 'Semigroup') instance(s) and the 'Alternative' instance differ:

* The 'Alternative' instance is logical disjunction, i.e. "succeed if /any/ succeed". The operation is "try the next if this one fails", and its identity is failure. 
* The 'Monoid' instance is logical conjunction, i.e. "succeed if /all/ succeed". The operation is "try each to make sure that none fail, then merge all the results", and its identity is success.

This difference exploits the different arities between these two "monoidal typeclasses":

* @Monoid (WarningValidation w e a) can require constraints on @a@, in particular @(Monoid a, ...) => ...@, 
* while @Alternative (WarningValidation w e)@ can't. 

-}
type SimpleWarningValidation = V.WarningValidation Warnings Errors

----------------------------------------
-- public helpers

{-| Succeed without warnings. 

@
success a
=
'WarningSuccess' 'mempty' a
@

-}
success
  :: ( Monoid w
     )
  => a -> WarningValidation w e a
success = WarningSuccess mempty

{-# INLINE success #-}

{-| Fail without warnings. 

@
failure e
=
'WarningFailure' 'mempty' e
@

-}
failure
  :: ( Monoid w
     )
  => e -> WarningValidation w e a
failure = WarningFailure mempty

{-# INLINE failure #-}

{-| Warn, trivially succeeding. 

@
warning w
=
'WarningSuccess' w ()
@

-}
warning
  :: (
     )
  => w -> WarningValidation w e ()
warning w = WarningSuccess w ()

{-# INLINE warning #-}

----------------------------------------

{-| Fail via @[]@, without warnings. 

@
failure0 e
=
'failure' [e]
@

-}
failure0
  :: ( Monoid w
     )
  => e -> WarningValidation w [e] a
failure0 e = failure [e]

{-# INLINE failure0 #-}

{-| Fail via @NonEmpty@, without warnings. 

@
failure1 e
=
'failure' [e]
@

-}
failure1
  :: ( Monoid w
     )
  => e -> WarningValidation w (NonEmpty e) a
failure1 e = failure (e:|[])

{-# INLINE failure1 #-}

{-| Warn, via @[]@, trivially succeeding. 

@
warning0 w
=
'warning' [w]
@

-}
warning0
  :: (
     )
  => w -> WarningValidation [w] e ()
warning0 w = warning [w]

{-# INLINE warning0 #-}

----------------------------------------

{-| Succeed, but with warnings. 

@
success w a
=
'WarningSuccess' w a
@

-}
successBut
  :: ( 
     )
  => w -> a -> WarningValidation w e a
successBut = WarningSuccess

{-# INLINE successBut #-}

{-| Succeed, but with a warning. 

@
successBut w a
=
'WarningSuccess' [w] a
@

-}
successBut0
  :: ( 
     )
  => w -> a -> WarningValidation [w] e a
successBut0 w = successBut [w]

{-# INLINE successBut0 #-}

{-| Fail with errors, and with warnings too. 

@
failureAnd w e
=
'WarningFailure' w e
@

-}
failureAnd
  :: ( 
     )
  => w -> e -> WarningValidation w e a
failureAnd = WarningFailure

{-# INLINE failureAnd #-}

{-| Fail with an error, via @[]@, and with a warning too. 

@
failureAnd0 w e
=
'WarningFailure' [w] [e]
@

-}
failureAnd0
  :: ( 
     )
  => w -> e
  -> WarningValidation [w] [e] a
failureAnd0 w e = WarningFailure [w] [e]

{-# INLINE failureAnd0 #-}

{-| Fail with an error, via @NonEmpty@, and with a warning too.  

@
failureAnd1 w e
=
'WarningFailure' [w] (e:|[])
@

-}
failureAnd1
  :: ( 
     )
  => w -> e
  -> WarningValidation [w] (NonEmpty e) a
failureAnd1 w e = WarningFailure [w] (e:|[])

{-# INLINE failureAnd1 #-}

----------------------------------------

failureIf
  :: ( Monoid w
     )
  => (a -> Bool)
  -> (a -> e)
  -> a
  -> WarningValidation w e a
failureIf predicate render = \a ->
  if   predicate a
  then success   a
  else failure (render a)

failureUnless
  :: ( Monoid w
     )
  => (a -> Bool)
  -> (a -> e)
  -> a
  -> WarningValidation w e a
failureUnless predicate =
  failureIf (predicate > not)

warningIf
  :: ( Monoid w
     )
  => (a -> Bool)
  -> (a -> w)
  -> a
  -> WarningValidation w e a
warningIf predicate render = \a ->
  let
    w =
      if   predicate a
      then mempty
      else render a
  in
    WarningSuccess w a

{-  
warningIf predicate render = \a ->
  if   predicate  a
  then success    a
  else successBut w
  where
  w = (render a)
-}

warningUnless
  :: ( Monoid w
     )
  => (a -> Bool)
  -> (a -> w)
  -> a
  -> WarningValidation w e a
warningUnless predicate =
  warningIf (predicate > not)

----------------------------------------

validate
  :: ( 
     )
  => (a -> w)
  -> (a -> Either e b)
  -> a
  -> WarningValidation w e b
validate softCheck hardCheck = go
  where
  go a
      = hardCheck a
      & either (WarningFailure w) (WarningSuccess w)
      where
      w = softCheck a
  
validateOnlyError
  :: ( Monoid w
     )
  => (a -> Either e b)
  -> a
  -> WarningValidation w e b
validateOnlyError =
  validate (const mempty)

validateOnlyError'
  :: ( Monoid e
     , Monoid w
     )
  => (a -> Maybe b)
  -> a
  -> WarningValidation w e b
validateOnlyError' hardCheck =
  validateOnlyError (hardCheck > maybe2either mempty)

validateOnlyWarning
  :: ( 
     )
  => (a -> w)
  -> (a -> b)
  -> a
  -> WarningValidation w e b
validateOnlyWarning softCheck noCheck =
  validate softCheck (noCheck > Right)

validateOnlyWarning'
  :: ( 
     )
  => (a -> w)
  -> a
  -> WarningValidation w e a
validateOnlyWarning' softCheck =
  validateOnlyWarning softCheck id

{-
validateOnlyWarning' softCheck =
  validate softCheck Right
-}

----------------------------------------

{-|


e.g.

@
-- a ~ Integer
-- b ~ Natural

-- e ~ String
-- w ~ String

-- f ~ []
-- g ~ NonEmpty

validate1
  :: (Integer -> String)
  -> (Integer -> Either String Natural)
  -> Integer
  -> WarningValidation [String] (NonEmpty String) Natural
@

-}
validate1
  :: ( Applicative f
     , Applicative g
     --, Alternative f
     )
  => (a -> w)
  -> (a -> Either e b)
  -> a
  -> WarningValidation (f w) (g e) b
validate1 softCheck hardCheck = 
  validate (softCheck > pure) (hardCheck > bimap pure id)

{-# SPECIALIZE validate1
  :: (a -> w)
  -> (a -> Either e b)
  -> a
  -> WarningValidation [w] [e] b
  #-}

{-# SPECIALIZE validate1
  :: (a -> w)
  -> (a -> Either e b)
  -> a
  -> WarningValidation [w] (NonEmpty e) b
  #-}

----------------------------------------



----------------------------------------

{-NOTES

instance Applicative NonEmpty where
  pure a = a :| []

-}

----------------------------------------


{-|

@
runWarningValidation = \case
  'WarningFailure' w e -> (w, 'Left'  e)
  'WarningSuccess' w a -> (w, 'Right' a)
@

-}
runWarningValidation
  :: (
     )
  => WarningValidation w e a
  -> (w, Either e a)
runWarningValidation = \case
  WarningFailure w e -> (w, Left  e)
  WarningSuccess w a -> (w, Right a)

{-# INLINE runWarningValidation #-}

{-| Promote warnings into errors (i.e. the severity is raised from non-fatal to fatal). 

to succeed with @Right a@:

* both the 'WarningValidation' must a 'WarningSuccess',
* and the warnings must be empty (i.e. @('==' 'mempty')@). 


-}
runErrorValidation
  :: ( Eq w, Monoid w
     )
  => WarningValidation w e a
  -> Either (w, Maybe e) a
runErrorValidation = runWarningValidation > \case
  (w, Left  e) -> Left (w, Just e)
  (w, Right a) ->
    
    if   w == mempty
    then Right a
    else Left (w, Nothing)

{-# INLINE runErrorValidation #-}

{- | Validates an @a@ with the given predicate @p@, returning @e@ if the predicate does not hold.

@
'predicate2validation' e p a
@

-}
predicate2validation
  :: ( Monoid w
     )
  => e -> (a -> Bool)
  -> a -> WarningValidation w e a
predicate2validation e p = \a ->
  if   p a
  then success a
  else failure e
{-# INLINE predicate2validation #-}
 



-- | 'validate's the @a@ with the given predicate, returning @e@ if the predicate does not hold.
--
-- This can be thought of as having the less general type:
--
-- @
-- validate :: e -> (a -> Bool) -> a -> Validation e a
-- @
validate :: Validate v => e -> (a -> Bool) -> a -> v e a
validate e p a =
  if p a then _Success # a else _Failure # e

-- | 'validationNel' is 'liftError' specialised to 'NonEmpty' lists, since
-- they are a common semigroup to use.
validationNel :: Either e a -> Validation (NonEmpty e) a
validationNel = liftError pure

-- | Converts from 'Either' to 'Validation'.
fromEither :: Either e a -> Validation e a
fromEither = liftError id

-- | 'liftError' is useful for converting an 'Either' to an 'Validation'
-- when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
liftError :: (b -> e) -> Either b a -> Validation e a
liftError f = either (Failure . f) Success

-- | 'validation' is the catamorphism for @Validation@.
validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation ec ac v = case v of
  Failure e -> ec e
  Success a -> ac a

-- | Converts from 'Validation' to 'Either'.
toEither :: Validation e a -> Either e a
toEither = validation Left Right

-- | @v 'orElse' a@ returns @a@ when @v@ is Failure, and the @a@ in @Success a@.
--
-- This can be thought of as having the less general type:
--
-- @
-- orElse :: Validation e a -> a -> a
-- @
orElse :: Validate v => v e a -> a -> a
orElse v a = case v ^. _Validation of
  Failure _ -> a
  Success x -> x

-- | Return the @a@ or run the given function over the @e@.
--
-- This can be thought of as having the less general type:
--
-- @
-- valueOr :: (e -> a) -> Validation e a -> a
-- @
valueOr :: Validate v => (e -> a) -> v e a -> a
valueOr ea v = case v ^. _Validation of
  Failure e -> ea e
  Success a -> a

-- | 'codiagonal' gets the value out of either side.
codiagonal :: Validation a a -> a
codiagonal = valueOr id

-- | 'ensure' leaves the validation unchanged when the predicate holds, or
-- fails with @e@ otherwise.
--
-- This can be thought of as having the less general type:
--
-- @
-- ensure :: e -> (a -> Bool) -> Validation e a -> Validation e a
-- @
ensure :: Validate v => e -> (a -> Bool) -> v e a -> v e a
ensure e p =
  over _Validation $ \v -> case v of
    Failure x -> Failure x
    Success a -> validate e p a

-- | Run a function on anything with a Validate instance (usually Either)
-- as if it were a function on Validation
--
-- This can be thought of as having the type
--
-- @(Either e a -> Either e' a') -> Validation e a -> Validation e' a'@
validationed :: Validate v => (v e a -> v e' a') -> Validation e a -> Validation e' a'
validationed f = under _Validation f

-- | @bindValidation@ binds through an Validation, which is useful for
-- composing Validations sequentially. Note that despite having a bind
-- function of the correct type, Validation is not a monad.
-- The reason is, this bind does not accumulate errors, so it does not
-- agree with the Applicative instance.
--
-- There is nothing wrong with using this function, it just does not make a
-- valid @Monad@ instance.
bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation v f = case v of
  Failure e -> Failure e
  Success a -> f a

-- | The @Validate@ class carries around witnesses that the type @f@ is isomorphic
-- to Validation, and hence isomorphic to Either.
class Validate f where
  _Validation ::
    Iso (f e a) (f g b) (Validation e a) (Validation g b)

  _Either ::
    Iso (f e a) (f g b) (Either e a) (Either g b)
  _Either =
    iso
      (\x -> case x ^. _Validation of
        Failure e -> Left e
        Success a -> Right a)
      (\x -> _Validation # case x of
        Left e -> Failure e
        Right a -> Success a)
  {-# INLINE _Either #-}

instance Validate Validation where
  _Validation =
    id
  {-# INLINE _Validation #-}
  _Either =
    iso
      (\x -> case x of
        Failure e -> Left e
        Success a -> Right a)
      (\x -> case x of
        Left e -> Failure e
        Right a -> Success a)
  {-# INLINE _Either #-}

instance Validate Either where
  _Validation =
    iso
      fromEither
      toEither
  {-# INLINE _Validation #-}
  _Either =
    id
  {-# INLINE _Either #-}

-- | This prism generalises 'Control.Lens.Prism._Left'. It targets the failure case of either 'Either' or 'Validation'.
_Failure ::
  Validate f =>
  Prism (f e1 a) (f e2 a) e1 e2
_Failure =
  prism
    (\x -> _Either # Left x)
    (\x -> case x ^. _Either of
             Left e -> Right e
             Right a -> Left (_Either # Right a))
{-# INLINE _Failure #-}

-- | This prism generalises 'Control.Lens.Prism._Right'. It targets the success case of either 'Either' or 'Validation'.
_Success ::
  Validate f =>
  Prism (f e a) (f e b) a b
_Success =
  prism
    (\x -> _Either # Right x)
    (\x -> case x ^. _Either of
             Left e -> Left (_Either # Left e)
             Right a -> Right a)
{-# INLINE _Success #-}

-- | 'revalidate' converts between any two instances of 'Validate'.
revalidate :: (Validate f, Validate g) => Iso (f e1 s) (f e2 t) (g e1 s) (g e2 t)
revalidate = _Validation . from _Validation

-}
