{-# LANGUAGE CPP #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE
    PackageImports,
    DeriveDataTypeable,
    DeriveGeneric,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveLift,
    DeriveAnyClass,
    LambdaCase,
    TypeOperators,
    AutoDeriveTypeable
 #-}

{-| the 'WarningValidation' @Applicative@:

* accumulate any warnings (non-fatal errors),
* and accumulate /all/ errors (fatal errors, but without aborting before finishing validating everything),
* then returning the validated output /with/ any warnings /unless/ there are one-or-more errors (which are also returned with warnings).

Inspired by the @Chronicle@ monad and the @Validation@ applicative.

Examples:

>>> import Data.Ratio (Ratio,(%))
>>> :{
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
     ( if   ((n >= 0) && (d >= 0))
       then []
       else ["the numerator and denominator were both negative"]
     )
  where
  r  = n' % d' :: Ratio Natural
  n' = fromIntegral (abs n)
  d' = fromIntegral (abs d)
:}
>>> -- errors
>>> validateNaturalRatio 1 0
WarningFailure [] ["the denominator must be non-zero"]
>>> validateNaturalRatio 1 (-2)
WarningFailure [] ["the ratio must be non-negative"]
>>> -- warnings
>>> validateNaturalRatio (-1) (-2)
WarningSuccess ["the numerator and denominator were both negative"] (1 % 2)
>>> -- successes
>>> validateNaturalRatio 1 2
WarningSuccess [] (1 % 2)
>>> validateNaturalRatio 0 2
WarningFailure [] ["the ratio must be non-negative"]

-}
module Spiros.WarningValidation where

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities

--import "base" Data.Ratio

----------------------------------------

{- | The type of strict pairs.

@(,)@ is lazy, @(:!:)@ is strict. 

-}
data Pair a b
 = !a :!: !b
 deriving
  ( Eq, Ord, Show, Read, Bounded, Ix
#if __GLASGOW_HASKELL__ >= 702
  , Generic, Data
  , Functor, Foldable, Traversable
#endif
  )

infixl 2 :!:

type (:!:) = Pair

----------------------------------------

{- |

see 'runWarningValidation':

@
WarningValidation w e a
~
(w, Either e a)
@

NOTE, the 'Monoid'(\/ 'Semigroup') instance(s) and the 'Alternative' instance differ:

* The 'Alternative' instance is logical disjunction, i.e. "succeed if /any/ succeed". The operation is "", and its identity is failure. 
* The 'Monoid' instance is logical conjunction, i.e. "succeed if /all/ succeed". The operation is "", and its identity is success.

This difference exploits the different arities between these two "monoidal typeclasses":

* @Monoid (WarningValidation w e a) can require constraints on @a@, in particular @(Monoid a, ...) => ...@, 
* while @Alternative (WarningValidation w e)@ can't. 

-}
data WarningValidation w e a 
 = WarningFailure !w !e
 | WarningSuccess !w !a
 deriving 
  ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
  , Generic, Data
  , Functor, Foldable, Traversable
#endif
  )

----------------------------------------
-- instances

{-|

@pure = 'WarningSuccess' 'mempty'@

-}
instance (Monoid w, Semigroup e)
      => Applicative (WarningValidation w e)
  where

  pure = WarningSuccess mempty 
  {-# INLINE pure #-}
  
  WarningFailure w1 e1 <*> WarningFailure w2 e2 = WarningFailure (w1 `mappend` w2) (e1 <> e2)

  WarningFailure w1 e1 <*> WarningSuccess w2 _  = WarningFailure (w1 `mappend` w2) e1
  WarningSuccess w1 _  <*> WarningFailure w2 e2 = WarningFailure (w1 `mappend` w2) e2

  WarningSuccess w1 f  <*> WarningSuccess w2 a  = WarningSuccess (w1 `mappend` w2) (f a)
  {-# INLINE (<*>) #-}
  
{-|

@empty = 'WarningFailure' 'mempty' 'mempty'@

-}
instance (Monoid w, Monoid e, Semigroup e)
      => Alternative (WarningValidation w e) where
  
  empty = WarningFailure mempty mempty
  {-# INLINE empty #-}
  
  v1@WarningSuccess{} <|> _  = v1
  WarningFailure{}    <|> v2 = v2
  {-# INLINE (<|>) #-}

{-|

-}
instance (Semigroup w, Semigroup e, Semigroup a)
      => Semigroup (WarningValidation w e a)
  where
  (<>) = mergeWarningValidation (<>) (<>) (<>)
  {-# INLINE (<>) #-}


{-|

@mempty = 'WarningSuccess' 'mempty' 'mempty'@

@
mempty ≠ 'empty' 
@

a.k.a.:

@
mempty @(WarningValidation _ _ _)
≠
'empty' @(WarningValidation _ _ _)
@

-}
instance (Monoid w, Monoid e, Monoid a)
      => Monoid (WarningValidation w e a)
  where
  mempty = WarningSuccess mempty mempty
  {-# INLINE mempty #-}
  mappend = mergeWarningValidation mappend mappend mappend
  {-# INLINE mappend #-}

----------------------------------------
-- instance helpers

-- | Fail if any fail, succeed only if all succeed,
-- while collecting all warnings either way. 
-- For the 'Monoid'\/'Semigroup instances. 
mergeWarningValidation
  :: (w -> w -> w)
  -> (e -> e -> e)
  -> (a -> a -> a)
  -> WarningValidation w e a
  -> WarningValidation w e a
  -> WarningValidation w e a
mergeWarningValidation mW mE mA = go

  where
  go (WarningFailure w1 e1) (WarningFailure w2 e2) = WarningFailure (w1 `mW` w2) (e1 `mE` e2)

  go (WarningFailure w1 e1) (WarningSuccess w2 _ ) = WarningFailure (w1 `mW` w2) e1
  go (WarningSuccess w1 _)  (WarningFailure w2 e2) = WarningFailure (w1 `mW` w2) e2

  go (WarningSuccess w1 a1) (WarningSuccess w2 a2) = WarningSuccess (w1 `mW` w2) (a1 `mA` a2)

{-# INLINE mergeWarningValidation #-}

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

{-
runErrorValidation
  :: (
     )
  => WarningValidation w e a
  -> Either (w,e) a
runErrorValidation = runWarningValidation > \case
  ([], Right a) -> Right a
  (w,  Left  e) -> Left (w,e)
  (w,  Left  e) -> Left (w,e)
-}

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

{-


{- |

warnings and errors share the same type (thus the error @e@ must be @Monoid@ too).

Bifunctor

-}
newtype WarningValidation' e a = WarningValidation'
 { getWarningValidation' :: WarningValidation e e a
 }

{- |

--TODO warnings and errors with same type?

Bifunctor

-}
newtype SimpleWarningValidation e a = SimpleWarningValidation
 { getSimpleWarningValidation :: WarningValidation
   (Seq e)       -- strict list
   (e :!: Seq e) -- strict non-empty list
   a
 }
 
{-

{ getSimpleWarningValidation :: WarningValidation [e] (NonEmpty e) a }

= Failure !(w :!: e)
 | Success !(w :!: a)

-}


-}

----------------------------------------
  
{-


{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

-- | A data type similar to @Data.Either@ that accumulates failures.
module Data.Validation
(
  -- * Data type
  Validation(..)
  -- * Constructing validations
, validate
, validationNel
, fromEither
, liftError
  -- * Functions on validations
, validation
, toEither
, orElse
, valueOr
, ensure
, codiagonal
, validationed
, bindValidation
  -- * Prisms
  -- | These prisms are useful for writing code which is polymorphic in its
  -- choice of Either or Validation. This choice can then be made later by a
  -- user, depending on their needs.
  --
  -- An example of this style of usage can be found
  -- <https://github.com/qfpl/validation/blob/master/examples/src/PolymorphicEmail.hs here>
, _Failure
, _Success
  -- * Isomorphisms
, Validate(..)
, revalidate
) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import Control.DeepSeq (NFData (rnf))
import Control.Lens (over, under)
import Control.Lens.Getter((^.))
import Control.Lens.Iso(Swapped(..), Iso, iso, from)
import Control.Lens.Prism(Prism, prism)
import Control.Lens.Review(( # ))
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Bool (Bool)
import Data.Data(Data)
import Data.Either(Either(Left, Right), either)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldr))
import Data.Function((.), ($), id)
import Data.Functor(Functor(fmap))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Data.Typeable(Typeable)
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
import Prelude(Show)


-- | An @Validation@ is either a value of the type @err@ or @a@, similar to 'Either'. However,
-- the 'Applicative' instance for @Validation@ /accumulates/ errors using a 'Semigroup' on @err@.
-- In contrast, the @Applicative@ for @Either@ returns only the first error.
--
-- A consequence of this is that @Validation@ has no 'Data.Functor.Bind.Bind' or 'Control.Monad.Monad' instance. This is because
-- such an instance would violate the law that a Monad's 'Control.Monad.ap' must equal the
-- @Applicative@'s 'Control.Applicative.<*>'
--
-- An example of typical usage can be found <https://github.com/qfpl/validation/blob/master/examples/src/Email.hs here>.
--
data Validation err a =
  Failure err
  | Success a
  deriving (
    Eq, Ord, Show, Data, Typeable
#if __GLASGOW_HASKELL__ >= 702
    , Generic
#endif
  )

instance Functor (Validation err) where
  fmap _ (Failure e) =
    Failure e
  fmap f (Success a) =
    Success (f a)
  {-# INLINE fmap #-}

instance Semigroup err => Apply (Validation err) where
  Failure e1 <.> b = Failure $ case b of
    Failure e2 -> e1 <> e2
    Success _ -> e1
  Success _  <.> Failure e2 =
    Failure e2
  Success f  <.> Success a  =
    Success (f a)
  {-# INLINE (<.>) #-}

instance Semigroup err => Applicative (Validation err) where
  pure =
    Success
  (<*>) =
    (<.>)

instance Alt (Validation err) where
  Failure _ <!> x =
    x
  Success a <!> _ =
    Success a
  {-# INLINE (<!>) #-}

instance Foldable (Validation err) where
  foldr f x (Success a) =
    f a x
  foldr _ x (Failure _) =
    x
  {-# INLINE foldr #-}

instance Traversable (Validation err) where
  traverse f (Success a) =
    Success <$> f a
  traverse _ (Failure e) =
    pure (Failure e)
  {-# INLINE traverse #-}

instance Bifunctor Validation where
  bimap f _ (Failure e) =
    Failure (f e)
  bimap _ g (Success a) =
    Success (g a)
  {-# INLINE bimap #-}


instance Bifoldable Validation where
  bifoldr _ g x (Success a) =
    g a x
  bifoldr f _ x (Failure e) =
    f e x
  {-# INLINE bifoldr #-}

instance Bitraversable Validation where
  bitraverse _ g (Success a) =
    Success <$> g a
  bitraverse f _ (Failure e) =
    Failure <$> f e
  {-# INLINE bitraverse #-}

appValidation ::
  (err -> err -> err)
  -> Validation err a
  -> Validation err a
  -> Validation err a
appValidation m (Failure e1) (Failure e2) =
  Failure (e1 `m` e2)
appValidation _ (Failure _) (Success a2) =
  Success a2
appValidation _ (Success a1) (Failure _) =
  Success a1
appValidation _ (Success a1) (Success _) =
  Success a1
{-# INLINE appValidation #-}

instance Semigroup e => Semigroup (Validation e a) where
  (<>) =
    appValidation (<>)
  {-# INLINE (<>) #-}

instance Monoid e => Monoid (Validation e a) where
  mappend =
    appValidation mappend
  {-# INLINE mappend #-}
  mempty =
    Failure mempty
  {-# INLINE mempty #-}

instance Swapped Validation where
  swapped =
    iso
      (\v -> case v of
        Failure e -> Success e
        Success a -> Failure a)
      (\v -> case v of
        Failure a -> Success a
        Success e -> Failure e)
  {-# INLINE swapped #-}

instance (NFData e, NFData a) => NFData (Validation e a) where
  rnf v =
    case v of
      Failure e -> rnf e
      Success a -> rnf a

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
