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
    GeneralizedNewtypeDeriving,
    TypeFamilies,
    LambdaCase,
    TypeOperators,
    TupleSections,
    NamedFieldPuns,
    RecordWildCards,
    ExplicitForAll,
    AutoDeriveTypeable
 #-}

{-| the 'WarningValidation' @Applicative@:

* accumulate any warnings (non-fatal errors),
* and accumulate /all/ errors (fatal errors, but without aborting before finishing validating everything),
* then returning the validated output /with/ any warnings /unless/ there are one-or-more errors (which are also returned with warnings).

Inspired by the @Chronicle@ monad and the @Validation@ applicative.

Examples:

* "Example.WarningValidation": see the @Example.WarningValidation@ module for a @doctest@ed example. Also see the source of the "Example.WarningValidation.validateNaturalRatio" function for an simple demonstration about how to write a validator that accumulates all errors, and warnings, that both: validates the two inputs individually; and validates a property of the two inputs jointly, a.k.a. output, which is cleaner in a @Monad@, but still possible and useful under an @Applicative@.



-}
module Spiros.WarningValidation where

import Prelude.Spiros.Reexports 
import Prelude.Spiros.Classes 
import Prelude.Spiros.Utilities

----------------------------------------
-- aliases

-- | "V" for "Validation". 
type V w e = WarningValidation w e

-- | the underscore implies that the last type variable is trivial i.e. unit,
-- following the @base@ naming convention. 
type V_ w e = WarningValidation w e ()

-- | the apostrophe implies that two type variables coincide,
-- following the @lens@ naming convention. 
type V' e = WarningValidation e e

-- |
type V'_ e = WarningValidation e e ()

-- | 
type V0 = WarningValidation Warnings Errors

-- | 
type V0_ = WarningValidation Warnings Errors ()

----------------------------------------

{- |

see 'runWarningValidation':

@
WarningValidation w e a
~
(w, Either e a)
@

NOTE, lazy accumulators @w@ and @e@, strict payload @a@:

* The constructors are strict in the warning type @w@ and error type @e@, which improves the performance of their accumulation;
* but 'WarningSuccess' is lazy in the return type @a@, which preserves the expected behavior (as with the majority of haskell datatypes).

For example, we can safely performing an otherwise unsafe\/partial operation in a where clause by checking preconditions and returning it only if they're satisfied:

>>> :set -XOverloadedStrings
>>> import Data.Ratio (Rational, (%))
>>> assertNonZeroDenominator d = hardAssertion "the denominator MUST be non-zero" (not (d == 0))
>>> validRatio = (\n d -> assertNonZeroDenominator d *> success (n % d)) :: Integer -> Integer -> WarningValidation Warnings Errors Rational
>>> validRatio 2 4
WarningSuccess (Warnings []) (1 % 2)
>>> validRatio 1 0
WarningFailure (Warnings []) (Errors ("the denominator MUST be non-zero" :| []))

The order of the effects (i.e. the checks) doesn't matter; i.e. you can check both "before" and "after" (i.e. to the left or to the right) of "succeeding" with the possibly-unsafe value (i.e. calling 'succeed' a.k.a. 'pure'):

>>> :set -XOverloadedStrings
>>> import Data.Ratio (Rational, (%))
>>> assertNonZeroDenominator d = hardAssertion "the denominator MUST be non-zero" (not (d == 0))
>>> validRatio_before = (\n d -> assertNonZeroDenominator d *> success (n % d)) :: Integer -> Integer -> WarningValidation Warnings Errors Rational
>>> validRatio_after  = (\n d -> success (n % d) <* assertNonZeroDenominator d) :: Integer -> Integer -> WarningValidation Warnings Errors Rational
>>> validRatio_before 2 4 == validRatio_after 2 4
True
>>> validRatio_before 1 0 == validRatio_after 1 0
True

Also see "Example.WarningValidation.validateNaturalRatio". 

NOTE, the 'Monoid'(\/ 'Semigroup') instance(s) and the 'Alternative' instance differ:

* The 'Alternative' instance is logical disjunction, i.e. "succeed if /any/ succeed". The operation is "try the next if this one fails", and its identity is failure. 
* The 'Monoid' instance is logical conjunction, i.e. "succeed if /all/ succeed". The operation is "try each to make sure that none fail, then merge all the results", and its identity is success.

This difference exploits the different arities between these two "monoidal typeclasses":

* @Monoid (WarningValidation w e a) can require constraints on @a@, in particular @(Monoid a, ...) => ...@, 
* while @Alternative (WarningValidation w e)@ can't. 

Specializations (a.k.a. "What can @w@ and\/or @e@ be?""):

* Strings: @w ~ 'Warnings'@ and @e ~ 'Errors'@; i.e. a @'NonEmpty' String@ and @[String]@.
* @'Data.Sequence.Seq'@: As @u\/sacundim@ mentions on @r\/haskell@, if you want to efficiently suppport keeping only a prefix of messages (like the first 10 errors), then you can "use Validation with a monoid that both: [1] Supports O(1) right appends; and [2] Supports take n in O(n)."
* @a@ itself: @(w ~ f a)@ and @(e ~ g a)@, for some containers @f@ and @g@; i.e. store the validated type itself, which can be interpreteted as "save all inputs that cause errors and\/or cause warnings during validation"; see 'Validated'. 
* custom types: e.g. @(w ~ URLValidationWarning)@ and @(e ~ URLValidationError)@; with @type URLValidator = Text -> Validation URLValidationWarning URLValidationError URL@; @URLValidationError@ can also instantiate 'Exception' with a 'Show' that pretty-prints it. 

-}
data WarningValidation w e a 
 = WarningFailure !w !e
 | WarningSuccess !w (a)
 deriving 
  ( Eq, Ord, Show, Read
  , Generic, Data
  , Functor, Foldable, Traversable
  )

{-
 = WarningFailure !w !e
 | WarningSuccess !w !a
-}

instance (Eq w, Eq e) => Eq1 (WarningValidation w e) where
  liftEq eq = \x y -> case (x,y) of 
    (WarningFailure w1 e1, WarningFailure w2 e2) ->
      (w1 == w2) && (e1 == e2)
    (WarningSuccess w1 a1, WarningSuccess w2 a2) ->
      (w1 == w2) && (a1 `eq` a2)
    (_,_) -> False
  {-# INLINE liftEq #-}

{-

instance (Eq1 f) => Eq2 (Validated f) where
  liftEq2 eqA eqB = \x y -> case (x,y) of 
    (WarningFailure w1 e1, WarningFailure w2 e2) ->
      (w1 `eqA` w2) && (e1 `eqA` e2)
    (WarningSuccess w1 a1, WarningSuccess w2 a2) ->
      (w1 `eqA` w2) && (a1 `eqB` a2)
    (_,_) ->
      False
  {-# INLINE liftEq2 #-}


instance (Ord1 f, Ord a) => Ord1 (WarningValidation w e) where
  liftCompare = 
-}
    
----------------------------------------

{-| The partition @Validated f a b@ is the result of validating a collection @f@ of the "raw" input type @a@ into the "valid" output typ
e @b@ (or attempting to do so).

This partition:

* Has collected all "valid" inputs as their validated output, into '_successes'.
* Has remembered all warnings; i.e. wherever the @(a -> m b)@ check has succeded with a warning (e.g. 'successBut'), that @a@ "remains" in '_warnings' while the @b@ gets put into '_successes'. 
* Has reported all errors; if some invalid input has both an error and a warning, (e.g. 'failureAnd'), that @a@ was put both in '_warnings' and in '_errors'.

When the '_errors' container is empty, the 'successes_' container should be "full" (as many outputs as inputs). e.g.:

@
-- f ~ []

Validated [] a b
~
([a], [b], [b])

> :set -XNamedFieldPuns
> wasValidatedSuccessfully :: Validated [] a b -> Bool
> wasValidatedSuccessfully Validated{_errors=[]) = True
> wasValidatedSuccessfully Validated{}           = False
@

-}
data Validated f a b = Validated
  { _successes :: !(f b)
  , _warnings  :: !(f a)
  , _errors    :: !(f a)
  }

instance (Eq1 f) => Eq2 (Validated f) where
  liftEq2 eqA eqB = \case
    Validated{_successes=s1, _warnings=w1, _errors=e1} -> \case
      Validated{_successes=s2, _warnings=w2, _errors=e2} ->
        all id
          [ w1 `eqFA` w2
          , e1 `eqFA` e2
          , s1 `eqFB` s2
            -- the _successes field is last for maximal laziness
          ]
    where
    eqFA = liftEq eqA
    eqFB = liftEq eqB
  {-# INLINE liftEq2 #-}

{-

 \x y -> case (x,y) of
    Validated{_successes, _warnings, _errors} = all


instance (Eq1 f,   Eq a,   Eq b)   => Eq   (Validated f a b) where
  (==) = eq1

instance (Ord1 f,  Ord a,  Ord b)  => Ord  (Validated f a b) where
  compare = compare1

instance (Read1 f, Read a, Read b) => Read (Validated f a b) where
  readPrec     = readPrec1
  readListPrec = readListPrecDefault
  
instance (Show1 f, Show a, Show b) => Show (Validated f a b) where
  showsPrec = showsPrec1
-}

-- | @'fromList' becomes '_successes'@
instance
  ( IsList (f b)
  , (b ~ Item (f b))
  , IsList (f a)
  , (a ~ Item (f a))
  )
  => IsList (Validated f a b)
  where

  type Item (Validated f a b) = Item (f b)

  toList Validated{_successes} = toList _successes

  fromList xs = Validated{..}
     where
     _successes = fromList xs
     _warnings  = fromList []
     _errors    = fromList []
  {-# INLINE fromList #-}
  
{-|

@
@

-}
instance ( Semigroup (f a), Semigroup (f b)
         )
      => Semigroup (Validated f a b)
  where
  (<>) = mergeValidated (<>) (<>) 
  {-# INLINE (<>) #-}

{-|

@
@

-}
instance ( Monoid (f a), Monoid (f b)
--         , Semigroup (f a), Semigroup (f b)
         )
      => Monoid (Validated f a b)
  where
  mempty = Validated mempty mempty mempty
  {-# INLINE mempty #-}
  mappend = mergeValidated mappend mappend 
  {-# INLINE mappend #-}

mergeValidated
 :: ()
 => (f ax -> f ay -> f az)
 -> (f bx -> f by -> f bz)
 -> Validated f ax bx
 -> Validated f ay by
 -> Validated f az bz
mergeValidated fa fb = go
  where
  go (Validated xSuccesses xWarnings xErrors)
     (Validated ySuccesses yWarnings yErrors)
    = Validated zSuccesses zWarnings zErrors
          where
          zSuccesses = (xSuccesses `fb` ySuccesses)
          zWarnings  = (xWarnings  `fa` yWarnings)
          zErrors    = (xErrors    `fa` yErrors) 

{-# INLINE mergeValidated #-}

----------------------------------------

{-| A convenient specialization, with a list of strings as warnings, and a (non-empty) list of strings as errors. Strings, being the most open simple datatype, are a common defaultfor messages. 

i.e. @SimpleWarningValidation a@

-}
type SimpleWarningValidation = WarningValidation Warnings Errors

type WarningValidation_ w e = WarningValidation w e () 
  
----------------------------------------
--

{-|

e.g. @'WarningValidation' 'Warnings' e a@

-}
newtype Warnings = Warnings [String]
  deriving 
  ( Eq, Ord, Show, Read
  , Generic, Data
  , NFData, Hashable
  , Semigroup, Monoid
  )

instance IsString Warnings where
  fromString = (:[]) > Warnings

instance IsList Warnings where
  type Item Warnings = String
  fromList = Warnings
  toList = getWarnings

getWarnings :: Warnings -> [String]
getWarnings (Warnings ws) = ws

----------------------------------------
--

{-|

e.g. @'WarningValidation' w 'Errors' a@

-}
newtype Errors = Errors (NonEmpty String)
  deriving 
  ( Eq, Ord, Show, Read
  , Generic, Data
  , NFData, Hashable
  , Semigroup
  )

instance IsString Errors where
  fromString = (:|[]) > Errors

-- | WARNING: @fromList@ is partial, on @[]@. 
instance IsList Errors where
  type Item Errors = String
  fromList = fromList > Errors
  toList = getErrors > toList

getErrors :: Errors -> NonEmpty String
getErrors (Errors es) = es

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

@  
  (<|>) = 'validateAny'
@

-}
instance (Monoid w, Monoid e, Semigroup e)
      => Alternative (WarningValidation w e) where
  
  empty = WarningFailure mempty mempty
  {-# INLINE empty #-}
  
  (<|>) = validateAny
  {-# INLINE (<|>) #-}

{-|

@
   (<>) = 'validateBoth'
@

-}
instance (Semigroup w, Semigroup e, Semigroup a)
      => Semigroup (WarningValidation w e a)
  where
  (<>) = mergeWarningValidation (<>) (<>) (<>)
  {-# INLINE (<>) #-}


{-|

@mempty = 'WarningSuccess' 'mempty' 'mempty'@

@
   mappend = 'validateBoth'
@


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
instance ( Monoid w, Monoid a
         , Semigroup w, Semigroup e, Semigroup a
         )
      => Monoid (WarningValidation w e a)
  where
  mempty = WarningSuccess mempty mempty
  {-# INLINE mempty #-}
  mappend = validateBoth
  {-# INLINE mappend #-}

----------------------------------------
-- instance helpers

validateAny
  :: (Monoid w, Semigroup e)
  => WarningValidation w e a
  -> WarningValidation w e a
  -> WarningValidation w e a
validateAny v1@WarningSuccess{} _  = v1
validateAny    WarningFailure{} v2 = v2

{-# INLINE validateAny #-}

----------------------------------------
-- instance helpers

validateBoth
  :: (Semigroup w, Semigroup e, Semigroup a)
  => WarningValidation w e a
  -> WarningValidation w e a
  -> WarningValidation w e a
validateBoth = mergeWarningValidation (<>) (<>) (<>)

{-# INLINE validateBoth #-}

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
  => w -> WarningValidation_ w e 
warning w = WarningSuccess w ()

{-# INLINE warning #-}
  
----------------------------------------
-- 

{-| Succeed trivially, without any warnings. 

@
= 'success' ()
@

-}
success_
  :: ( Monoid w
     )
  => WarningValidation w e ()
success_ = success ()

{-# INLINE success_ #-}

{-| Fail trivially, without any warnings. 

@
= 'failure' ()
@

-}
failure_
  :: ( Monoid w
     )
  => WarningValidation w () a
failure_ = failure ()

{-# INLINE failure_ #-}

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
  :: ( Applicative f
     )
  => w -> a -> WarningValidation (f w) e a
successBut0 w = successBut (pure w)

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

{-| Fail with an error and with a warning, via @pure@ to injection a singleton message into a container. 

@
failureAnd1 @[] @[] w e
=
'WarningFailure' [w] [e]

failureAnd1 @[] @NonEmpty w e
=
'WarningFailure' [w] (e:|[])
@

-}
failureAnd1
  :: forall f g. forall w e a.
     ( Applicative f
     , Applicative g
     )
  => w
  -> e
  -> WarningValidation (f w) (g e) a
failureAnd1 w e = WarningFailure (pure w) (pure e)
{-# INLINE failureAnd1 #-}

{-

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
-}

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

failureIf_
  :: ( Monoid w
     )
  => (x -> Bool)
  -> (x -> e)
  -> x
  -> WarningValidation_ w e
failureIf_ predicate render
  = failureIf predicate render
  > void 

-- void :: Functor f => f a -> f ()
  
failureUnless_
  :: ( Monoid w
     )
  => (x -> Bool)
  -> (x -> e)
  -> x
  -> WarningValidation_ w e
failureUnless_ predicate render
  = failureUnless predicate render
  > void 

----------------------------------------

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

----------------------------------------

{- | Convert a simple @Bool@-based validator into the more informative and flexible @WarningValidation@-based one.

Parameters (@certifyBool render check cast@):

* @render@: render the input as an error message, often a container of text, like @[String]@ or 'Errors'. 
* @check@: check whether the input is valid; obviously, @True@ means valid, @False@ means invalid. 
* @cast@: convert the input to output; can safely be a partial function, if it's total whenever the @check@ passes, since the validation type is lazy in the return type.

Naming:

* @"certify"@ is a synonym with @"validate"@.
* the @certify<...>@ functions only raise errors, while the @verify<...>@ functions can both raise errors and emit warnings.

Generalizations:

* See 'verifyBool'. 

Specializations:

@
certifyBool _ _ 'id'
  :: ( ...
     )
  => (a -> e)
  -> (a -> Bool)
  -> (a -> 'WarningValidation' w e a)

certifyBool 'show' _ _
  :: ( ...
     , Show a
     )
  => (a -> Bool)
  -> (a -> b)
  -> (a -> 'WarningValidation' w e b)
@

Examples:

@
validateNatural = certifyBool
  (\a -> 'fromString' (show a) <> fromString " must be non-negative")
  (>=0)
  'fromIntegral'

-- (inferred)
validateNatural
  :: ( ...
     , 'IsString' e, 'Semigroup' e
     , 'Show' a, 'Ord' a , 'Integral' a, 
     , 'Num' b
     )
  => (a -> 'WarningValidation' w e b)

-- (monomorphic)
validateNatural
  :: ()
  => (Integer -> 'WarningValidation' () 'Errors' Natural)
@

-}
certifyBool
  :: ( Monoid w
     )
  => (a -> e)
  -> (a -> Bool)
  -> (a -> b)
  -> (a -> WarningValidation w e b)
certifyBool render check cast = go
  where
  go a =
    if check a 
    then let b = cast   a in success b
    else let e = render a in failure e
{-# INLINE certifyBool #-}

{-| Like 'certifyBool', but the message doesn't render the input. 

More convenient, but less informative. Practically, it means you can say stuff like @"the number must be positive"@ not @("expected a positive number, but received: " <> ...)@. 

@
= 'certifyBool' ('const' e) check cast
@

-}
certifyBool_
  :: ( Monoid w
     )
  => e
  -> (a -> Bool)
  -> (a -> b)
  -> (a -> WarningValidation w e b)
certifyBool_ e check cast =
  certifyBool (const e) check cast
  
{-# INLINE certifyBool_ #-}

{- | Convert a simple @Bool@-based validator into the more informative and flexible @WarningValidation@-based one.

Parameters (@verifyBool render check cast@):

* @render@: render the input as an error message, often a container of text, like @[String]@ or 'Errors'. 
* @check@: check whether the input is valid; obviously, @True@ means valid, @False@ means invalid. 
* @cast@: convert the input to output; can safely be a partial function, if it's total whenever the @check@ passes, since the validation type is lazy in the return type. 

Naming:

* @"verify"@ is a synonym with @"validate"@.
* the @verify<...>@ functions can both raise errors and emit warnings, while the @certify<...>@ functions only raise errors. 

Specializations:

* 'certifyBool'

@
'certifyBool' = 'verifyBool' ('const' 'mempty') _
'certifyBool' = 'verifyBool' _ ('const' 'False')
@

* identity

@
verifyBool _ _ 'id'
  :: ( ...
     )
  => (a -> e)
  -> (a -> Bool)
  -> (a -> 'WarningValidation' w e a)
@

* showing

@
verifyBool 'show' _ _
  :: ( ...
     , Show a
     )
  => (a -> Bool)
  -> (a -> b)
  -> (a -> 'WarningValidation' w e b)
@

Examples:

@
validateNatural = verifyBool
  (\a -> 'fromString' (show a) <> fromString " must be non-negative")
  (>=0)
  'fromIntegral'

-- (inferred)
validateNatural
  :: ( ...
     , 'IsString' e, 'Semigroup' e
     , 'Show' a, 'Ord' a , 'Integral' a, 
     , 'Num' b
     )
  => (a -> 'WarningValidation' w e b)

-- (monomorphic)
validateNatural
  :: ()
  => (Integer -> 'WarningValidation' () 'Errors' Natural)
@

-}
verifyBool
  :: ( Monoid w
     )
  => (a -> w)
  -> (a -> Bool)
  -> (a -> e)
  -> (a -> Bool)
  -> (a -> b)
  -> (a -> WarningValidation w e b)
verifyBool renderWarning softCheck renderError hardCheck cast = go
  where
  go a =
    let
      w = if softCheck a then renderWarning a else mempty
    in
      if hardCheck a 
      then let b =        cast a in successBut w b
      else let e = renderError a in failureAnd w e

{-| Like 'verifyBool', but the messages (i.e. both the error message and the warning message), don't render the input (i.e. each is the same across all input).

More convenient, but less informative. Practically, it means you can say stuff like @"the number must be positive"@ not @("expected a positive number, but received: " <> ...)@. 

@
= 'verifyBool' ('const' w) softCheck ('const' e) hardCheck cast
@

-}
verifyBool_
  :: ( Monoid w
     )
  => w
  -> (a -> Bool)
  -> e
  -> (a -> Bool)
  -> (a -> b)
  -> (a -> WarningValidation w e b)
verifyBool_ w softCheck e hardCheck cast =
  verifyBool (const w) softCheck (const e) hardCheck cast
{-# INLINE verifyBool_ #-}

-- {- | See 'certifyMaybe'. 

-- -}
-- verifyMaybe
--   :: ( Monoid w
--      )
--   => (e)
--   -> (a -> Maybe b)
--   -> (a -> WarningValidation w e b)
-- verifyMaybe render check = go
--   where
--   go a = case check a of
--     Nothing -> let e = render a in failure e
--     Just b  -> success b
-- {-# INLINE verifyMaybe #-}

{- | Convert a simple @Maybe@-based validator into the more informative and flexible @WarningValidation@-based one.

= Notes on transformations. 

Loosely, @(certifyMaybe _)@ transforms a @Kleisli Maybe@ to a @Kleisli (WarningValidation w e)@:

@
(a -> Maybe b) -> (a -> WarningValidation w e b)
~
Kleisli Maybe a b -> Kleisli (WarningValidation w e) a b
@

and given these definition \/ this notation:

@
-- the kleisli arrow
newtype Kleisli m a b = Kleisli (a -> m b)

-- natural transformation between functors
type (~>) f g = (forall x. f x -> g x)

-- natural transformation between prounctors(?)
type (~~>>) p q = (forall x y. p x y -> q x y)
@

it looks like:

@
(forall x y. Kleisli Maybe x y -> Kleisli (WarningValidation w e) x y)
~
Kleisli Maybe ~~>> Kleisli (WarningValidation w e)
@

-}
certifyMaybe
  :: ( Monoid w
     )
  => (a -> e)
  -> (a -> Maybe b)
  -> (a -> WarningValidation w e b)
certifyMaybe render check = go
  where
  go a = case check a of
    Nothing -> let e = render a in failure e
    Just b  -> success b
{-# INLINE certifyMaybe #-}

{- | See 'certifyEither'. 

-}
verifyEither
  :: ( Monoid w
     )
  => (a -> Either w ())
  -> (a -> Either e b)
  -> (a -> WarningValidation w e b)
verifyEither softCheck hardCheck = go
  where
  go a =
    let
      w = softCheck a & either id (const mempty) 
    in
      case hardCheck a of
        Right b -> successBut w b
        Left  e -> failureAnd w e

{-# INLINE verifyEither #-}

{- | Convert a simple @Either@-based validator into the more informative and flexible @WarningValidation@-based one.

= Notes on transformations. 

Loosely, @certifyEither@ transforms a @Kleisli (Either e)@ to a @Kleisli (WarningValidation w e)@:

@
(a -> (Either e b) -> (a -> WarningValidation w e b)
~
Kleisli (Either e) a b -> Kleisli (WarningValidation w e) a b
@

and given these definition \/ this notation:

@
-- the kleisli arrow
newtype Kleisli m a b = Kleisli (a -> m b)

-- natural transformation between functors
type (~>) f g = (forall x. f x -> g x)

-- natural transformation between prounctors(?)
type (~~>>) p q = (forall x y. p x y -> q x y)
@

it looks like:

@
(forall x y. Kleisli (Either e) x y -> Kleisli (WarningValidation w e) x y)
~
Kleisli (Either e) ~~>> Kleisli (WarningValidation w e)
@

-}
certifyEither
  :: ( Monoid w
     )
  => (a -> Either e b)
  -> (a -> WarningValidation w e b)
certifyEither check = go
  where
  go a = case check a of
    Left  e -> failure e
    Right b -> success b
{-# INLINE certifyEither #-}

----------------------------------------

{-| The generic representation of an algebraic datatype is some sum-of-products (i.e. "cases with fields", i.e. @Either@'s of @(,)@'s). 

-}
verifyGeneric
  :: ( 
     )
  => (Either (w,e) (w,a))
  -> (WarningValidation w e a)
verifyGeneric = \case
  Left  (w,e) -> WarningFailure w e
  Right (w,x) -> WarningSuccess w x

{-| The generic representation of 'WarningValidation' with warnings ignored (e.g. @(w ~ ())@).

-}
certifyGeneric
  :: ( Monoid w
     )
  => (Either              e a)
  -> (WarningValidation w e a)
certifyGeneric
  = bimap (mempty,) (mempty,)
  > verifyGeneric

{-| A single "partition" is when the type getting validated, or a collection thereof, is itself is returned as a error or a warning. 

@
verifyPartition = 'verifyGeneric'
@

-}
verifyPartition
  :: ( Monoid a
     )
  => (Either (a,a) (a,a))
  -> (WarningValidation a a a)
verifyPartition = verifyGeneric

{-| A single "partition" is when the type getting validated, or a collection thereof, is itself is returned as a error. 

@
certifyPartition = 'certifyGeneric'
@

-}
certifyPartition
  :: ( Monoid a
     )
  => (Either a a)
  -> (WarningValidation a a a)
certifyPartition = certifyGeneric

----------------------------------------

{- | Validates an @a@ from the given @Either@, failing with @e@ on @Left e@, and succeeding with @a@ on @Right a@. 

@
= 'either' 'failure' 'success'
@

-}
either2validation
  :: ( Monoid w
     )
  => (Either e a)
  -> WarningValidation w e a
either2validation = either failure success
{-# INLINE either2validation #-}

{- | Validates an @a@ from the given @Maybe@, failing with @e@ on @Nothing@, and succeeding with @a@ on @Just a@. 

@
= \e -> 'maybe' ('failure' e) 'success'
@

-}
maybe2validation
  :: ( Monoid w
     )
  => e
  -> Maybe a
  -> WarningValidation w e a
maybe2validation e = maybe (failure e) success
{-# INLINE maybe2validation #-}

{- | Validates an @a@ from the given @Maybe@, failing on @Nothing@, and succeeding with @a@ on @Just a@. 

@
= 'maybe2validation' 'mempty'
@

-}
maybe2validation_
  :: ( Monoid w
     , Monoid e
     )
  => Maybe a
  -> WarningValidation w e a
maybe2validation_ = maybe2validation mempty 
{-# INLINE maybe2validation_ #-}

{- | Validates an @a@ with the given predicate @p@, returning @e@ if the predicate does not hold.

@
'predicate2validation' e p a
@

-}
predicate2validation
  :: ( Monoid w
     )
  => (a -> e)
  -> (a -> Bool)
  -> a
  -> WarningValidation w e a
predicate2validation render predicate = go
  where
  go a
    = if   predicate a
      then success a
      else failure e
      where
      e = render a
{-# INLINE predicate2validation #-}

{- | Validates that given @condition@ holds @True@,
failing with the @message@ otherwise.

@
boolean2validation message condition
=
'predicate2validation' ('const' message) ('const' condition) ()
@

-}
boolean2validation
  :: ( Monoid w
     )
  => e
  -> Bool
  -> WarningValidation_ w e 
boolean2validation message condition
  = predicate2validation (const message) (const condition) ()

{-# INLINE boolean2validation #-}

----------------------------------------

{- | Asserts that given @condition@ holds @True@,
failing with the @message@ otherwise.

@
'hardAssertion' message condition
@

-}
hardAssertion
  :: ( Monoid w
     )
  => e
  -> Bool
  -> WarningValidation_ w e 
hardAssertion message condition = go
  where
  go =
    if   condition
    then success_
    else failure message

{-# INLINE hardAssertion #-}

{- | Informs whether given @condition@ holds @True@,
warning with the @message@ otherwise
(but still succeeding anyways).

@
'softAssertion' message condition
@

-}
softAssertion
  :: ( Monoid w
     )
  => w
  -> Bool
  -> WarningValidation_ w e 
softAssertion message condition = WarningSuccess w ()
  where
  w =
    if   condition
    then mempty
    else message

{-# INLINE softAssertion #-}  

----------------------------------------

{-| A single "partition" is when the type getting validated is returned, and collected into: [1] disjoint successes\/failures; and [2] possibly overlapping warnings.

e.g.:

@
partitionValidation           pure :: f a -> Validated f  a a
partitionValidation @a @a @[] pure :: [a] -> Validated [] a a

pure @(Validation w _ a) ≡ WarningSuccess (mempty @w)

partitionValidation pure (                        xs)
≡
partitionValidation id   ('WarningSuccess' `fmap` xs)

> partitionValidation @a @a @[] pure xs
Validated
 { '_successes' = 'WarningSuccess' `fmap` xs
 , '_warnings'  = []
 , '_errors'    = []
 }
@

e.g.:

@
validateNaturalRational :: Natural -> Natural -> Ratioinal
exampleNumerators       :: [Natural]
exampleDenominators     :: [Natural]

> partitionViaValidator @(Natural,Natural) @Rational @[] (uncurry validateNaturalRational) (zip exampleNumerators exampleDenominators)

@

wraps 'partitionValidation'.

-}
partitionViaValidator 
  :: forall a b f.
     ( Monoid (f a) -- TODO Monoid1 f ??
     , Monoid (f b) -- TODO Monoid1 f ??
     , Applicative f
     , Foldable    f
     )
  => (a -> WarningValidation a a b)
  -> (f a -> Validated f a b)
partitionViaValidator check
  = fmap check
  > partitionValidation

{-|

@
= 'foldMap' 'validation2validated'
@

-}
partitionValidation
  :: forall a b f.
     ( Monoid (f a) -- TODO Monoid1 f ??
     , Monoid (f b) -- TODO Monoid1 f ??
     , Foldable f
     , Applicative f
     )
  => f (WarningValidation a a b)
  -> Validated f a b
partitionValidation
  = foldMap validation2validated -- foldl' go mempty
  --NOTES foldMap :: Monoid m => (a -> m) -> t a -> m

{-|

Either:

@
          '_successes' = 'pure' b 
          '_warnings'  = 'pure' w
          '_errors'    = 'mempty'

@

Or:

@
          '_successes' = 'mempty'
          '_warnings'  = 'pure' w
          '_errors'    = 'pure' a
@

-}
validation2validated
  :: forall a b f.
     ( Monoid (f a) -- TODO Monoid1 f ??
     , Monoid (f b) -- TODO Monoid1 f ??
     , Applicative f
     )
  => WarningValidation a a b
  -> Validated f a b
validation2validated = \case
  
  WarningSuccess w b -> Validated{..}
          where
          _successes = pure b 
          _warnings  = pure w
          _errors    = mempty
         
  WarningFailure w a -> Validated{..}
          where
          _successes = mempty
          _warnings  = pure w
          _errors    = pure a

----------------------------------------

{-| Like 'partitionViaValidator', but on a binary (not unary) function, and specialized to lists. 
  
e.g.:

@
validateNaturalRational :: Natural -> Natural -> Ratioinal
exampleNumerators       :: [Natural]
exampleDenominators     :: [Natural]

> partitionViaValidator2 @Natural @Natural @Rational validateNaturalRational exampleNumerators exampleDenominators
@

wraps 'partitionValidation'.

-}
partitionViaValidator2 
  :: forall a b c.
     (
     )
  => ( a  ->  b  -> WarningValidation (a,b) (a,b) c)
  -> ([a] -> [b] -> Validated [] (a,b) c)
partitionViaValidator2 check2 = go
  where
  go as bs
    = zipWith check2 as bs
    & partitionValidation


{-| Like 'partitionViaValidator', but on a ternary (not unary) function, and specialized to lists. 

wraps 'partitionValidation'.

-}
partitionViaValidator3
  :: forall a b c d.
     (
     )
  => ( a  ->  b  ->  c  -> WarningValidation (a,b,c) (a,b,c) d)
  -> ([a] -> [b] -> [c] -> Validated []              (a,b,c) d)
partitionViaValidator3 check3 = go
  where
  go as bs cs
    = zipWith3 check3 as bs cs
    & partitionValidation

{-| Like 'partitionViaValidator', but on a quaternary (not unary) function, and specialized to lists. 

wraps 'partitionValidation'.

-}
partitionViaValidator4
  :: forall r a b c d.
     (
     )
  => ( a  ->  b  ->  c  ->  d  ->
      WarningValidation (a,b,c,d) (a,b,c,d) r
     )
  -> ([a] -> [b] -> [c] -> [d] ->
      Validated [] (a,b,c,d) r
     )
partitionViaValidator4 check4 = go
  where
  go as bs cs ds
    = zipWith4 check4 as bs cs ds
    & partitionValidation

{-| Like 'partitionViaValidator', but on a 5-argument (not unary) function, and specialized to lists. 

wraps 'partitionValidation'.

-}
partitionViaValidator5
  :: forall r a b c d e.
     (
     )
  => ( a  ->  b  ->  c  ->  d  ->  e  ->
      WarningValidation (a,b,c,d,e) (a,b,c,d,e) r
     )
  -> ([a] -> [b] -> [c] -> [d] -> [e] ->
      Validated [] (a,b,c,d,e) r
     )
partitionViaValidator5 check5 = go
  where
  go as bs cs ds es
    = zipWith5 check5 as bs cs ds es
    & partitionValidation

{-

  :: forall a b c f.
     ( Monoid (f a) -- TODO Monoid1 f ??
     , Monoid (f b) -- TODO Monoid1 f ??
     , Monoid (f c) -- TODO Monoid1 f ??
     , Applicative f
     , Foldable    f
     )
  => (  a ->   b -> WarningValidation (a,b) (a,b) c)
  -> (f a -> f b -> Validated f (a,b) c)
-}

----------------------------------------

{-


softAssertion
  :: ( Semigroup w, Monoid w
     )
  => w
  -> Bool
  -> WarningValidation_ w e 
softAssertion message condition
  = warning w *> success_
  where
  w =
    if   condition
    then message
    else mempty



  
  where
  softCheck
    = if   condition
      then success a
      else failure e
      where
      e = render a



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



{- | The type of strict pairs.

@(,)@ is lazy, @(:!:)@ is strict. 

-}
data Pair a b
 = !a :!: !b
 deriving
  ( Eq, Ord, Show, Read, Bounded, Ix

  , Generic, Data
  , Functor, Foldable, Traversable

  )

infixl 2 :!:

type (:!:) = Pair


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
