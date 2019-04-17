--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------

{-# LANGUAGE PackageImports, TypeOperators, LambdaCase, PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE RankNTypes, PolyKinds, KindSignatures, ConstraintKinds, ScopedTypeVariables #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------
-- Options ---------------------------------------
--------------------------------------------------

-- {-# OPTIONS_HADDOCK not-home #-}

--------------------------------------------------
--------------------------------------------------

{-| Utilities.

These identifiers are "soft" overrides, they generalize the signatures of their @Prelude@ namesakes:

* 'map': @() => []@ to @(Functor f) => f@
* 'sequence': @Monad@ to @Applicative@
* 'sequence_': @Monad@ to @Applicative@

These symbols are "hard" overrides, they are completely different from @Prelude@:

* '>': forward-composition
* '<': backward-composition (i.e. '(.)')

-}

module Prelude.Spiros.Utilities where

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude.Spiros.Types
import Prelude.Spiros.Compatibility

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..))

--------------------------------------------------

import qualified "containers" Data.Set as Set

--------------------------------------------------

import "mtl" Control.Monad.Reader
  (ReaderT,Reader,runReaderT,runReader)
 
import "mtl" Control.Monad.State
  (StateT,State,runStateT,evalStateT,execStateT,runState,evalState,execState)

--------------------------------------------------

import "deepseq" Control.DeepSeq (NFData,force)

--------------------------------------------------

-- import qualified "text" Data.Text      as TS
-- import qualified "text" Data.Text.Lazy as TL

--------------------------------------------------

--import qualified "bytestring" Data.ByteString      as StrictBytes 
import qualified "bytestring" Data.ByteString.Lazy as LazyBytes

--------------------------------------------------

-- import qualified "template-haskell" Language.Haskell.TH.Syntax as TemplateHaskell

--------------------------------------------------

import qualified "base" Data.Char as Char

import "base" Control.Arrow              ((>>>),(<<<))
import "base" Control.Concurrent         (threadDelay,forkIO,ThreadId)
import "base" Control.Exception          (SomeException,IOException,evaluate)
import "base" Control.Monad              (forever, void, when)
import "base" Data.Char                  (isAlpha)
import "base" Data.Foldable              (Foldable(..))
import "base" Data.Function              ((&))
import "base" Data.Proxy                 (Proxy(..))
import "base" Data.Ratio                 (Ratio,(%))
import "base" Data.String                (IsString)
import "base" Numeric.Natural            (Natural)

--------------------------------------------------

import           "base" Control.Category (Category)
import qualified "base" Control.Category as Category
import           "base" Data.Typeable

--------------------------------------------------

import qualified "base" Control.Exception   as E
import qualified "base" System.Environment  as IO

import qualified "base" Data.List as List

import           "base" Data.Foldable
-- import           "base" Text.Read

--------------------------------------------------

import qualified "base" Prelude
import           "base" Prelude hiding
  ( (<), (>)
  , map
  , sequence, sequence_
  )

--------------------------------------------------
-- Imports: CPP ----------------------------------
--------------------------------------------------

#if HAS_BASE_NonEmpty
import "base"       Data.List.NonEmpty         (NonEmpty)
--import qualified Data.List.NonEmpty as NonEmpty
#else
import "semigroups" Data.List.NonEmpty         (NonEmpty)
#endif

--------------------------------------------------

#if HAS_BASE_Semigroup
import qualified "base"       Data.Semigroup as Semigroup
import           "base"       Data.Semigroup ((<>))
#else
import qualified "semigroups" Data.Semigroup as Semigroup
import           "semigroups" Data.Semigroup ((<>))
#endif

--------------------------------------------------

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
import "base" Data.Foldable              (sequenceA_,toList)
import "base" Data.Traversable           (sequenceA)
#else
import "base" Data.Foldable              (sequenceA_)
#endif

--------------------------------------------------

#if IS_COMPILER_ghc
import "base" GHC.Exts                   (IsString(..))
#endif

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

-- | @Natural@ between `minimumPrecedence` and `maximumPrecedence`, inlcusively (i.e. @[0 .. 11]@).

type HaskellPrecedence = Int

--------------------------------------------------

{-| A number of microseconds (there are one million microseconds per second). An integral number because it's the smallest resolution for most GHC functions. @Int@ because GHC frequently represents integrals as @Int@s (for efficiency). 

Has smart constructors for common time units; in particular, for thread delays, and for human-scale durations.

* 'microseconds'
* 'milliseconds'
* 'seconds'
* 'minutes'
* 'hours'

Which also act as self-documenting (psuedo-keyword-)arguments for 'threadDelay', via 'delayFor'. 

-}
newtype Time = Time { toMicroseconds :: Int }

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------
-- "soft" overrides...

-- | (generalization)
-- @= 'fmap'@
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

{-# INLINEABLE map #-}

--------------------------------------------------

-- | (generalization)
-- @= 'sequenceA'@
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = sequenceA

{-# INLINEABLE sequence #-}

--------------------------------------------------

-- | (generalization)
-- @ = 'sequenceA_'@
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = sequenceA_

{-# INLINEABLE sequence_ #-}

--------------------------------------------------
-- "hard" overrides...

{- | forwards composition

e.g. "f, then g, then h"

@
forwards x
 = x
 & f
 > g
 > h
@

same precedence/associativity as '.'

-}
(>) :: (a -> b) -> (b -> c) -> (a -> c)
(>) = (>>>)
infixr 9 >

{-# INLINEABLE (>) #-}

--------------------------------------------------

{- | backwards composition

e.g. "h, after g, after f"

@
backwards x
 = h
 < g
 < f
 $ x
@

same precedence/associativity as '.'

-}
(<) :: (b -> c) -> (a -> b) -> (a -> c)
(<) = (<<<)
infixr 9 <

{-# INLINEABLE (<) #-}

---------------------------------------

-- NOTE
-- infixr 1 <<<
-- infixr 9 .
-- function application (i.e. whitespace juxtaposition) is like: infixl 10 _
-- infixr 0 $
-- infixl 1 &

-- | same precedence/associativity as "Prelude.<"
lessThan :: Ord a => a -> a -> Bool
lessThan = (Prelude.<)
infix 4 `lessThan`

{-# INLINEABLE lessThan #-}

--------------------------------------------------

-- | same precedence/associativity as "Prelude.>"
greaterThan :: Ord a => a -> a -> Bool
greaterThan = (Prelude.>)
infix 4 `greaterThan`

{-# INLINEABLE greaterThan #-}

--------------------------------------------------

{- | @(-:) = (,)@

fake dictionary literal syntax:

@
 [ "a"-: 1
 , "b"-: 2
 , "c"-: 1+2
 ] :: [(String,Integer)]
@

-}
(-:) :: a -> b -> (a,b)
(-:) = (,)
infix 1 -:

{-# INLINEABLE (-:) #-}

--------------------------------------------------
--------------------------------------------------

todo :: a --TODO call stack
todo = error "TODO"
{-# DEPRECATED todo "use { __ERROR__ \"TODO\" }" #-}

--------------------------------------------------

__BUG__ :: SomeException -> a --TODO callstack
__BUG__ = error . show

{-# INLINEABLE __BUG__ #-}

--------------------------------------------------

__ERROR__ :: String -> a --TODO callstack
__ERROR__ = error 

{-# INLINEABLE __ERROR__ #-}

--------------------------------------------------

-- | 'throwM' a 'userError' (a safer 'Prelude.error').

errorM :: (MonadThrow m) => String -> m a
errorM s = throwM e
  where

  e :: IOException
  e = userError s

{-# INLINEABLE errorM #-}

--------------------------------------------------
--------------------------------------------------

-- | @= 'pure' ()@

nothing :: (Applicative m) => m ()
nothing = pure ()

{-# INLINEABLE nothing #-}

{- | Raise a Function Arrow into a Kleisli Arrow.

a convenience function for composing pure functions between "kleislis" in monadic sequences.

Definition:

@= ('pure' .)@

@
returning f ≡ f >>> return

returning f ≡ \x -> return (f x)
@

Usage:

@readFile "example.txt" >>= returning show >>= forceIO
@

-}

returning :: (Applicative m) => (a -> b) -> (a -> m b)
returning f = f > pure

{-# INLINEABLE returning #-}

--------------------------------------------------
--------------------------------------------------

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

{-# INLINEABLE maybe2bool #-}

--------------------------------------------------

maybe2either :: e -> Maybe a -> Either e a 
maybe2either e = maybe (Left e) Right

{-# INLINEABLE maybe2either #-}

--------------------------------------------------

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

{-# INLINEABLE either2maybe #-}

--------------------------------------------------

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)

{-# INLINEABLE either2bool #-}

--------------------------------------------------

maybe2list :: Maybe a -> [a]
maybe2list = maybe [] (:[])

{-# INLINEABLE maybe2list #-}

--------------------------------------------------

list2maybe :: [a] -> Maybe a
list2maybe = \case
 [] -> Nothing
 (x:_) -> Just x

{-# INLINEABLE list2maybe #-}

--------------------------------------------------

#if HAS_BASE_NonEmpty
nonempty2list :: NonEmpty a -> [a]
nonempty2list = toList

{-# INLINEABLE nonempty2list #-}
#endif

--------------------------------------------------

list
  :: r -> (a -> [a] -> r)
  -> ([a] -> r)
list y f = \case
  []     -> y
  (x:xs) -> f x xs

{-# INLINEABLE list #-}

--------------------------------------------------
--------------------------------------------------
-- numbers

-- | 
--
-- @
-- unsafeNatural :: Int -> Natural  
-- @
-- 
unsafeNatural :: Integral i => i -> Natural
unsafeNatural = fromIntegral

{-# INLINEABLE unsafeNatural #-}

--------------------------------------------------

-- | an alias, since @(%)@ is prime symbolic real estate. 
ratio :: Integral a => a -> a -> Ratio a
ratio = (%)

infixl 7 `ratio` -- same as (%)

{-# INLINEABLE ratio #-}

--------------------------------------------------
-- etc

--pure1 :: (Applicative f) => f a ->

-- | @($>) = flip ('<$')@
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

{-# INLINEABLE ($>) #-}

--------------------------------------------------

-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
--
-- NOTE: conflicts with the lens package 
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 5 <&>
{-NOTE
  infixl 4 <$>
  infixl 4 <*>
-}

{-# INLINEABLE (<&>) #-}

--------------------------------------------------
-- list utilities...

{- | Remove duplicates (from the given list).

== Examples

>>> ordNub "abcab"
"abc"
>>> ordNub ""
""

== Definition

@
ordNub = 'ordNubBy' 'id'
@

== Laws

Idempotent (i.e. multiple applications are redundant)

Stable (i.e. preserves the original order) 

== Performance

Semantically, @ordNub@ should be equivalent to 'Data.List.nub'.

Operationally, it's much faster for large lists:

* @nub@ — only needs an @Eq@ constraint, but takes @O(n^2)@ time complexity.
* @ordNub@ — needs an @Ord@ constraint, but only takes @O(n log n)@ time complexity.

== Links

* <http://github.com/nh2/haskell-ordnub>, by /Niklas Hambüchen/.

-}

ordNub :: Ord a => [a] -> [a]
ordNub = ordNubBy id

{-# INLINEABLE ordNub #-}

--------------------------------------------------

{- | Selects a key for each element, and takes the @nub@ based on that key.

See 'ordNub'.

-}

ordNubBy :: Ord b => (a -> b) -> [a] -> [a]
ordNubBy f = \l -> go Set.empty l
  where

  go !_ [] = []
  go !s (x:xs)

      | y `Set.member` s = go s xs
      | otherwise        = let !s' = Set.insert y s
                            in x : go s' xs
      where
        y = f x

{-# INLINEABLE ordNubBy #-}

--------------------------------------------------

{- | Replace a substring.

Notes:

* Replace it everywhere it occurs (not just the first occurence).
* The first argument should not be the empty list (currently, this returns the input).

== Examples

>>> replaceSubstring "el" "_" "Hello Bella"
"H_lo B_la"
>>> replaceSubstring "el" "e" "Hello"
"Helo"
>>> replaceSubstring "" "e" "Hello"
"Hello"

== Laws

@
> ∀ xs ys  →  not (null xs)  ⇒  replaceSubstring xs xs ys ≡ ys
@

== Specializations

@
replaceSubstring :: (Eq a) =>    [a] ->    [a] ->    [a] ->    [a]
replaceSubstring ::           [Char] -> [Char] -> [Char] -> [Char]
@

== Naming

/Subsequence/ vs /Substring/:

* a /subsequence/ is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements.
* a /substring/ is a contiguous sequence of characters within a string.

In Haskell, a @String@ can literally be a sequence of characters.

== Links

* <https://www.stackage.org/haddock/lts/extra/Extra.html#v:replace @Extra@>
* <https://en.wikipedia.org/wiki/Subsequence>
* <https://en.wikipedia.org/wiki/Substring>

-}

replaceSubstring :: (Eq a) => [a] -> [a] -> [a] -> [a]

replaceSubstring []   _    xs = xs
replaceSubstring from into xs
  | Just xs <- List.stripPrefix from xs = into ++ replaceSubstring from into xs

replaceSubstring from into (x:xs) = x : replaceSubstring from into xs
replaceSubstring from into []     = []

{-# SPECIALIZE replaceSubstring :: [Char] -> [Char] -> [Char] -> [Char] #-}

{-TODO-

this:

   prop> \xs ys  ->  not (null xs)  ==>  replaceSubstring xs xs ys == ys

causes:

    Variable not in scope:
      polyQuickCheck
        :: Language.Haskell.TH.Syntax.Name
           -> Language.Haskell.TH.Lib.Internal.ExpQ

-}

--------------------------------------------------

{- | Safely get the @n@-th item in the given list.

>>> nth 1 ['a'..'c']
Just 'b'
>>> nth 1 []
Nothing

-}

nth :: Natural -> [a] -> Maybe a
nth n = go
  where

  go =
    ( List.zip [0..]
    > List.lookup n
    )

{-# INLINEABLE nth #-}

--------------------------------------------------

-- | reverse @cons@
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

{-# INLINEABLE snoc #-}

--------------------------------------------------
-- number utilities...

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral

{-# INLINEABLE toInt #-}

--------------------------------------------------
-- string utilities...

{- | The /tightest/ precedence, for manual @Show@ instances.

== Notes

* Function Application has precedence @10@ (i.e. `applicationPrecedence`).
* @('$')@ has precedence @0@ (i.e. `minimumPrecedence`).

== Usage

@
import \"base\" "Text.Show"

instance Show XYZ where

  'showsPrec' precedence x = 'showParen' (precedence >= 'maximumPrecedence') (s ++)
    where

    s :: String
    s = showXYZ x

    showXYZ = ...

    -- NOTE If « precedence » is `minimumPrecedence`, then « s » is /never/ parenthesized.
    --      If « precedence » is `maximumPrecedence`, then  « s » is /always/ parenthesized,
    --      /unless/ it's an “atomic expression” (e.g. a numeric literal or string literal).
@

-}

maximumPrecedence :: HaskellPrecedence
maximumPrecedence = 11

{-# INLINEABLE maximumPrecedence #-}

--------------------------------------------------

{- | The precedence of Function Application (a /tight/ precedence).

`applicationPrecedence` is the precedence of the /"whitespace operator"/.
In pseudo-Haskell:

@
(␣) f x = f x
infixl 10 ␣
@

See `maximumPrecedence`.

-}

applicationPrecedence :: HaskellPrecedence
applicationPrecedence = 10

{-# INLINEABLE applicationPrecedence #-}

--------------------------------------------------

{- | The /loosest/ precedence.

See `maximumPrecedence`.

-}

minimumPrecedence :: HaskellPrecedence
minimumPrecedence = 0

{-# INLINEABLE minimumPrecedence #-}

--------------------------------------------------

{- | Strip both leading and trailing whitespace.

Whitespace is defined by `Char.isSpace`.
Whitespace characters include:

* @\' \'@
* @\'\t\'@
* @\'\n\'@
* @\'\r\'@

Naming: \"left+right strip\".

-}

lrstrip :: String -> String
lrstrip = rstrip . lstrip

{-# INLINEABLE lrstrip #-}

--------------------------------------------------

{- | Strip /leading/ whitespace.

See `lrstrip`.

Naming: \"left strip\".

-}

lstrip :: String -> String
lstrip = dropWhile Char.isSpace
  
{-# INLINEABLE lstrip #-}

--------------------------------------------------

{- | Strip /trailing/ whitespace.

See `lrstrip`.

Naming: \"right strip\".

-}

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

{-# INLINEABLE rstrip #-}

--------------------------------------------------

shown :: forall a t.
    ( Show a
    , IsString t
    )
  => a
  -> t  
shown = fromString . show

{-# INLINEABLE shown #-}

--------------------------------------------------
-- enum utilities...

{-|

>>> pBool = Proxy :: Proxy Bool
>>> constructors pBool
[False,True]

-}
constructors :: (BoundedEnum a) => proxy a -> [a]
constructors _ = [minBound..maxBound]

{-# INLINEABLE constructors #-}

--------------------------------------------------

{-| like 'constructors', but with an implicit type parameter.

>>> constructors' == [False,True]
True

>> :set -XTypeApplications
>> constructors' @Bool
[False,True]

-}
constructors' :: forall a. (BoundedEnum a) => [a]
constructors' = constructors proxy
  where
  proxy = Proxy :: Proxy a

{-# INLINEABLE constructors' #-}

--------------------------------------------------

typeName
  :: forall proxy a t.
    ( Typeable a
    , IsString t
    )
  => proxy a
  -> t
typeName proxy = typeRep proxy & show & fromString 

{-# INLINEABLE typeName #-}

--------------------------------------------------
-- functions utilities...

identity :: (Category cat) => (a `cat` a)
identity = Category.id

{-# INLINEABLE identity #-}

--------------------------------------------------

-- compose :: (Category (==>)) => (b ==> c) -> (a ==> b) -> (a ==> c)
compose
  :: (Category cat)
  => (b `cat` c)
  -> (a `cat` b)
  -> (a `cat` c)  
compose = (Category.<<<)

{-# INLINEABLE compose #-}

--------------------------------------------------
-- monad utilities...

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM check action = do

  b <- check

  when b action

{-# INLINEABLE whenM #-}

--------------------------------------------------

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM check = whenM (not <$> check)

{-# INLINEABLE unlessM #-}

--------------------------------------------------

-- | @= 'flip' 'runReaderT'@
runReaderT' :: r -> (ReaderT r) m a -> m a
runReaderT' = flip runReaderT

{-# INLINEABLE runReaderT' #-}

-- | @= 'flip' 'runStateT'@
runStateT' :: s -> (StateT s) m a -> m (a, s)
runStateT' = flip runStateT

{-# INLINEABLE runStateT' #-}

-- | @= 'flip' 'evalStateT'@
evalStateT' :: (Monad m) => s -> (StateT s) m a -> m a
evalStateT' = flip evalStateT

{-# INLINEABLE evalStateT' #-}

-- | @= 'flip' 'execStateT'@
execStateT' :: (Monad m) => s -> (StateT s) m a -> m s
execStateT' = flip execStateT

{-# INLINEABLE execStateT' #-}

-- | @= 'flip' 'runReader'@
runReader' :: r -> Reader r a -> a
runReader' = flip runReader

{-# INLINEABLE runReader' #-}

-- | @= 'flip' 'runState'@
runState' :: s -> State s a -> (a, s)
runState' = flip runState

{-# INLINEABLE runState' #-}

-- | @= 'flip' 'evalState'@
evalState' :: s -> State s a -> a
evalState' = flip evalState

{-# INLINEABLE evalState' #-}

-- | @= 'flip' 'execState'@
execState' :: s -> State s a -> s
execState' = flip execState

{-# INLINEABLE execState' #-}

--------------------------------------------------
-- filepath utilities...

{-| 'isAbsoluteOnAnyPlatform' and 'isRelativeOnAnyPlatform' are like
'System.FilePath.isAbsolute' and 'System.FilePath.isRelative' but have
platform independent heuristics.

The System.FilePath exists in two versions, Windows and Posix. The two
versions don't agree on what is a relative path and we don't know if we're
given Windows or Posix paths.
This results in false positives when running on Posix and inspecting
Windows paths, like the hackage server does.

> System.FilePath.Posix.isAbsolute \"C:\\hello\" == False
> System.FilePath.Windows.isAbsolute \"/hello\" == False

This means that we would treat paths that start with \"/\" to be absolute.
On Posix they are indeed absolute, while on Windows they are not.

The portable versions should be used when we might deal with paths that
are from another OS than the host OS. For example, the Hackage Server
deals with both Windows and Posix paths while performing the
PackageDescription checks. In contrast, when we run 'cabal configure' we
do expect the paths to be correct for our OS and we should not have to use
the platform independent heuristics.

== Examples

>>> isAbsoluteOnAnyPlatform "C:\\\\Users"
True
>>> isAbsoluteOnAnyPlatform "/home"
True

-}

isAbsoluteOnAnyPlatform :: FilePath -> Bool
isAbsoluteOnAnyPlatform = \case

  -- « POSIX » root:

  ('/' : _)              -> True

  -- e.g. « C:\\directory »:

  (drive : ':':'\\' : _) -> isAlpha drive

  -- « UNC »:

  ('\\' : '\\' : _)      -> True

  _                      -> False

--------------------------------------------------

-- | @isRelativeOnAnyPlatform = not . 'isAbsoluteOnAnyPlatform'@
isRelativeOnAnyPlatform :: FilePath -> Bool
isRelativeOnAnyPlatform = not . isAbsoluteOnAnyPlatform

--------------------------------------------------
-- Time...

microseconds :: Int -> Time
milliseconds :: Int -> Time
seconds      :: Int -> Time
minutes      :: Int -> Time
hours        :: Int -> Time

{-# INLINEABLE microseconds #-}
{-# INLINEABLE milliseconds #-}
{-# INLINEABLE seconds #-}
{-# INLINEABLE minutes #-}
{-# INLINEABLE hours #-}

microseconds = Time
milliseconds = Time . (\t -> t*1000)
seconds      = Time . (\t -> t*1000*1000)
minutes      = Time . (\t -> t*1000*1000*1000)
hours        = Time . (\t -> t*1000*1000*1000*1000)

--------------------------------------------------

delayFor :: (MonadIO m) => Time -> m ()
delayFor = toMicroseconds >>> threadDelay >>> liftIO 

{-# INLINEABLE delayFor #-}

delayMicroseconds :: (MonadIO m) => Int -> m ()
delayMicroseconds i = delayFor (microseconds i)

{-# INLINEABLE delayMicroseconds #-}

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds i = delayFor (milliseconds i)

{-# INLINEABLE delayMilliseconds #-}

delaySeconds :: (MonadIO m) => Int -> m ()
delaySeconds i = delayFor (seconds i)

{-# INLINEABLE delaySeconds #-}

--------------------------------------------------

{-

{-| Normally used time units, for threads and for human-scale durations.

A psuedo-keyword-argument for 'threadDelay'. 

-}
data SimpleTimeUnit
  = Nanoseconds  Double
  | Milliseconds Double
  | Seconds      Double
  | Minutes      Double
  | Hours        Double

toNanoseconds :: SimpleTimeUnit -> Int
toNanoseconds = go > fromIntegral
 where
 go = \case
   Nanoseconds  i -> i
   Milliseconds i -> i * (1000)
   Seconds      i -> i * (1000*1000)
   Minutes      i -> i * (1000*1000*1000)
   Hours        i -> i * (1000*1000*1000*1000)

delayFor :: (MonadIO m) => SimpleTimeUnit -> m ()
delayFor = toNanoseconds >>> threadDelay >>> liftIO 

delayNanoseconds :: (MonadIO m) => Int -> m ()
delayNanoseconds i = delayFor (Nanoseconds i)

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds i = delayFor (Milliseconds i)

delaySeconds :: (MonadIO m) => Int -> m ()
delaySeconds i = delayFor (Seconds i)

-- liftIO . threadDelay . (*1000) . (*1000)
-}

--------------------------------------------------
-- IO

io :: MonadIO m => IO a -> m a
io = liftIO

{-# INLINEABLE io #-}

--------------------------------------------------

forkever_ :: IO () -> IO ()
forkever_ = void . forkever Nothing

{-# INLINEABLE forkever_ #-}

--------------------------------------------------

forkever ::Maybe Int -> IO () -> IO ThreadId
forkever t m = forkIO $ forever $ do
    m
    _delay
    where
    _delay = maybe nothing delayMilliseconds t

{-# INLINEABLE forkever #-}

--------------------------------------------------

--TODO -- | Call once to start, then call repeatedly to get the elapsed time since the first call.
-- --   The time is guaranteed to be monotonic. This function is robust to system time changes.
-- --
-- -- > do f <- offsetTime; xs <- replicateM 10 f; return $ xs == sort xs
-- --
-- -- (inlined from the `extra` package)
-- offsetTime :: IO (IO Seconds)
-- offsetTime = do
--     start <- time
--     return $ do
--         end <- time
--         return $ 1e-9 * fromIntegral (toNanoSecs $ end - start)
--     where time = getTime Monotonic

-- | @= 'evaluate' . 'force'@
forceIO :: NFData a => a -> IO a
forceIO = evaluate . force

{-# INLINEABLE forceIO #-}

--------------------------------------------------

-- | @~ 'forceIO'@
forceIO_ :: NFData a => a -> IO ()
forceIO_ = void . evaluate . force

{-# INLINEABLE forceIO_ #-}

--------------------------------------------------
--------------------------------------------------

--TODO: MonadIO

{-| Return the value of the first environment variable that's been set, or a default value if all are unset.

Examples:

@
> firstSetEnvironmentVariable \"\/usr\/run\" [ \"XDG_RUNTIME_HOME\", \"TMP\" ]
@

Properties:

@
firstSetEnvironmentVariable x [] ≡ return x
@

-}

firstSetEnvironmentVariable :: String -> [String] -> IO String
firstSetEnvironmentVariable =

  firstEnvironmentVariableSatisfying (const True)

{-# INLINEABLE firstSetEnvironmentVariable #-}

--------------------------------------------------

-- {-| Return the first __numerical__ (@Int@) value among the given environment variables, or a default number.

-- Examples:

-- @
-- > firstReadableEnvironmentVariable 1 [ "EUID", "UID" ]
-- @

-- Properties:

-- @
-- firstReadableEnvironmentVariable n [] ≈ return n
-- @

-- -}

-- firstIntEnvironmentVariable :: Int -> [String] -> IO Int
-- firstIntEnvironmentVariable = firstReadableEnvironmentVariable

-- {-# INLINEABLE firstIntEnvironmentVariable #-}

--------------------------------------------------

-- {-| Return the first __numerical__ value among the given environment variables, or a default number.

-- Properties:

-- @
-- firstReadableEnvironmentVariable x [] ≈ return x
-- @

-- -}

-- firstReadableEnvironmentVariable :: (Read a) => a -> [String] -> IO a
-- firstReadableEnvironmentVariable x variables = to <$>

--   firstEnvironmentVariableSatisfying is (show x) variables

--   where

--   to = readMaybe > maybe x id
--   is = readMaybe > maybe False (const True)

-- {-# INLINEABLE firstReadableEnvironmentVariable #-}

--------------------------------------------------

{-| Return the first __nonempty__ value among the given environment variables, or a default value if all are either unset or set-to-empty.

Examples:

@
> firstNonemptyEnvironmentVariable "/usr/run" [ "XDG_RUNTIME_HOME", "TMP" ]
@

Properties:

@
firstNonemptyEnvironmentVariable x [] ≈ return x
@

Notes:

* on Windows, @firstNonemptyEnvironmentVariable@ should be equivalent to 'firstSetEnvironmentVariable'.

-}

firstNonemptyEnvironmentVariable :: String -> [String] -> IO String
firstNonemptyEnvironmentVariable =

  firstEnvironmentVariableSatisfying (/= "")

{-# INLINEABLE firstNonemptyEnvironmentVariable #-}

--------------------------------------------------

firstEnvironmentVariableSatisfying :: (String -> Bool) -> String -> [String] -> IO String
firstEnvironmentVariableSatisfying predicate = \x0 variables -> do

  x' <- foldrM go Nothing variables

  let x = x' & maybe x0 Semigroup.getFirst

  E.evaluate (force x)

  where

  go :: String -> Maybe (Semigroup.First String) -> IO (Maybe (Semigroup.First String))
  go n y = do

    x <- IO.lookupEnv n

    let x'  = x >>= p
    let x'' = Semigroup.First <$> x'

    let z = x'' <> y

    return z

  p :: String -> Maybe String
  p x = if predicate x then Just x else Nothing

{-# INLINEABLE firstEnvironmentVariableSatisfying #-}

--------------------------------------------------

toLazyByteString :: StrictBytes -> LazyBytes
toLazyByteString

  = LazyBytes.fromChunks
  . (: [])

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------