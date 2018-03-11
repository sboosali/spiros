{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PackageImports, TypeOperators, LambdaCase, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, PolyKinds, KindSignatures, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# OPTIONS_HADDOCK not-home #-}

{-|

These identifiers are "soft" overrides, they generalize the signatures of their @Prelude@ namesakes:

* 'map': @() => []@ to @(Functor f) => f@
* 'sequence': @Monad@ to @Applicative@
* 'sequence_': @Monad@ to @Applicative@

These symbols are "hard" overrides, they are completely different from @Prelude@:

* '>': forward-composition
* '<': backward-composition (i.e. '(.)')

-}
module Prelude.Spiros.Utilities where

-- non-exports

--TODO import "clock" System.Clock
import "vinyl" Data.Vinyl.Functor

import "mtl" Control.Monad.Reader
  (ReaderT,Reader,runReaderT,runReader)
import "mtl" Control.Monad.State
  (StateT,State,runStateT,evalStateT,execStateT,runState,evalState,execState)

import "deepseq" Control.DeepSeq (NFData,force)

import qualified "text" Data.Text      as TS
import qualified "text" Data.Text.Lazy as TL

import qualified "bytestring" Data.ByteString      as BS 
import qualified "bytestring" Data.ByteString.Lazy as BL 

import qualified "template-haskell" Language.Haskell.TH.Syntax as TemplateHaskell

--

import "base" Data.Function ((&))
import "base" Data.Functor.Product
import "base" Control.Arrow ((>>>),(<<<))
import "base" Control.Exception (SomeException,evaluate)
import "base" Control.Concurrent (threadDelay,forkIO,ThreadId)
import "base" Control.Monad (forever, void)
import "base" Data.Proxy
import "base" Data.String(IsString)
import "base" Control.Monad.IO.Class
import "base" Data.Foldable (sequenceA_,toList)
import "base" Data.Traversable (sequenceA)
import "base" Data.List.NonEmpty (NonEmpty)
--import qualified Data.List.NonEmpty as NonEmpty
import "base" Numeric.Natural

import           "base" Control.Category (Category)
import qualified "base" Control.Category as Category
import           "base" Data.Typeable
import           "base" GHC.Exts (IsString(..))

--

import qualified "base" Prelude
import           "base" Prelude hiding
  ( (<), (>)
  , map
  , sequence, sequence_
  )

----------------------------------------

-- | alphanumeric alias
type List a = [a]

type StrictText = TS.Text
type LazyText   = TL.Text 

type StrictBytes = BS.ByteString
type LazyBytes   = BL.ByteString

{-| a haskell identifier, via @TemplateHaskellQuotes@.

@
> :set -XTemplateHaskellQuotes
> \'fmap :: 'HaskellName'
@

-}
type HaskellName = TemplateHaskell.Name

{-| a finite type,
whose values may be enumerated into a finite list.

-}
type BoundedEnum a = (Enum a, Bounded a)

{-| for `interpolatedstring-perl6`
i.e. the type supports string literals (via 'IsString') and can be appended (via 'Monoid').

uses @ConstraintKinds@.

e.g.

@
-- -XQuasiQuotes
import Text.InterpolatedString.Perl6 (qq)

hello :: (CanInterpolate t) => t -> t
hello t = [qc| "hello" ++ $t |]

helloworld = hello "world" :: String
@

-}
type CanInterpolate t = (IsString t, Monoid t)
  
-- | (from vinyl)
type I = Identity

-- | (from vinyl)
type C = Const

type P = Proxy

-- | a natural transformation
type (:~>) f g = forall x. f x -> g x

-- |
type (:*:) = Product

-- |
type (f :. g) x = f (g x)

-- | (from vinyl)
type (:.:) = Compose

pattern I :: a -> Identity a
pattern I x = Identity x

pattern C :: forall a (b :: k). a -> Const a b
pattern C x = Const x

pattern P :: forall (a :: k). Proxy a
pattern P = Proxy

pattern (:*:) :: f a -> g a -> Product f g a
pattern f :*: g = (Pair f g)

----------------------------------------
-- soft overrides

-- | (generalization)
-- @= 'fmap'@
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- | (generalization)
-- @= 'sequenceA'@
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = sequenceA

-- | (generalization)
-- @ = 'sequenceA_'@
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = sequenceA_

----------------------------------------
-- hard overrides

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

----------------------------------------

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

-- | same precedence/associativity as "Prelude.>"
greaterThan :: Ord a => a -> a -> Bool
greaterThan = (Prelude.>)
infix 4 `greaterThan`

----------------------------------------

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

todo :: a --TODO call stack
todo = error "TODO"
{-# DEPRECATED todo "use { __ERROR__ \"TODO\" }" #-}

__BUG__ :: SomeException -> a --TODO callstack
__BUG__ = error . show

__ERROR__ :: String -> a --TODO callstack
__ERROR__ = error 

-- | @= pure ()@
nothing :: (Applicative m) => m ()
nothing = pure ()

-- nothing :: (Monad m) => m ()
-- nothing = return ()

----------------------------------------

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

maybe2either :: e -> Maybe a -> Either e a 
maybe2either e = maybe (Left e) Right

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)

maybe2list :: Maybe a -> [a]
maybe2list = maybe [] (:[])

list2maybe :: [a] -> Maybe a
list2maybe = \case
 [] -> Nothing
 (x:_) -> Just x

nonempty2list :: NonEmpty a -> [a]
nonempty2list = toList

----------------------------------------
-- numbers

-- | 
--
-- @
-- unsafeNatural :: Int -> Natural  
-- @
-- 
unsafeNatural :: Integral i => i -> Natural
unsafeNatural = fromIntegral

----------------------------------------
-- etc

-- | @($>) = flip ('<$')@
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
--
-- NOTE: conflicts with the lens package 
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

infixl 5 <&>
{-NOTE
  infixl 4 <$>
  infixl 4 <*>
-}

-- | reverse @cons@
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral

-- | safely-partial @(!)@
index :: (Integral n) => [a] -> n -> Maybe a
index [] _ = Nothing
index (x:xs) n
 | n == 0         = Just x
 | n `lessThan` 0 = Nothing
 | otherwise      = index xs (n-1)

strip :: String -> String
strip = rstrip . lstrip

lstrip :: String -> String
lstrip = dropWhile (`elem` (" \t\n\r"::String))

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

shown :: forall a t.
    ( Show a
    , IsString t
    )
  => a
  -> t  
shown = fromString . show

{-|

>>> pBool = Proxy :: Proxy Bool
>>> constructors pBool
[False,True]

-}
constructors :: (BoundedEnum a) => proxy a -> [a]
constructors _ = [minBound..maxBound]

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

identity :: (Category cat) => (a `cat` a)
identity = Category.id

-- compose :: (Category (==>)) => (b ==> c) -> (a ==> b) -> (a ==> c)
compose
  :: (Category cat)
  => (b `cat` c)
  -> (a `cat` b)
  -> (a `cat` c)  
compose = (Category.<<<)

typeName
  :: forall proxy a t.
    ( Typeable a
    , IsString t
    )
  => proxy a
  -> t
typeName proxy = typeRep proxy & show & fromString 

----------------------------------------

-- | @= 'flip' 'runReaderT'@
runReaderT' :: r -> (ReaderT r) m a -> m a
runReaderT' = flip runReaderT

-- | @= 'flip' 'runStateT'@
runStateT' :: s -> (StateT s) m a -> m (a, s)
runStateT' = flip runStateT

-- | @= 'flip' 'evalStateT'@
evalStateT' :: (Monad m) => s -> (StateT s) m a -> m a
evalStateT' = flip evalStateT

-- | @= 'flip' 'execStateT'@
execStateT' :: (Monad m) => s -> (StateT s) m a -> m s
execStateT' = flip execStateT

-- | @= 'flip' 'runReader'@
runReader' :: r -> Reader r a -> a
runReader' = flip runReader

-- | @= 'flip' 'runState'@
runState' :: s -> State s a -> (a, s)
runState' = flip runState

-- | @= 'flip' 'evalState'@
evalState' :: s -> State s a -> a
evalState' = flip evalState

-- | @= 'flip' 'execState'@
execState' :: s -> State s a -> s
execState' = flip execState

----------------------------------------
-- Time

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

microseconds :: Int -> Time
milliseconds :: Int -> Time
seconds      :: Int -> Time
minutes      :: Int -> Time
hours        :: Int -> Time

microseconds = Time
milliseconds = Time . (\t -> t*1000)
seconds      = Time . (\t -> t*1000*1000)
minutes      = Time . (\t -> t*1000*1000*1000)
hours        = Time . (\t -> t*1000*1000*1000*1000)

delayFor :: (MonadIO m) => Time -> m ()
delayFor = toMicroseconds >>> threadDelay >>> liftIO 

delayMicroseconds :: (MonadIO m) => Int -> m ()
delayMicroseconds i = delayFor (microseconds i)

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds i = delayFor (milliseconds i)

delaySeconds :: (MonadIO m) => Int -> m ()
delaySeconds i = delayFor (seconds i)

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

----------------------------------------
-- IO

io :: MonadIO m => IO a -> m a
io = liftIO

forkever_ :: IO () -> IO ()
forkever_ = void . forkever Nothing

forkever ::Maybe Int -> IO () -> IO ThreadId
forkever t m = forkIO $ forever $ do
    m
    _delay
    where
    _delay = maybe nothing delayMilliseconds t

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

-- | @~ 'forceIO'@
forceIO_ :: NFData a => a -> IO ()
forceIO_ = void . evaluate . force

----------------------------------------
