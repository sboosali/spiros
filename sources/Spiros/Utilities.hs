{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, TypeOperators, LambdaCase, PatternSynonyms #-}
{-# LANGUAGE PolyKinds, KindSignatures, ConstraintKinds #-}
{-# OPTIONS_HADDOCK not-home #-}

module Spiros.Utilities where

import Data.Vinyl.Functor

import Data.Functor.Product
import Control.Arrow ((>>>),(<<<))
import Control.Exception (SomeException)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Proxy
import Data.String(IsString)

import Prelude hiding ((<),(>))
import qualified Prelude

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

__BUG__ :: SomeException -> a --TODO callstack
__BUG__ = error . show

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

nothing :: (Monad m) => m ()
nothing = return ()

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

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

-- | reverse @cons@
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

-- | @($>) = flip ('<$')@
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral

-- | safely-partial @(!)@
index :: (Integral n, Num n) => [a] -> n -> Maybe a
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
