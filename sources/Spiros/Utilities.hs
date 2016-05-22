{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, TypeOperators, LambdaCase #-}
module Spiros.Utilities where

import Control.Arrow ((>>>),(<<<))
import Control.Exception (SomeException)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

import Prelude hiding ((<),(>))
import qualified Prelude

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

-- | reverse @cons@
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

-- | a natural transformation
type (:~>) f g = forall x. f x -> g x

-- | @($>) = flip ('<$')@
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

__BUG__ :: SomeException -> a --TODO callstack
__BUG__ = error . show

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

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral

todo :: a --TODO call stack
todo = error "TODO"
