{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------
--------------------------------------------------

{-| re-export custom data types that are generally useful for me.

the @newtype@s distinguish them with more meaningful names (of constructors, accessors, transformers, etc), but still support the convenience of automatically deriving as many instances as correct. 

-}

module Prelude.Spiros.Types where

#include "sboo-base-feature-macros.h"

--------------------------------------------------
-- Imports: 1st Party ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports: 2nd Party ----------------------------
--------------------------------------------------

import qualified "text" Data.Text      as TS
import qualified "text" Data.Text.Lazy as TL

--------------------------------------------------

import qualified "bytestring" Data.ByteString      as BS 
import qualified "bytestring" Data.ByteString.Lazy as BL 

--------------------------------------------------

import qualified "template-haskell" Language.Haskell.TH.Syntax as TemplateHaskell

--------------------------------------------------

import "base" Data.Proxy      (Proxy(..))
import "base" Data.String     (IsString)

--import "base"  ()
import "base" Data.Functor.Const    (Const(..))
import "base" Data.Functor.Compose  (Compose(..))
import "base" Data.Functor.Identity (Identity(..))
import "base" Data.Functor.Product  (Product(..))
import "base" Data.Functor.Sum      (Sum(..))

--------------------------------------------------
-- Imports: 3rd Party ----------------------------
--------------------------------------------------

--import "vinyl" Data.Vinyl.Functor

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

-- | alphanumeric alias
type List a = [a]

--------------------------------------------------

{-| a finite type,
whose values may be enumerated into a finite list.

-}
type BoundedEnum a = (Enum a, Bounded a)

--------------------------------------------------

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
  
--------------------------------------------------
--------------------------------------------------

type StrictText = TS.Text
type LazyText   = TL.Text 

--------------------------------------------------

type StrictBytes = BS.ByteString
type LazyBytes   = BL.ByteString

--------------------------------------------------

{-| a haskell identifier, via @TemplateHaskellQuotes@.

@
> :set -XTemplateHaskellQuotes
> \'fmap :: 'HaskellName'
@

-}
type HaskellName = TemplateHaskell.Name

--------------------------------------------------
--------------------------------------------------

-- | @I a ≡ a@
type I = Identity

--------------------------------------------------

-- | @C a b ≡ a@
type C = Const

--------------------------------------------------

-- | @P a ≡ ()@
type P = Proxy

--------------------------------------------------
--------------------------------------------------

-- | @(f :+: g) a  ≡  Either (f a) (g a)@
type (:+:) = Sum

--------------------------------------------------

-- | @(f :*: g) a  ≡  (f a, g a)@
type (:*:) = Product

--------------------------------------------------

-- | @ (f :.: g) a  ≡  f (g a)@
type (:.:) = Compose

--------------------------------------------------
--------------------------------------------------

-- |
type (f :. g) x = f (g x)

--------------------------------------------------

-- | a natural transformation
type (:~>) f g = forall x. f x -> g x

--------------------------------------------------
--------------------------------------------------

pattern I :: a -> Identity a
pattern I x = Identity x

#if HAS_PRAGMA_COMPLETE
{-# COMPLETE I #-}
#endif

--------------------------------------------------

pattern C :: forall a (b :: k). a -> Const a b
pattern C x = Const x

--------------------------------------------------

pattern P :: forall (a :: k). Proxy a
pattern P = Proxy

--------------------------------------------------

pattern (:*:) :: f a -> g a -> Product f g a
pattern f :*: g = (Pair f g)

--TODO use base's functors, not vinyl's.

--------------------------------------------------
--------------------------------------------------

{-NOTES

Show2 Either

Read2 Either

Ord2 Either

Eq2 Either

Bifunctor Either

Bifoldable Either

Bitraversable Either

Monad (Either e)

Functor (Either a)

MonadFix (Either e)

Applicative (Either e)

Foldable (Either a)

Traversable (Either a)

Show a => Show1 (Either a)

Read a => Read1 (Either a)

Ord a => Ord1 (Either a)

Eq a => Eq1 (Either a)

Generic1 * (Either a)

(Data a, Data b) => Data (Either a b)

(Ord b, Ord a) => Ord (Either a b)

(Read b, Read a) => Read (Either a b)	 

(Show b, Show a) => Show (Either a b)

Semigroup (Either a b)

type Rep1 * (Either a)

type Rep (Either a b)

type (==) (Either k1 k2) a b

-}
