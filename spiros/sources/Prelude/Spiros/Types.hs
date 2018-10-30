
{-| re-export custom data types that are generally useful for me.

the @newtype@s distinguish them with more meaningful names (of constructors, accessors, transformers, etc), but still support the convenience of automatically deriving as many instances as correct. 

-}

module Prelude.Spiros.Types where

--module Prelude.Spiros.Types ( module X ) where

----------------------------------------

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

