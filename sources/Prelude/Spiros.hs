{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

{-|

exports:

* single-character composition, i.e. ('>') an ('<')
* universally (or frequently) derived classes,
i.e. @deriving (...,'Data','Generic','NFData','Semigroup')@
* @safe-exceptions@'s @'throw'@, which generalizes @IO@ to 'MonadThrow'
* type names for common types (lazy text, lazy bytes, etc)
* and more (see the source)

hides:

* partial functions, e.g. 'head'

see:

* <http://www.stephendiehl.com/posts/protolude.html>

-}
module Prelude.Spiros
 ( module Base
 , module X -- re-eXports
 )
where

import Prelude.Spiros.Utilities                         as X

--------------------------------------------------------------------------------

import "safe" Safe                              as X

import "hashable" Data.Hashable                 as X (Hashable(..),hashUsing) -- for defining instances manually 

import "data-default-class" Data.Default.Class  as X (Default(..))

-- https://www.fpcomplete.com/blog/2016/06/announce-safe-exceptions
import "safe-exceptions" Control.Exception.Safe as X -- TODO mv so module, like Spiros.Exceptions?
-- import "exceptions" Control.Monad.Catch         as X (MonadThrow(..))

-- import "unordered-containers" Data.HashSet        as X (HashSet)
-- import "unordered-containers" Data.HashMap.Strict as X (HashMap)

--------------------------------------------------------------------------------
-- the standard library (i.e. the libraries GHC bootstraps with, and thus are always available)

import "deepseq" Control.DeepSeq                as X (NFData(..))

import "text" Data.Text.Lazy                    as X (Text)         --  lazy 

import "bytestring" Data.ByteString.Lazy        as X (ByteString)   -- lazy 

import "transformers" Control.Monad.Trans.Class as X (MonadTrans(..))

import "mtl" Control.Monad.Reader               as X
import "mtl" Control.Monad.Except               as X
import "mtl" Control.Monad.State                as X
-- import "mtl" Control.Monad.Writer.Strict        as X

import "containers" Data.Set                    as X (Set)
import "containers" Data.Map                    as X (Map)

import "base" GHC.Generics                      as X (Generic)
import "base" Data.Data                         as X (Data)
import "base" Data.Function                     as X ((&),on)
import "base" Data.Foldable                     as X (traverse_)
import "base" Control.Arrow                     as X ((>>>),(<<<))
import "base" Numeric.Natural                   as X (Natural)
import "base" Data.Proxy                        as X (Proxy(..))
import "base" Control.Applicative               as X
import "base" Data.Ix                           as X (Ix) 
import "base" Data.Bits                         as X (Bits,FiniteBits)
import "base" Data.Functor.Identity             as X (Identity(..)) 
import "base" Control.Monad.IO.Class    as X (MonadIO(..))

-- TODO when were they merged into base from semi groups? 
import "base" Data.Semigroup                    as X (Semigroup(..))
import "base" Data.List.NonEmpty                     as X (NonEmpty(..))  

#if MIN_VERSION_base(4,8,0)
#else
import Data.Functor as X ((<$>))
import Data.Monoid as X (Monoid(..))
#endif

--------------------------------------------------------------------------------
-- the Prelude

import Data.List as Base hiding
 ( minimumBy  -- partials
 , maximumBy
 , (!!)
 , find
 )

import Prelude as Base hiding
 ( (<)
 , (>)
 -- partials
 , error, undefined
 , tail
 , init
 , head
 , last
 , minimum
 , maximum
 , foldr1
 , foldl1
 , foldl1
 , scanl1
 , scanr1
 , read
 , toEnum
 )

--------------------------------------------------------------------------------