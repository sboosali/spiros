{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

{-|
Module      :  Prelude.Spiros
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

(Re-)Exports:

* single-character composition, i.e. ('>') an ('<')
* universally (or frequently) derived classes,
i.e. @deriving (...,'Data','Generic','NFData','Semigroup')@
* @safe-exceptions@'s @'throw'@, which generalizes @IO@ to 'MonadThrow'
* type names for common types (lazy text, lazy bytes, etc)
* and more (see the source)

Hides:

* partial functions, e.g. 'head'
* some aliased functions (like @sequence@, which is generalized into @sequenceA@).

Also see:

* <http://www.stephendiehl.com/posts/protolude.html>

-}
module Prelude.Spiros
 ( module Base
 , module X -- re-eXports
 )
where

import Prelude.Spiros.Utilities                      as X

----------------------------------------

import "safe" Safe                                   as X

----------------------------------------

import "hashable" Data.Hashable                      as X
 ( Hashable(..)
 , hashUsing
   -- for defining instances manually 
 )

----------------------------------------

import "data-default-class" Data.Default.Class       as X
 (Default(..))

----------------------------------------
-- https://www.fpcomplete.com/blog/2016/06/announce-safe-exceptions

import "safe-exceptions" Control.Exception.Safe      as X -- TODO mv so module, like Spiros.Exceptions?
-- import "exceptions" Control.Monad.Catch           as X (MonadThrow(..))

----------------------------------------

-- import "unordered-containers" Data.HashSet        as X (HashSet)
-- import "unordered-containers" Data.HashMap.Strict as X (HashMap)

-- import "protolude" Protolude                      as X

import "string-conv" Data.String.Conv                as X
 ( StringConv (..)
 , Leniency (..)
 , toS
 , toSL
 , convS
 , convSL
 )

----------------------------------------
-- the standard library (i.e. the libraries GHC bootstraps with, and thus are always available)

import "deepseq" Control.DeepSeq                     as X (NFData(..))

----------------------------------------

import "text" Data.Text.Lazy                         as X (Text)         --  lazy 

----------------------------------------

import "bytestring" Data.ByteString.Lazy             as X (ByteString)   -- lazy 

----------------------------------------

import "transformers" Control.Monad.Trans.Class      as X (MonadTrans(..))

----------------------------------------

--import "mtl" Control.Monad.Trans                   as X (MonadTrans(..))

import "mtl" Control.Monad.Reader                    as X
 (
    MonadReader(..),
    asks,
    Reader,
    runReader,
    mapReader,
    withReader,
    ReaderT(ReaderT),
    runReaderT,
    mapReaderT,
    withReaderT,
 )

import "mtl" Control.Monad.State                     as X
 (
    MonadState(..),
    modify,
    modify',
    gets,
    State,
    runState,
    evalState,
    execState,
    mapState,
    withState,
    StateT(StateT),
    runStateT,
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
 )

import "mtl" Control.Monad.Except                    as X
 (
    MonadError(..),
    ExceptT(ExceptT),
    Except,
    runExceptT,
    mapExceptT,
    withExceptT,
    runExcept,
    mapExcept,
    withExcept,
 )

-- import "mtl" Control.Monad.Writer.Strict          as X

----------------------------------------

import "containers" Data.Set                         as X (Set)
import "containers" Data.Map                         as X (Map)

----------------------------------------

--import "base" Control.Exception                    as X (evaluate)

import "base" Data.Char                              as X 
import "base" Numeric.Natural                        as X (Natural)
import "base" Data.Maybe                             as X 
import "base" Data.Either                            as X 
import "base" Data.Function                          as X ((&),on,fix)
-- TODO when were they merged into base from semi groups? 
import "base" Data.Semigroup                         as X (Semigroup(..))
import "base" Data.List.NonEmpty                     as X
 ( NonEmpty(..)
   -- unqualified smart constructor
 , nonEmpty
   -- safe versions
 , head, tail, last, init
 , some1, scanl1, scanr1, group1, groupBy1
 )

import "base" Data.Ix                                as X
 ( Ix
 ) 
import "base" Data.Bits                              as X
 ( Bits
 , FiniteBits
 )

import "base" Data.Foldable                          as X
 ( traverse_
 , for_
 , sequenceA_
 )
import "base" Data.Traversable                       as X
 ( sequenceA
 )

import "base" Control.Applicative                    as X  
import "base" Control.Arrow                          as X
  ((&&&),(***),(+++),(|||))
import "base" Control.Monad                          as X
 ( void
 , forever
 , (>=>), (<=<)
 , join
 , guard, when, unless
 )

import "base" Control.Category                       as X
  (Category,(>>>),(<<<))

import "base" Control.Monad.Fail                     as X (MonadFail(..))
import "base" Control.Monad.Fix                      as X (MonadFix(..))
import "base" Control.Monad.IO.Class                 as X (MonadIO(..))

import "base" Data.Proxy                             as X (Proxy(..))
import "base" Data.Functor.Identity                  as X (Identity(..))   
import "base" Data.Coerce                            as X (coerce, Coercible)
import "base" GHC.Exts                               as X
  ( IsList(..)
  , IsString(..)
  , groupWith, sortWith
  )

import "base" GHC.Generics                           as X (Generic)
import "base" Data.Data                              as X (Data)
import "base" Data.Typeable                          as X
 ( Typeable
 , typeRep
 )

----------------------------------------

#if MIN_VERSION_base(4,8,0)
#else
import Data.Functor as X ((<$>))
import Data.Monoid  as X (Monoid(..))
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor as X 
#else
#endif

#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable    as X
import Data.Bitraversable as X
#else
#endif

----------------------------------------
-- the Prelude

import Data.List as Base hiding
  -- partials
 ( (!!)
 , find
 , minimumBy, maximumBy
 , scanl1, scanr1
 , head, tail, last, init
 )

import Prelude as Base hiding
 ( (<), (>)
 -- aliased
 , sequence, sequence_
 -- deprecated
 , fail
 -- partials
 , error, undefined
 , minimum, maximum
 , scanl1, scanr1
 , head, tail, last, init
 , foldr1
 , foldl1
 , foldl1
 , read
 , toEnum
 )

----------------------------------------
