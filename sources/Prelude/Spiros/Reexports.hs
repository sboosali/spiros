{-# LANGUAGE CPP #-}
#include <base-feature-macros.h>

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

#if !HAVE_MONAD_FAIL
{-# LANGUAGE ConstraintKinds #-}
#endif
  
{-|
Module      :  Prelude.Spiros.Reexports
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

(Re-)Exports:

* universally (or frequently) derived classes,
i.e. @deriving (...,'Data','Generic','NFData','Semigroup')@
* @safe-exceptions@'s @'throw'@, which generalizes @IO@ to 'MonadThrow'
* @safe@ (total) versions of partial functions, like 'readMay'. 
* 'assert'
* and many more (see the source)

Defines:

* single-character composition, i.e. ('>') an ('<')
* type names for common types ('LazyText' for lazy text, 'StrictBytes' for strict bytestrings, etc)

Hides:

* partial functions, e.g. @head@
* some aliased functions (like @sequence@, which is generalized into @sequenceA@).

Also see (these aren't dependencies, just influences):

* <http://www.stephendiehl.com/posts/protolude.html>
* <https://hackage.haskell.org/package/foundation-0.0.20/docs/Foundation.html>
* <https://github.com/quchen/articles/blob/master/haskell-cpp-compatibility.md>

-}
module Prelude.Spiros.Reexports
 ( module X -- re-eXports
 , module Base
#if !HAVE_MONAD_FAIL
 , module Prelude.Spiros.Reexports
#endif
 )
where

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

import "semigroups" Data.Semigroup.Generic           as X
 ( gmappend, gmempty
 )

---------------------------------------
-- https://www.fpcomplete.com/blog/2016/06/announce-safe-exceptions

import "safe-exceptions" Control.Exception.Safe      as X -- TODO mv so module, like Spiros.Exceptions?
--import "exceptions" Control.Monad.Catch           as X (MonadThrow(..))

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

import "deepseq" Control.DeepSeq                     as X
 ( NFData(..)
 , force
 )

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
import "containers" Data.Sequence                    as X (Seq)
import "containers" Data.IntSet                      as X (IntSet)
import "containers" Data.IntMap                      as X (IntMap)
import "containers" Data.Graph                       as X (Graph)
import "containers" Data.Tree                        as X (Tree)

----------------------------------------

import "template-haskell" Language.Haskell.TH.Syntax as X (Lift)

-- import "template-haskell" Language.Haskell.TH.Syntax as X

----------------------------------------

--import "base" Control.Exception                    as X (evaluate)

import "base" Data.Int                               as X
 ( Int
 , Int8, Int16, Int32, Int64
 )
   
import "base" Data.Word                              as X
 ( Word
 , Word8, Word16, Word32, Word64
 )

import "base" Data.Void                              as X
  ( Void
  , absurd
  )

import "base" Data.Char                              as X
    ( Char

    , isControl, isSpace
    , isLower, isUpper, isAlpha, isAlphaNum, isPrint
    , isDigit, isOctDigit, isHexDigit
    , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator

    , isAscii, isLatin1
    , isAsciiUpper, isAsciiLower

    , generalCategory

    , toUpper, toLower, toTitle

    , digitToInt
    , intToDigit

    , ord
    , chr
    )

import "base" Numeric.Natural                        as X (Natural)
import "base" Data.Ratio                             as X (Ratio)
  
import "base" Data.Maybe                             as X 
import "base" Data.Either                            as X 
import "base" Data.Function                          as X ((&),on,fix)

import "base" Text.Read                              as X
 ( readEither,readMaybe
 )

import "base" Data.Semigroup                         as X (Semigroup(..))
 -- TODO when were they merged into base from semi groups?

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
 ( MonadPlus(..)
 , void
 , forever
 , (>=>), (<=<)
 , join
 , guard, when, unless
 )

import "base" Control.Category                       as X
  (Category,(>>>),(<<<))

#ifdef HAVE_MONAD_FAIL
import "base" Control.Monad.Fail                     as X (MonadFail(..))
#endif

import "base" Control.Monad.Fix                      as X (MonadFix(..))
import "base" Control.Monad.IO.Class                 as X (MonadIO(..))

--import "base" Text.Printf                            as X (printf)

import "base" Data.Proxy                             as X (Proxy(..))
import "base" Data.Functor.Identity                  as X (Identity(..))   
import "base" Data.Coerce                            as X (coerce, Coercible)

import "base" GHC.Exts                               as X
  ( IsList(..)
  , IsString(..)
  , groupWith, sortWith
  )

import "base" GHC.Generics                           as X
 ( Generic
 , Generic1
 )
import "base" Data.Typeable                          as X
 ( Typeable
 , typeRep
 )
import "base" Data.Data                              as X (Data)

import "base" Control.Exception                      as X (assert)

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

--TODO
-- #if !HAVE_FOLDABLE_TRAVERSABLE_IN_PRELUDE
-- import Data.Foldable (Foldable (..))
-- import Prelude       hiding (foldr, foldr1)
-- #endif
--
-- #if !HAVE_MONOID_IN_PRELUDE
-- import Data.Monoid as X hiding ((<>))
-- #endif

----------------------------------------
-- the Prelude

import Data.List as Base hiding
  -- partials
 ( (!!)
 , find
 , minimumBy, maximumBy
 , scanl1, scanr1
 , head, tail, last, init
 -- aliased
 , map 
 )

import Prelude as Base hiding
 ( (<), (>)
 -- aliased
 , map, sequence, sequence_
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
-- backwards-compatibility

#if !HAVE_MONAD_FAIL
type MonadFail m = Monad m
#endif

----------------------------------------



----------------------------------------
