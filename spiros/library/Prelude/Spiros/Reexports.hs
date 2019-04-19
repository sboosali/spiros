--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------
  
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

--------------------------------------------------

module Prelude.Spiros.Reexports

 ( module EXPORT
 , module Base
 , module Prelude.Spiros.Compatibility

 ) where

--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

import Prelude.Spiros.Types
import Prelude.Spiros.Compatibility

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

import "generic-deriving" Generics.Deriving.Enum     as EXPORT

  ( GEnum(genum)
  , GIx
  )

--------------------------------------------------

import "safe" Safe                                   as EXPORT

--------------------------------------------------

import "exceptions" Control.Monad.Catch              as EXPORT

  ( MonadThrow(..)
  , MonadCatch(..)
  , MonadMask(..)
  )

--------------------------------------------------

import "data-default-class" Data.Default.Class       as EXPORT

  ( Default(..)
  )

--------------------------------------------------

import "semigroups" Data.Semigroup.Generic           as EXPORT

  ( gmappend
  , gmempty
  )

---------------------------------------
-- https://www.fpcomplete.com/blog/2016/06/announce-safe-exceptions

--import "safe-exceptions" Control.Exception.Safe      as EXPORT -- TODO mv so module, like Spiros.Exceptions?
--import "exceptions" Control.Monad.Catch           as EXPORT (MonadThrow(..))

--------------------------------------------------

-- import "unordered-containers" Data.HashSet        as EXPORT (HashSet)
-- import "unordered-containers" Data.HashMap.Strict as EXPORT (HashMap)

-- import "protolude" Protolude                      as EXPORT

import "string-conv" Data.String.Conv                as EXPORT

  ( StringConv (..)
  , Leniency (..)

  , toS
  , toSL
  , convS
  , convSL
  )

--------------------------------------------------
-- imports from the "standard library"...
-- (i.e. the libraries GHC bootstraps with, and thus are always available)
--------------------------------------------------

--------------------------------------------------
-- `deepseq`
--------------------------------------------------

import Sprelude.Export.DeepSeq                      as EXPORT

--------------------------------------------------
-- `hashable`
--------------------------------------------------

import "hashable" Data.Hashable                      as EXPORT ( Hashable(..) )
import "hashable" Data.Hashable                      as EXPORT ( hashUsing )

--------------------------------------------------
-- `text`
--------------------------------------------------

import "text" Data.Text                              as EXPORT (Text)        -- strict

--------------------------------------------------
-- `bytestring`
--------------------------------------------------

import "bytestring" Data.ByteString                  as EXPORT (ByteString)  -- strict

--------------------------------------------------
-- `transformers`
--------------------------------------------------

import "transformers" Control.Monad.Trans.Class      as EXPORT ( MonadTrans(..) )

--------------------------------------------------
-- `mtl`
--------------------------------------------------

--import "mtl" Control.Monad.Trans                   as EXPORT (MonadTrans(..))

import "mtl" Control.Monad.Reader                    as EXPORT
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

--------------------------------

import "mtl" Control.Monad.State                     as EXPORT
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

--------------------------------

import "mtl" Control.Monad.Except                    as EXPORT
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

-- import "mtl" Control.Monad.Writer.Strict          as EXPORT

--------------------------------------------------
-- `containers`
--------------------------------------------------

import "containers" Data.Set                         as EXPORT (Set)
import "containers" Data.Map                         as EXPORT (Map)
import "containers" Data.IntSet                      as EXPORT (IntSet)
import "containers" Data.IntMap                      as EXPORT (IntMap)

import "containers" Data.Graph                       as EXPORT (Graph)
import "containers" Data.Tree                        as EXPORT (Tree)
import "containers" Data.Sequence                    as EXPORT (Seq)

--------------------------------------------------
-- `template-haskell`
--------------------------------------------------

import "template-haskell" Language.Haskell.TH.Syntax as EXPORT (Lift)

-- import "template-haskell" Language.Haskell.TH.Syntax as EXPORT

--------------------------------------------------
-- `base`
--------------------------------------------------

import "base" Control.Exception                      as EXPORT

 ( Exception(..)
 , SomeException

 -- `Exception` instances:

 , AssertionFailed
 , ErrorCall
 , IOException
 , PatternMatchFail
 
 -- functions:

 , assert
 , throwTo
 , throwIO
 , ioError

 -- (also see the export list for « exceptions » package.)
 )

--------------------------------

import "base" System.Exit                            as EXPORT

  ( ExitCode(..)
  , exitWith
  )

--------------------------------

import "base" Debug.Trace                            as EXPORT

  ( traceIO
  , trace, traceShow
  , traceM, traceShowM
  )

--------------------------------

import "base" Data.Int                               as EXPORT

  ( Int
  , Int8, Int16, Int32, Int64
  )
   
--------------------------------

import "base" Data.Word                              as EXPORT

  ( Word
  , Word8, Word16, Word32, Word64
  )

--------------------------------

import "base" Data.Void                              as EXPORT

  ( Void
  , absurd
  )

--------------------------------

import "base" Data.Char                              as EXPORT

  ( Char

  , ord
  , chr

  , toUpper
  , toLower
  , toTitle

  , generalCategory
  , isControl
  , isSpace
  , isLower
  , isUpper
  , isAlpha
  , isAlphaNum
  , isPrint
  , isDigit
  , isOctDigit
  , isHexDigit
  , isLetter
  , isMark
  , isNumber
  , isPunctuation
  , isSymbol
  , isSeparator
  , isAscii
  , isLatin1
  , isAsciiUpper
  , isAsciiLower

  , digitToInt
  , intToDigit

  )

--------------------------------

import "base" Numeric.Natural                        as EXPORT (Natural)

--------------------------------

import "base" Data.Ratio                             as EXPORT (Ratio)

--------------------------------

import "base" Data.Maybe                             as EXPORT 

--------------------------------

import "base" Data.Either                            as EXPORT

--------------------------------

import "base" Data.Tuple                             as EXPORT ( fst, snd, swap )

--------------------------------

import "base" Data.Function                          as EXPORT ((&),on,fix)

--------------------------------

import "base" Text.Read                              as EXPORT

  ( readEither
  , readMaybe
  )

--------------------------------

import "base" Data.Ix                                as EXPORT

  ( Ix
  ) 

--------------------------------

import "base" Data.Bits                              as EXPORT

  ( Bits
  , FiniteBits
  )

--------------------------------

import "base" Data.Foldable                          as EXPORT

  ( traverse_
  , for_
  , sequenceA_
  , asum
  )

--------------------------------

import "base" Data.Traversable                       as EXPORT

  ( sequenceA
  )

--------------------------------

import "base" Control.Applicative                    as EXPORT  

--------------------------------

import "base" Control.Arrow                          as EXPORT

  ( (&&&)
  , (***)
  , (+++)
  , (|||)
  )

--------------------------------

import "base" Control.Monad                          as EXPORT
 ( MonadPlus(..)
 , void
 , forever
 , (>=>), (<=<)
 , join
 , guard, when, unless
 )

--------------------------------

import "base" Control.Category                       as EXPORT

  ( Category
  , (>>>), (<<<)
  )

--------------------------------

import "base" Control.Monad.Fix                      as EXPORT (MonadFix(..))

--------------------------------

--import "base" Text.Printf                            as EXPORT (printf)

--------------------------------

import "base" Data.Proxy                             as EXPORT (Proxy(..))

--------------------------------

import "base" Data.Functor.Identity                  as EXPORT (Identity(..))   

--------------------------------

import "base" Data.Coerce                            as EXPORT (coerce, Coercible)

--------------------------------

import "base" Data.Typeable                          as EXPORT

  ( Typeable
  , typeRep
  )

--------------------------------

import "base" Data.Data                              as EXPORT (Data)

--------------------------------------------------
-- Imports: CPP ----------------------------------
--------------------------------------------------

#if HAS_BASE_NonEmpty
import "base" Data.List.NonEmpty                     as EXPORT
 ( NonEmpty(..)
   -- unqualified smart constructor
 , nonEmpty
   -- safe versions
 , head, tail, last, init
 , some1, scanl1, scanr1, group1, groupBy1
 )
#endif

--------------------------------------------------

#if HAS_BASE_Bifunctor
import Data.Bifunctor as EXPORT 
#else
#endif

--------------------------------------------------

#if HAS_BASE_Bifoldable_Bitraversable
import Data.Bifoldable    as EXPORT
import Data.Bitraversable as EXPORT
#else
#endif

--------------------------------------------------

#if !HAS_PRELUDE_Monoid
import Data.Functor as EXPORT ((<$>))
import Data.Monoid  as EXPORT (Monoid(..))
#endif

--------------------------------------------------

#if HAS_BASE_Contravariant
import "base" Data.Functor.Contravariant              as EXPORT
 ( Contravariant(..)
 , Predicate(..)
 , Comparison(..)
 , Equivalence(..)
 , Op(..)
 , defaultComparison
 , defaultEquivalence
 )
#endif

--------------------------------------------------

#if MIN_VERSION_base(4,7,0) 
import "base" Control.Exception                       as EXPORT
 ( SomeAsyncException
 , AsyncException
 )
#endif

--------------------------------

-- #if MIN_VERSION_base(4,8,0)
-- import "base" Control.Exception                       as EXPORT
--  ( AllocationLimitExceeded
--  )
-- #endif

--------------------------------

-- #if MIN_VERSION_base(4,9,0)
-- import "base" Control.Exception                       as EXPORT
--  ( TypeError
--  )
-- #endif

--------------------------------

-- #if MIN_VERSION_base(4,10,0)
-- import "base" Control.Exception                       as EXPORT
--  ( CompactionFailed
--  )
-- #endif

--------------------------------------------------
-- `hashable` package...

import "hashable" Data.Hashable                      as EXPORT (Hashable(..))
import "hashable" Data.Hashable                      as EXPORT (hashUsing)

#if HAS_HASHABLE_Hashable1
import "hashable" Data.Hashable.Lifted               as EXPORT (Hashable1(..))
#endif

#if HAS_HASHABLE_Hashable2
import "hashable" Data.Hashable.Lifted               as EXPORT (Hashable2(..))
#endif

--------------------------------------------------
-- `ghc` compiler...

#if IS_COMPILER_ghc

import "base" GHC.Exts                               as EXPORT
  ( IsList(..)
  , IsString(..)
  , groupWith, sortWith
  )

import "base" GHC.Generics                           as EXPORT
 ( Generic
 , Generic1
 )

#endif

--------------------------------

#if HAS_GHC_CallStack
import "base" GHC.Stack.Types (HasCallStack)
#endif

--------------------------------------------------

--TODO #if !HAS_BASE_Foldable_TRAVERSABLE
-- import Data.Foldable (Foldable (..))
-- import Prelude       hiding (foldr, foldr1)
-- #endif
--
--TODO #if !HAS_PRELUDE_Monoid
-- import Data.Monoid as EXPORT hiding ((<>))
-- #endif

--------------------------------------------------
-- the Prelude

import Data.List as Base hiding

  ( -- partials:
    (!!)
  , find
  , minimumBy, maximumBy
  , scanl1, scanr1
  , head, tail, last, init

  -- aliased:
  , map 
  )

--------------------------------

import Prelude as Base hiding

  ( -- shadowed:

    (<)
  , (>)

  -- compatibility:

#if HAS_PRELUDE_OPERATOR_Append
  , (<>)
#endif

  -- aliased:

  , map
  , sequence
  , sequence_

  -- partials:

  , undefined
  , minimum, maximum
  , scanl1, scanr1
  , head, tail, last, init
  , foldr1
  , foldl1
  , foldl1
  , read
  , toEnum

  -- deprecated:

  , fail

  -- too short:

  , pi

  )

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------