{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------
--------------------------------------------------
  
{-| Re-Export the @Prelude@ and other @"base"@ modules.

Module      :  Prelude.Spiros.Base
Stability   :  experimental

these `ghc` compiler versions correspond with these `base` package versions:

* @ghc-8.6.3@  — @base-4.12.0.0@
* @ghc-8.4.4@  — @base-4.11.1.0@
* @ghc-8.2.2@  — @base-4.10.1.0@
* @ghc-8.0.2@  — @base-4.9.1.0@
* @ghc-7.10.3@ — @base-4.8.2.0@

-}

--------------------------------------------------
--------------------------------------------------

module Base.GHC710

  ( module Prelude

  , module Control.Applicative
  , module Control.Arrow
  , module Control.Category
  , module Control.Concurrent
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Monad.Fail
  , module Control.Monad.IO.Class
  , module Control.Monad.ST

  , module Data.Bifunctor
  , module Data.Bits
  , module Data.Bool
  , module Data.Char
  , module Data.Coerce
  , module Data.Complex
  , module Data.Data
  , module Data.Dynamic
  , module Data.Either
  , module Data.Fixed
  , module Data.Foldable
  , module Data.Functor
  , module Data.Functor.Classes
  , module Data.Functor.Compose
  , module Data.Functor.Identity
  , module Data.Function
  , module Data.Int
  , module Data.IORef
  , module Data.Ix
  , module Data.List
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Ord
  , module Data.Proxy
  , module Data.Ratio
  , module Data.Semigroup
  , module Data.STRef
  , module Data.String
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Unique
  , module Data.Version
  , module Data.Void
  , module Data.Word

  , module Debug.Trace

  , module Foreign.Storable
  , module Foreign.Ptr
  , module Foreign.ForeignPtr
  , module Foreign.StablePtr

  , module GHC.Conc
  , module GHC.Exts
  , module GHC.Generics
  , module GHC.IO.Exception

  , module Numeric
  , module Numeric.Natural

  , module System.Environment
  , module System.Exit
  , module System.IO
  , module System.IO.Error
  , module System.IO.Unsafe
  , module System.Mem
  , module System.Mem.StableName
  , module System.Timeout

  , module Text.ParserCombinators.ReadP
  , module Text.ParserCombinators.ReadPrec
  , module Text.Printf
  , module Text.Read

  , module Unsafe.Coerce

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "base" Prelude

--import Prelude                  hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))

--------------------------------------------------

import Control.Applicative
import Control.Arrow
  ( Arrow(arr, (***), (&&&)), returnA, ArrowZero(zeroArrow), ArrowPlus((<+>)), ArrowChoice ((+++), (|||)), ArrowApply(app), ArrowMonad(..), ArrowLoop(loop)
  )

import Control.Category         ((>>>), (<<<))
import Control.Concurrent
import Control.Exception
import Control.Monad            hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Fix        hiding (fix)
import Control.Monad.Fail              (MonadFail)
import Control.Monad.IO.Class
import Control.Monad.ST

--------------------------------------------------

import Data.Bifunctor
import Data.Bits
import Data.Bool
import Data.Char
import Data.Coerce
import Data.Complex
import Data.Data
import Data.Dynamic
import Data.Either
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Function            hiding ((.), id)
import Data.Int
import Data.IORef
import Data.Ix
import Data.List                hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.List.NonEmpty              (NonEmpty(..))
import Data.Maybe
import Data.Monoid              hiding ((<>), First(..), Last(..))
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Semigroup
import Data.STRef
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Unique
import Data.Version
import Data.Void
import Data.Word

--------------------------------------------------

import Debug.Trace

--------------------------------------------------

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr

--------------------------------------------------

import GHC.Conc                 hiding (withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Exts                        (lazy, inline, sortWith, groupWith)
import GHC.Generics                    (Generic)
import GHC.IO.Exception

--------------------------------------------------

import Numeric
import Numeric.Natural

--------------------------------------------------

import System.Environment
import System.Exit
import System.IO                       (Handle, hClose)
import System.IO.Error
import System.IO.Unsafe
import System.Mem
import System.Mem.StableName
import System.Timeout

--------------------------------------------------

import Text.ParserCombinators.ReadP    (ReadP, ReadS, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
import Text.Printf                     (printf, hPrintf)
import Text.Read                       (Read(..), readMaybe, readEither)

--------------------------------------------------

import Unsafe.Coerce

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-



-}