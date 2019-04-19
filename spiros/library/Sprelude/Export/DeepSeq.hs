--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Re-Export "Control.DeepSeq", with some utilities and aliases.

== Usage

The "Control.Exception" and "Control.DeepSeq" modules provide for controlling the /strictness/ of evaluation.

Uses include:

* Forcing traces (see "Debug.Trace.trace").
* Forcing @error@s inside monadic computations, instead of those errors being thrown from pure code (see "Prelude.error").
* Removing space leaks — by popping a thunk once via `evaluateWHNF`, rather than repeatedly via laziness.

-}

module Sprelude.Export.DeepSeq

  ( -- * /Normal-Form/

    evaluateNF
  , evaluateNF_

  -- * /Weak-Head/ Normal-Form

  , evaluateWHNF
  , evaluateWHNF_

  -- * @module "Control.DeepSeq"@

  , module EXPORT

  ) where

--------------------------------------------------
-- Includes --------------------------------------
--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "base"    Prelude         as EXPORT (seq)

--------------------------------------------------

import "deepseq" Control.DeepSeq as EXPORT (NFData(..))
import "deepseq" Control.DeepSeq as EXPORT (deepseq, force, ($!!))

--------------------------------

#if HAS_DEEPSEQ_NFData1
import "deepseq" Control.DeepSeq as EXPORT (NFData1(..))
import "deepseq" Control.DeepSeq as EXPORT (rnf1)
#endif

--------------------------------

#if HAS_DEEPSEQ_NFData2
import "deepseq" Control.DeepSeq as EXPORT (NFData2(..))
import "deepseq" Control.DeepSeq as EXPORT (rnf2)
#endif

--------------------------------

#if MIN_VERSION_deepseq(1,4,3)
import "deepseq" Control.DeepSeq as EXPORT (rwhnf)  
#else
-- see below
#endif

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "deepseq" Control.DeepSeq

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "base" Control.Exception
import           "base" Control.Monad ( (<$!>) )

--------------------------------------------------

import Prelude.Spiros.Compatibility ( MonadIO, liftIO )

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | Evaluate an expression /strongly/, ignoring it.

== Definition

@
≡ `Control.DeepSeq.force`
@

== Naming

“/WHNF/” abbreviates “/Weak Head Normal-Form/” 

-}

evaluateNF :: (NFData a, MonadIO m) => a -> m a
evaluateNF = evaluateWHNF . Control.DeepSeq.force

--------------------------------------------------

{- | Evaluate an expression /strongly/, returning it.

== Definition

@
≡ `Control.DeepSeq.rnf`
@

== Naming

“/WHNF/” abbreviates “/Weak Head Normal-Form/”

-}

evaluateNF_ :: (NFData a, MonadIO m) => a -> m ()
evaluateNF_ = evaluateWHNF . Control.DeepSeq.rnf

--------------------------------------------------

{- | Evaluate an expression /weakly/, returning it.

== Definition

@
≡ `Control.Exception.evaluate`
@

== Naming

“/WHNF/” abbreviates “/Weak Head Normal-Form/”

-}

evaluateWHNF :: MonadIO m => a -> m a
evaluateWHNF = liftIO . Control.Exception.evaluate

--------------------------------------------------

{- | Evaluate an expression /weakly/, ignoring it.

== Naming

“/WHNF/” abbreviates “/Weak Head Normal-Form/” 

-}

evaluateWHNF_ :: MonadIO m => a -> m ()
evaluateWHNF_ what = (`seq` ()) <$!> evaluateWHNF what

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------