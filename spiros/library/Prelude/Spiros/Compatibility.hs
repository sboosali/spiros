--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------
  
{-|

Module      :  Prelude.Spiros.Compatibility
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Compatibility-Module, whose definitions (to be exported) and\/or imports (to be re-exported)
are guarded by conditional-compilation (via @CPP@ macros).

For:

* @MonadFail@
* @Semigroup@

-}

module Prelude.Spiros.Compatibility

 ( -- * @Semigroup@ Compatibility

  Semigroup
 , (<>)

  -- * @MonadFail@ Compatibility

 , MonadFail
 , Product(Pair)
 , MonadIO(liftIO)
 , fail

 ) where

--------------------------------------------------
-- Includes --------------------------------------
--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports: CPP ----------------------------------
--------------------------------------------------

#if HAS_MONAD_FAIL
import "base" Control.Monad.Fail (MonadFail(fail))
#else
import "base" Control.Monad      (Monad(fail))
#endif

--------------------------------------------------

#if HAS_BASE_Functors
import "base"         Data.Functor.Product
#else
import "transformers" Data.Functor.Product
#endif

--------------------------------------------------

#if HAS_BASE_MonadIO
import "base"         Control.Monad.IO.Class
#else
import "transformers" Control.Monad.IO.Class
#endif

--------------------------------------------------

#if HAS_BASE_Semigroup
import           "base"       Data.Semigroup ( Semigroup(..) )
#else
import           "semigroups" Data.Semigroup ( Semigroup )
import           "semigroups" Data.Semigroup ( (<>) )
#endif

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

--------------------------------------------------
-- backwards-compatibility:

#if !HAS_MONAD_FAIL
type MonadFail m = Monad m
#endif

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------