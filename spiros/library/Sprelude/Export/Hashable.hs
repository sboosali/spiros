--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Re-Export "Data.Hashable".

The @hashable@ package contains the @"Data.Hashable"@ module,
which provides the `Hashable` class.

== Links

* <https://www.stackage.org/haddock/lts/hashable/Data-Hashable.html>

-}

module Sprelude.Export.Hashable

  (

    -- * @module "Data.Hashable"@

    module EXPORT

  ) where

--------------------------------------------------
-- Includes --------------------------------------
--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "hashable" Data.Hashable as EXPORT ( Hashable(..) )
import "hashable" Data.Hashable as EXPORT ( hashUsing )

--------------------------------

#if HAS_HASHABLE_Hashable1
import "hashable" Data.Hashable.Lifted as EXPORT ( Hashable1(..) )
import "hashable" Data.Hashable.Lifted as EXPORT ( hashWithSalt1 )
#endif

--------------------------------

#if HAS_HASHABLE_Hashable2
import "hashable" Data.Hashable.Lifted as EXPORT ( Hashable2(..) )
import "hashable" Data.Hashable.Lifted as EXPORT ( hashWithSalt2 )
#endif

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "hashable" Data.Hashable

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------