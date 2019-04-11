--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PackageImports #-}

--------------------------------------------------
-- Options ---------------------------------------
--------------------------------------------------

{-# OPTIONS_HADDOCK not-home #-}

--------------------------------------------------

{- | 'Prelude.NotSpiros' is like 'Prelude.Spiros', but more idiomatic / conventional.

Contributors may prefer this module.

For example, it doesn't shadow the comparison operators (@('>')@ and @('<')@) with composition operators:

* 'Prelude.NotSpiros.>'
* 'Prelude.NotSpiros.<'
* 'Prelude.NotSpiros.map'

c.f.:

* 'Prelude.Spiros.>'
* 'Prelude.Spiros.<'
* 'Prelude.Spiros.map'

-}

module Prelude.NotSpiros

  ( module Prelude.Spiros
  , module Prelude
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import        Prelude.Spiros hiding ((>), (<), map)
import "base" Prelude               ((>), (<), map)

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------