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

module Prelude.Spiros

 ( -- * Re-exports
   -- $reexports
   module X -- re-eXports

   -- * Usage
   -- $usage   

   -- * Non-exports
   -- $nonexports
 
   -- * Notes
   -- $notes
 ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Prelude.Spiros.Types                          as X
import Prelude.Spiros.Reexports                      as X
import Prelude.Spiros.Utilities                      as X
import Prelude.Spiros.System                         as X
import Prelude.Spiros.Exception                      as X
import Prelude.Spiros.Validator                      as X
import Prelude.Spiros.GUI                            as X
import Prelude.Spiros.Generics                       as X
import Prelude.Spiros.Enriched                       as X
import Prelude.Spiros.Parse                          as X
import Prelude.Spiros.Print                          as X

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

{- $usage

assertions:

@
'assert' :: Bool -> a -> a 

If the first argument evaluates to True, then the result is the second argument. Otherwise an AssertionFailed exception is raised, containing a String with the source file and line number of the call to assert.

Assertions can normally be turned on or off with a compiler flag (for GHC, assertions are normally on unless optimisation is turned on with -O or the -fignore-asserts option is given). When assertions are turned off, the first argument to assert is ignored, and the second argument is returned as the result.
@

-}

{- $reexports

These are re-exported by @Prelude.Spiros@.

    "Prelude.Spiros.Reexports" re-exports: the core types\/values from several packages; minus /all/ partial functions, /except/ for some functions whose names are prefixed with @"unsafe"@, i.e. "explicitly partial functions", e.g. 'unsafeNatural' (however, no @unsafeHead@ is exported, as its need often implies that the @[]@ being used is the wrong type).

    "Prelude.Spiros.Utilities" defines a few dozen simple utilities, like an extended prelude. 

    "Prelude.Spiros.System" provides system information: about the current operating system, architecture, and compiler. 

    "Prelude.Spiros.Exception" defines a few new exception types, which may (or may not) tag the message with a @TemplateHaskell@ 'Name' or with a 'CallStack', as auxiliary\/contextual information.

    "Prelude.Spiros.Parse" provides utilities for simple parsing of custom datatypes.

    "Prelude.Spiros.Print" provides utilities for simple pretty-printing of custom datatypes.

    "Prelude.Spiros.Validator" re-exports helpers for defining simple validators (e.g. @a -> Maybe b@). 

    "Prelude.Spiros.GUI" provides helpers for working with @TemplateHaskell@ 'Name's.

    "Prelude.Spiros.TemplateHaskell" provides a few helpers for using @doctest@ and working with @TemplateHaskell@.

-}

  
{- $nonexports

These must be explicitly imported, they aren't re-exported by @Prelude.Spiros@.

    "Prelude.Spiros.Classes" re-exports only typeclases\/methods (and a few helpers), from several packages (like @Prelude.Spiros.Reexports@), for deriving or defining instances (e.g. in a @.Types@ module). Unlike @Prelude.Spiros.Reexports@, partial functions that are methods (like @toEnum@ and @fromEnum@) are necessarily exported, since they must be /visible/ when manually writing instances. 

-}


{- $notes

Most examples (all those prefixed with a triple @(>>>)@) are @doctest@ed. Those with single @(>)@ may have brittle output, and codeblocks might describe relations by "returning" variables, and thus aren't.

Contributors to any /reverse dependency/ of @spiros@ may prefer "Prelude.NotSpiros".

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------