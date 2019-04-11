--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

--------------------------------------------------

{- | Utilities for 'MonadThrow' (from the @exceptions@ package).

Throwers (Generalizers):

* 'throwEitherM'
* 'throwMaybeM'
* 'throwListM'

Throwers (Introspecting):

* 'throwN'

-}

module Prelude.Spiros.Exception where

--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

import Prelude.Spiros.Compatibility
import Prelude.Spiros.Utilities
import Prelude.Spiros.Reexports
import Prelude.Spiros.GUI

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..))

--import "exceptions" Control.Monad.Catch hiding (throwM)
--import "safe-exceptions" Control.Exception.Safe 

--------------------------------------------------

import qualified "safe" Safe

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

import "base" Control.Exception (Exception(..))

--------------------------------------------------

--import qualified "base" Prelude
import "base" Prelude hiding
 ( fail
 , (>), (<)
 )

--------------------------------------------------
-- Imports: CPP ----------------------------------
--------------------------------------------------

#if !HAS_PRELUDE_OPERATOR_Append
import "base" Data.Monoid ((<>))
#endif

--------------------------------------------------

-- #if HAS_GHC_HasCallStack
-- import           "base" GHC.Stack.Types (HasCallStack)
-- import           "base" GHC.Stack       (CallStack,callStack,prettyCallStack)--,getCallStack
-- #endif

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-|

Generalize 'Maybe' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
'throwMaybeM' ≡ 'maybe' ('throwM' e) 'return'
@

-}

throwMaybeM
  :: ( MonadThrow m
     , Exception e
     )
  => e
  -> (Maybe a -> m a)
 
throwMaybeM e = maybe (throwM e) return

{-# INLINEABLE throwMaybeM #-}

--------------------------------------------------

{-| 

Generalize '[]' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

-}

throwListM
  :: ( MonadThrow m
     , Exception e
     )
  => e
  -> ([a] -> m a)
 
throwListM e

  = list (throwM e) (\x _ -> return x)

{-# INLINEABLE throwListM #-}

--------------------------------------------------

{-|

Generalize @'Either' 'E.SomeException'@ (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
≡ 'either' 'throwM' 'return'
@

-}

throwEitherM
  :: ( MonadThrow m
     , Exception e
     )
  => (Either e a -> m a)

throwEitherM = either throwM return

{-# INLINEABLE throwEitherM #-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-|

Generalize 'Maybe' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
≡ 'maybe' ('throwM' _) 'return'
@

-}

throwMaybe
  :: ( MonadThrow m
     )
  => (Maybe a -> m a)

throwMaybe = throwMaybeM e
  where

  e =  Prelude.userError "<<< throwMaybeM Nothing >>>"

{-# INLINEABLE throwMaybe #-}

--------------------------------------------------

{-|

Generalize '[]' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
'throwListM' ≡ \\case
  []    -> 'throwM' _
  (x:_) -> 'return' x
@

Only return the first success (i.e. the head of the "list of successes").

-}

throwList
  :: ( MonadThrow m
     )
  => ([a] -> m a)

throwList = throwListM e
  where

  e =  Prelude.userError "<<< throwListM [] >>>"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | `throwM' a (@template-haskell@) 'Name'.

-}
 
throwN
  :: (MonadThrow m)
  => Name -> String
  -> m a

throwN name sMessage = throwM e
  where

  e = Prelude.userError s
  s = sName <> ": " <> sMessage

  sName = (displayGUI (unsafeGUI name))

{-# INLINEABLE throwN #-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------