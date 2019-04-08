
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------
--------------------------------------------------

{-| Simple string-based printers.

NOTE “Printing” here means rendering \/ displaying \/ showing
(not @stdout@ or @putStrLn@, necessarily).

-}

module Prelude.Spiros.Print

  ( SimplePrint

  ) where

--------------------------------------------------
--------------------------------------------------

import Prelude.Spiros.Types
import Prelude.Spiros.Classes
import Prelude.Spiros.Reexports

import Prelude.Spiros.Utilities
import qualified Prelude.Spiros.GUI as GUI

-- import qualified Prelude.Spiros.Base as Base
-- import           Prelude.Spiros.Base (String)

--------------------------------------------------
--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..))

--------------------------------------------------

import qualified "containers" Data.Map as Map

--------------------------------------------------
------------------------------------------------

import           "base" Control.Exception (Exception(..))

import qualified "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Simple printer.

a Type Alias for showing strings:

@
show :: (Show a) => 'SimplePrint' a
@

Usage:

@
-- an example printer:

printVerbosity :: 'SimpleParse' Verbosity
printVerbosity = \case

  Concise -> \"concise\"
  Verbose -> \"verbose\"

-- for this type:

data Verbosity = Concise | Verbose
@

-}

type SimplePrint a =

  (a -> String)

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------