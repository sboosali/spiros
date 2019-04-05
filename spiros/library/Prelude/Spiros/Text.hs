
--------------------------------------------------
--------------------------------------------------

{-| Re-Export @"Data.Text"@ and @"Data.ByteString"@, i.e. __strict__ text and __strict__ bytes.

All @Spiros.*@ modules (whenever possible) use this module's 'Text' type,
and its re-exported @Text.*@ functions.

-}

module Prelude.Spiros.Text

--------------------------------------------------

  ( module Prelude.Spiros.Text
  -- , module Data.Text
  -- , module Data.ByteString
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text      as TS
import qualified "text" Data.Text.Lazy as TL

--------------------------------------------------

import qualified "bytestring" Data.ByteString      as BS 
import qualified "bytestring" Data.ByteString.Lazy as BL 

--------------------------------------------------
-- Re-Exports ------------------------------------
--------------------------------------------------

import "text"       Data.Text

--------------------------------------------------

import "bytestring" Data.ByteString

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type StrictText = TS.Text
type LazyText   = TL.Text 

--------------------------------------------------

type StrictBytes = BS.ByteString
type LazyBytes   = BL.ByteString

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------