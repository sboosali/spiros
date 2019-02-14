{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------
--------------------------------------------------

{-| Simple "lookup-based" parsers.

-}

module Prelude.Spiros.Parse where

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
--------------------------------------------------

data ParseError = ParseError

  { stringBeingParsed :: String
  , thingToParseInto  :: String
  }

  deriving (Show,Eq,Ord,Lift,Generic,NFData,Hashable)

--------------------------------------------------

instance Exception ParseError where

  displayException ParseError{ stringBeingParsed, thingToParseInto } = (Prelude.concat . Prelude.concat)
    [ [ "Can't parse " ]
    , (if (Prelude.not (Prelude.null thingToParseInto)) then [ "« ", thingToParseInto ," » from " ] else [])
    , [ "« ", (show stringBeingParsed), " »." ]
    ]

--------------------------------------------------

instance IsString ParseError where

  fromString s = ParseError
    { stringBeingParsed = s
    , thingToParseInto  = ""
    } 

--------------------------------------------------
--------------------------------------------------

{-| Create a simple parser for a type.

@
≡ 'mkShowParserWith' ('constructors' _)
@

>>> parseBool = mkBoundedEnumParser :: String -> Maybe Bool
>>> parseBool "True"
Just True
>>> parseBool "Abolish ICE"
Nothing

-}

mkBoundedEnumParser :: forall a. forall m. (MonadThrow m, BoundedEnum a, Show a, Typeable a) => String -> m a
mkBoundedEnumParser = mkShowParserWith (constructors proxy)
  where
  proxy = [] :: [a]

{-# INLINEABLE mkBoundedEnumParser #-}

--------------------------------------------------

{-| Create a simple parser from a list of ('Show'able) values.

>>> parseHaskellBool = mkShowParserWith [False, True] 
>>> parseHaskellBool "True"
True
>>> parseHaskellBool "true"
***

-}

mkShowParserWith
  :: forall a. forall m. (MonadThrow m, Show a, Typeable a)
  => [a]
  -> String -> m a

mkShowParserWith values = mkParserWith title (map go values)
  where

  go x = (x, [show x])

  title = GUI.displayGUI (GUI.fromTypeProxy proxy)

  proxy = [] :: [a]

{-# INLINEABLE mkShowParserWith #-}

--------------------------------------------------

{-| Create a simple parser from a list.

>>> parseINIBool = mkParserWith "INI Bool" [ False -: ["false","no","0"], True -: ["true","yes","1"] ] 
>>> parseINIBool "true"
True
>>> parseINIBool "2"
*** ParseError: Can't parse « INI Bool » from « "2" ».

Strings should be distinct. Within a @[String]@, duplicates are ignored.
Across each @[(a, [String])]@, all but one are ignored.

== Implementation

Internally, builds a @Map@.

-}

mkParserWith
  :: (MonadThrow m)
  => String -> [(a, [String])]
  -> String -> m a

mkParserWith title aliases = lookupM
  where

  lookupM s
    = (Map.lookup s table)
    & (maybe (throwM e) return)
    where
    e = ParseError
      { stringBeingParsed = s
      , thingToParseInto  = title
      }

  table = Map.fromList entries

  entries = aliases & concatMap mkEntry

  mkEntry (x, ts) = ts & fmap (\t -> (t, x))

{-# INLINEABLE mkParserWith #-}

--------------------------------------------------
--------------------------------------------------
