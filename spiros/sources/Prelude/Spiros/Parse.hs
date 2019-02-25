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
-- Types -----------------------------------------
--------------------------------------------------

{-| Simple parser.

a Type Alias for parsing values from strings:

@
(readThrow) :: (Read a) => 'SimpleParse' a
@

Expansions.

@
                  'SimpleParse' a

≡

('MonadThrow' m) => 'ParseM' m a

≡

('MonadThrow' m) => (String -> m a)
@


Specializations.

Specializations include:

@
'SimpleParse' a  ≡  (String -> 'Maybe'                a)
'SimpleParse' a  ≡  (String ->                      [a])
'SimpleParse' a  ≡  (String -> 'Either' 'SomeException' a)
'SimpleParse' a  ≡  (String -> 'IO'                   a)
@

Usage:

@
-- an example printer:

parseVerbosity :: 'SimpleParse' Verbosity
parseVerbosity s = go s

  where
  go = \case
  
    \"concise\" -> return Concise
    \"verbose\" -> return Verbose
  
    \"Concise\" -> return Concise
    \"Verbose\" -> return Verbose
  
    \"default\" -> return def
  
    _         -> throwString s

-- for this type:

data Verbosity = Concise | Verbose

instance Default Verbosity where def = Concise
@

Also see 'SimpleParseM'.

-}

type SimpleParse a =

  (forall m. (MonadThrow m) => SimpleParseM m a)

---(forall m. (MonadThrow m) => String -> m a)

--------------------------------------------------

{-| Simple (monadic) parser.

Usage:

@
-- an example printer:

parseVerbosity :: ('MonadThrow' m) => 'SimpleParseM' m Verbosity
parseVerbosity s = go s

  where
  go = \case
  
    \"concise\" -> return Concise
    \"verbose\" -> return Verbose
  
    \"Concise\" -> return Concise
    \"Verbose\" -> return Verbose
  
    \"default\" -> return def
  
    _         -> throwString s

-- for this type:

data Verbosity = Concise | Verbose

instance Default Verbosity where def = Concise

-- which can be instantiated as:

parseVerbosity_Maybe :: 'SimpleParseM' 'Maybe' Verbosity
parseVerbosity_Maybe = parseVerbosity

parseVerbosity_Either :: 'SimpleParseM' 'Either' Verbosity
parseVerbosity_Either = parseVerbosity

parseVerbosity_List :: 'SimpleParseM' [] Verbosity
parseVerbosity_List = parseVerbosity

parseVerbosity_IO :: 'SimpleParseM' 'IO' Verbosity
parseVerbosity_IO = parseVerbosity
@

-}

type SimpleParseM m a =

  (String -> m a)

--------------------------------------------------
--------------------------------------------------

-- {-|

-- -}

-- newtype SimpleParserM (m :: * -> *) (a :: *) = SimpleParserM

--   { getSimpleParserM ::

--       (String -> m a)
--   }

--   deriving (Functor,Generic)

-- --TODO Cpp for DerivingStrategies
--   -- deriving newtype  (Functor,Foldable,Traversable)
--   -- deriving stock    (Generic)
--   -- deriving newtype  (NFData)

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
-- Definitions -----------------------------------
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

mkBoundedEnumParser :: forall a. forall m. (MonadThrow m, BoundedEnum a, Show a, Typeable a) => SimpleParseM m a
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
  -> SimpleParseM m a

mkShowParserWith values = mkParserFromList title aliases
  where

  aliases = (go <$> values)

  go x = (x, [show x])

  title = GUI.displayGUI (GUI.fromTypeProxy proxy)

  proxy = [] :: [a]

{-# INLINEABLE mkShowParserWith #-}

--------------------------------------------------

{-| Create a simple parser from a "printing" function.


>>> printINIBool = (fmap Data.Char.toLower . show)
>>> parseINIBool = mkParserFromPrinterWith "INI Bool" printINIBool [False,True]
>>> parseINIBool "true" :: Maybe Bool
Just True
>>> parseINIBool "2" :: Maybe Bool
Nothing

in @(mkParserFromPrinterWith _ p)@, the printing function @p@ should be injective
(otherwise, some values will be ignored).

e.g. for a type @XYZ@:

@
data XYZ = ...
  deriving (Show, Enum, Eq, Ord, ...)

allXYZs :: [XYZ]
allXYZs = 'constructors'

printXYZ :: XYZ -> String
printXYZ = show

parseXYZ :: ('MonadThrow' m) => String -> m XYZ
parseXYZ = 'mkParserFromPrinterWith' "XYZ" printXYZ allXYZs
@

-}

mkParserFromPrinterWith
  :: (MonadThrow m)
  => String -> (a -> String) -> [a]
  -> SimpleParseM m a

mkParserFromPrinterWith title printer values = mkParserFromList title aliases
  where

  aliases = (go <$> values)

  go x = (x, [printer x])

--aliases = zip (values) (printer <$> values)

{-# INLINEABLE mkParserFromPrinterWith #-}

--------------------------------------------------

{-| Create a simple parser from a list.

>>> parseINIBool = mkParserFromList "INI Bool" [ False -: ["false","no","0"], True -: ["true","yes","1"] ] 
>>> parseINIBool "true"
True
>>> parseINIBool "2"
*** ParseError: Can't parse « INI Bool » from « "2" ».

Strings should be distinct. Within a @[String]@, duplicates are ignored.
Across each @[(a, [String])]@, all but one are ignored.

== Implementation

Internally, builds a @Map@.

-}

mkParserFromList
  :: (MonadThrow m)
  => String -> [(a, [String])]
  -> SimpleParseM m a

mkParserFromList title aliases = lookupM
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

{-# INLINEABLE mkParserFromList #-}

--------------------------------------------------
--------------------------------------------------