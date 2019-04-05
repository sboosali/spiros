{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , DeriveGeneric
           , DeriveAnyClass
           #-}

--------------------------------------------------
--------------------------------------------------

{-| Simple "lookup-based" parsers.

-}

module Prelude.Spiros.Parse

  ( SimpleParse
  , SimpleParseM

  , ParseError(..)
  , ParseErrorConfig(..)

  , mkBoundedEnumParser
  , mkShowParserWith
  , mkParserFromList
  , mkParserFromPrinterWith

  , displayParseError
  , displayParseErrorWith

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
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
--import qualified "base" Text.Show as Show

--------------------------------------------------

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

  { stringBeingParsed :: !String
  , thingToParseInto  :: !String
  }

  deriving ({-Show,-}Eq,Ord
           {-,Lift-},Generic
           ,NFData,Hashable
           )

--------------------------------------------------

instance Exception ParseError where

  -- | @'displayException' \@'ParseError' ≡ 'displayParseError'@

  displayException = displayParseErrorWith (def :: ParseErrorConfig)

--------------------------------------------------

{-|

>>> :set -XOverloadedStrings
>>> Prelude.putStrLn (Prelude.show ("unparseable" :: ParseError))
[ParseError] Can't parse <<< "unparseable" >>>.

-}

instance Show ParseError where

  -- | @show \@'ParseError' ≡ 'displayParseError'@

  showsPrec precedence x = showParen (precedence >= maximumPrecedence) (displayed ++)
    where

    displayed :: String
    displayed = displayParseErrorWith (def :: ParseErrorConfig) x

    maximumPrecedence :: Int
    maximumPrecedence = 11

--------------------------------------------------

-- | Inject into 'stringBeingParsed' ('thingToParseInto' stays empty).

instance IsString ParseError where

  fromString s = ParseError

    { stringBeingParsed = s
    , thingToParseInto  = ""
    } 

--------------------------------------------------
--------------------------------------------------

data ParseErrorConfig = ParseErrorConfig

  { useUnicodeCharacters :: !Bool
  , useANSIColorCodes    :: !Bool
  }

  deriving (Show,Eq,Ord
           {-,Lift-},Generic
           ,NFData,Hashable
           )

--------------------------------------------------

-- | all @False@ (for portability).

instance Default ParseErrorConfig where

  def = ParseErrorConfig

    { useUnicodeCharacters = False
    , useANSIColorCodes    = False
    }

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Create a simple parser for a type.

@
≡ 'mkShowParserWith' ('constructors' _)
@

== Examples (@doctest@ed)

>>> parseBool = mkBoundedEnumParser :: String -> Maybe Bool
>>> parseBool "True"
Just True
>>> parseBool "Abolish ICE"
Nothing

== Exceptions

throws 'ParseError'.

-}

mkBoundedEnumParser :: forall a. forall m. (MonadThrow m, BoundedEnum a, Show a, Typeable a) => SimpleParseM m a
mkBoundedEnumParser = mkShowParserWith (constructors proxy)
  where
  proxy = [] :: [a]

{-# INLINEABLE mkBoundedEnumParser #-}

--------------------------------------------------

{-| Create a simple parser from a list of ('Show'able) values.

== Examples (@doctest@ed)

>>> parseHaskellBool = mkShowParserWith [False, True] 
>>> parseHaskellBool "True"
True
>>> parseHaskellBool "true"
*** Exception: [ParseError] Can't parse <<< ghc-prim:GHC.Types.(type Bool) >>> from <<< "true" >>>.

== Exceptions

throws 'ParseError'.

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

== Examples (@doctest@ed)

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

== Exceptions

throws 'ParseError'.

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

== Examples (@doctest@ed)

>>> parseINIBool = mkParserFromList "INI Bool" [ False -: ["false","no","0"], True -: ["true","yes","1"] ] 
>>> parseINIBool "true"
True
>>> parseINIBool "2"
*** Exception: [ParseError] Can't parse <<< INI Bool >>> from <<< "2" >>>.

Strings should be distinct. Within a @[String]@, duplicates are ignored.
Across each @[(a, [String])]@, all but one are ignored.

== Exceptions

throws 'ParseError'.

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

{-|

@
'displayParseError' ≡ 'displayParseErrorWith' 'def'
@

-}

displayParseError :: ParseError -> String
displayParseError = displayParseErrorWith def

--------------------------------------------------

{-|

== Examples (@doctest@ed)

>>> :set -XOverloadedStrings
>>> Prelude.putStrLn (Control.Exception.displayException ("unparseable" :: ParseError))
[ParseError] Can't parse <<< "unparseable" >>>.

>>> Prelude.putStrLn (displayParseErrorWith def{ useUnicodeCharacters = True } ParseError{ stringBeingParsed = "2", thingToParseInto = "INI Bool" })
[ParseError] Can't parse « INI Bool » from « "2" ».

>>> Prelude.putStrLn (displayParseErrorWith def{ useUnicodeCharacters = False } ParseError{ stringBeingParsed = "2", thingToParseInto = "INI Bool" })
[ParseError] Can't parse <<< INI Bool >>> from <<< "2" >>>.

-}

displayParseErrorWith :: ParseErrorConfig -> ParseError -> String
displayParseErrorWith ParseErrorConfig{ useUnicodeCharacters {-, useANSIColorCodes-} } ParseError{ stringBeingParsed, thingToParseInto } = concats

    [ [ "[ParseError] ", "Can't parse " ]

    , (if   (Prelude.not (Prelude.null thingToParseInto))
       then [ bracketedString (thingToParseInto), " from " ]
       else [])

    , [ bracketedString (show stringBeingParsed), "." ]
    ]

    where

    concats = (Prelude.concat . Prelude.concat)

    bracketedString :: String -> String
    bracketedString s = concat

      [ (if useUnicodeCharacters then "«" else "<<<")
      , " "
      , s
      , " "
      , (if useUnicodeCharacters then "»" else ">>>")
      ]

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- __useUnicode :: Bool
-- __useUnicode = False

--------------------------------------------------
--------------------------------------------------