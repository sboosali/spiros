{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}

-- {-# LANGUAGE DeriveFoldable             #-}
-- {-# LANGUAGE DeriveTraversable          #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Prelude.Spiros.Pretty where

--------------------------------------------------
--------------------------------------------------

import Prelude.Spiros.Classes
import Prelude.Spiros.Reexports

--------------------------------------------------

import qualified        Prelude.Spiros.Enriched as T        (replace)
import qualified "text" Data.Text               as T hiding (replace)

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow)
 -- ( MonadThrow (throwM)
 -- , MonadCatch (catch)
 -- )

--------------------------------------------------

--import "base" Prelude

--------------------------------------------------
--------------------------------------------------

{-| Simple printer.

Usage:

Here is an example printer,

@
printVerbosity :: 'Parse' Verbosity
printVerbosity = \case

  Concise -> "concise"
  Verbose -> "verbose"
@

for this type

@
data Verbosity = Concise | Verbose
@

-}

type Print a =

  a -> String

--------------------------------------------------

{-| Simple parser.



Expansions.

@
                  'Parse' a

≡

('MonadThrow' m) => 'ParseM' m a

≡

('MonadThrow' m) => (String -> m a)
@



Specializations.

Specializations include:

@
'Parse' a  ≡  (String -> 'Maybe'                a)
'Parse' a  ≡  (String ->                      [a])
'Parse' a  ≡  (String -> 'Either' 'SomeException' a)
'Parse' a  ≡  (String -> 'IO'                   a)
@


Usage:

Here is an example parser,

@
parseVerbosity :: 'Parse' Verbosity
parseVerbosity s = go s

  where
  go = \case
  
    "concise" -> return Concise
    "verbose" -> return Verbose
  
    "Concise" -> return Concise
    "Verbose" -> return Verbose
  
    "default" -> return def
  
    _         -> throwString s

parseVerbosity_Maybe :: 'ParseM' 'Maybe' Verbosity
parseVerbosity_Maybe = parseVerbosity
@

given

@
data Verbosity = Concise | Verbose

instance Default Verbosity where def = Concise
@

Also see 'ParseM'.

-}

type Parse a =

  (forall m. (MonadThrow m) => ParseM m a)

---(forall m. (MonadThrow m) => String -> m a)

--------------------------------------------------

{-| Simple parser.

Usage:

Here is an example parser,

@
parseVerbosity :: ('MonadThrow' m) => 'Parse' m Verbosity
parseVerbosity s = go s

  where
  go = \case
  
    "concise" -> return Concise
    "verbose" -> return Verbose
  
    "Concise" -> return Concise
    "Verbose" -> return Verbose
  
    "default" -> return def
  
    _         -> throwString s

parseVerbosity_Maybe :: 'Parse' 'Maybe' Verbosity
parseVerbosity_Maybe = parseVerbosity
@

given

@
data Verbosity = Concise | Verbose

instance Default Verbosity where def = Concise
@

-}

type ParseM m a =

  (String -> m a)

--------------------------------------------------

{-|

-}

newtype SimpleParserM (m :: * -> *) (a :: *) = SimpleParserM

  { getSimpleParserM ::
      (String -> m a)
  }

  deriving (Functor,Generic)

--TODO Cpp for DerivingStrategies
  -- deriving newtype  (Functor,Foldable,Traversable)
  -- deriving stock    (Generic)
  -- deriving newtype  (NFData)

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data TokenStyle = TokenStyle

  { separator :: WordSeparator
  , casing    :: WordCasing
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

newtype WordSeparator = WordSeparator

  (Maybe Char)

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

{-|  

-}

data WordCasing = WordCasing -- TODO acronyms/abbreviations

  { firstWord  :: SubwordCasing
  , laterWords :: SubwordCasing
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

data SubwordCasing

  = LowerCased                  -- ^ e.g. @"lower"@
  | TitleCased                  -- ^ e.g. @"Title"@
  | UpperCased                  -- ^ e.g. @"UPPER"@

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data KnownTokenStyle

  = CamelCase                   -- ^ e.g. @"camelCase"@
  | ClassCase                   -- ^ e.g. @"ClassCase"@
  | ConstCase                   -- ^ e.g. @"CONST_CASE"@
  | PascalCase                  -- ^ e.g. @"Pascal_Case"@

  | UnderscoreCase              -- ^ e.g. @"underscore_case"@
  | HyphenCase                  -- ^ e.g. @"hyphen-case"@
  | SlashCase                   -- ^ e.g. @"slash/case"@
  | DotCase                     -- ^ e.g. @"dot.case"@

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

pattern SnakeCase :: KnownTokenStyle
pattern SnakeCase = UnderscoreCase

pattern KebabCase :: KnownTokenStyle
pattern KebabCase = HyphenCase

--------------------------------------------------

pattern BashCase    :: KnownTokenStyle
pattern BashCase    = ConstCase

pattern PythonCase  :: KnownTokenStyle
pattern PythonCase  = UnderscoreCase

pattern LispCase    :: KnownTokenStyle
pattern LispCase    = HyphenCase

pattern HaskellCase :: KnownTokenStyle
pattern HaskellCase = CamelCase

--------------------------------------------------

pattern ModuleCase  :: KnownTokenStyle
pattern ModuleCase  = ClassCase

pattern PackageCase :: KnownTokenStyle
pattern PackageCase = HyphenCase


-- pattern HaskellModuleCase  = ClassCase
-- pattern HaskellPackageCase = HyphenCase

--pattern ModuleCase = DotCase

--------------------------------------------------

pattern FilepathCase :: KnownTokenStyle
pattern FilepathCase = SlashCase

--------------------------------------------------
--------------------------------------------------

fromKnownTokenStyle :: KnownTokenStyle -> TokenStyle
fromKnownTokenStyle = \case

  UnderscoreCase              -> separatorTokenStyle '_'
  HyphenCase                  -> separatorTokenStyle '-'
  SlashCase                   -> separatorTokenStyle '/'
  DotCase                     -> separatorTokenStyle '.'

  CamelCase                   -> TokenStyle { separator = noSeparator
                                            , casing    = WordCasing { firstWord  = LowerCased
                                                                     , laterWords = TitleCased
                                                                     }
                                            }

  ClassCase                   -> TokenStyle { separator = noSeparator
                                            , casing    = uniformWordCasing TitleCased
                                            }

  ConstCase                   -> TokenStyle { separator = charSeparator '_'
                                            , casing    = uniformWordCasing UpperCased
                                            }

  PascalCase                  -> TokenStyle { separator = charSeparator '_'
                                            , casing    = uniformWordCasing TitleCased
                                            }

--------------------------------------------------

{-|

-}

separatorTokenStyle :: Char -> TokenStyle
separatorTokenStyle c = TokenStyle

  { separator = charSeparator c
  , casing    = uniformWordCasing LowerCased
  }

--------------------------------------------------

{-|

-}

uniformWordCasing :: SubwordCasing -> WordCasing
uniformWordCasing x = WordCasing

  { firstWord  = x
  , laterWords = x
  }

--------------------------------------------------

{-|

@
≡ 'Nothing'
@

-}

noSeparator :: WordSeparator
noSeparator = WordSeparator Nothing

-- (Nothing :: Maybe Char)

--------------------------------------------------

{-|

@
≡ 'Just'
@

-}

charSeparator :: Char -> WordSeparator
charSeparator c = coerce (Just c)

--------------------------------------------------
--------------------------------------------------
{- Old Code
--------------------------------------------------





{-|

-}

newtype Casing = Casing

  (Maybe Char)

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)








  = CamelCase
  | ClassCase
  | SnakeCase
  | LispCase
  | FileCase
  | NamespaceCase





{-| Simple parser.

'ParseM' specialized to « m ~ Maybe ».

-}

type Parse a =

  String -> Maybe a

--------------------------------------------------

{-| Simple parser.

-}

type ParseM a =

  (forall m. (MonadThrow m) => String -> m a)







-}
--------------------------------------------------
--------------------------------------------------
{- Notes

--------------------------------------------------

map :: (Char -> Char) -> Text -> Text

O(n) map f t is the Text obtained by applying f to each element of t.

Example:

>>> let message = pack "I am not angry. Not at all."
>>> T.map (\c -> if c == '.' then '!' else c) message
"I am not angry! Not at all!"

Subject to fusion. Performs replacement on invalid scalar values.

--------------------------------------------------

intercalate :: Text -> [Text] -> Text

O(n) The intercalate function takes a Text and a list of Texts and concatenates the list after interspersing the first argument between each element of the list.

Example:

>>> T.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
"WeNI!seekNI!theNI!HolyNI!Grail"

--------------------------------------------------

intersperse :: Char -> Text -> Text

O(n) The intersperse function takes a character and places it between the characters of a Text.

Example:

>>> T.intersperse '.' "SHIELD"
"S.H.I.E.L.D"

Subject to fusion. Performs replacement on invalid scalar values.

--------------------------------------------------

transpose :: [Text] -> [Text]

O(n) The transpose function transposes the rows and columns of its Text argument. Note that this function uses pack, unpack, and the list version of transpose, and is thus not very efficient.

Examples:

>>> transpose ["green","orange"]
["go","rr","ea","en","ng","e"]
>>> transpose ["blue","red"]
["br","le","ud","e"]

--------------------------------------------------

reverse :: Text -> Text

O(n) Reverse the characters of a string.

Example:

>>> T.reverse "desrever"
"reversed"

Subject to fusion.

--------------------------------------------------

replace

  :: Text	
     needle to search for. If this string is empty, an error will occur.
  
  -> Text	
     replacement to replace needle with.
  
  -> Text	
     haystack in which to search.
  
  -> Text	 

O(m+n) Replace every non-overlapping occurrence of needle in haystack with replacement.

This function behaves as though it was defined as follows:

replace needle replacement haystack =
  intercalate replacement (splitOn needle haystack)

As this suggests, each occurrence is replaced exactly once. So if needle occurs in replacement, that occurrence will not itself be replaced recursively:

>>> replace "oo" "foo" "oo"
"foo"

In cases where several instances of needle overlap, only the first one will be replaced:

>>> replace "ofo" "bar" "ofofo"
"barfo"

In (unlikely) bad cases, this function's time complexity degrades towards O(n*m).

--------------------------------------------------
Case conversion
--------------------------------------------------

When case converting Text values, do not use combinators like map toUpper to case convert each character of a string individually, as this gives incorrect results according to the rules of some writing systems. The whole-string case conversion functions from this module, such as toUpper, obey the correct case conversion rules. As a result, these functions may map one input character to two or three output characters. For examples, see the documentation of each function.

Note: In some languages, case conversion is a locale- and context-dependent operation. The case conversion functions in this module are not locale sensitive. Programs that require locale sensitivity should use appropriate versions of the case mapping functions from the text-icu package.

--------------------------------------------------

toLower :: Text -> Text

O(n) Convert a string to lower case, using simple case conversion. Subject to fusion.

The result string may be longer than the input string. For instance, "İ" (Latin capital letter I with dot above, U+0130) maps to the sequence "i" (Latin small letter i, U+0069) followed by " ̇" (combining dot above, U+0307).

--------------------------------------------------

toUpper :: Text -> Text

O(n) Convert a string to upper case, using simple case conversion. Subject to fusion.

The result string may be longer than the input string. For instance, the German "ß" (eszett, U+00DF) maps to the two-letter sequence "SS".

--------------------------------------------------

toTitle :: Text -> Text

O(n) Convert a string to title case, using simple case conversion. Subject to fusion.

The first letter of the input is converted to title case, as is every subsequent letter that immediately follows a non-letter. Every letter that immediately follows another letter is converted to lower case.

The result string may be longer than the input string. For example, the Latin small ligature ﬂ (U+FB02) is converted to the sequence Latin capital letter F (U+0046) followed by Latin small letter l (U+006C).

Note: this function does not take language or culture specific rules into account. For instance, in English, different style guides disagree on whether the book name "The Hill of the Red Fox" is correctly title cased—but this function will capitalize every word.

--------------------------------------------------

-}
--------------------------------------------------