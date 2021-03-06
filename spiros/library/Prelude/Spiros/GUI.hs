--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds  #-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

--------------------------------------------------

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase     #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

--------------------------------------------------

{- | Globally-Unique Haskell-language Identifers.

== Types

* `GUI`  — a Haskell identifer, qualified by ① its namespace, ② its module, and ③ its package.
* `Name` — re-exported (from the @template-haskell@ package).

== Usage

Uses of `GUI` include:

* deeply-embedded domain-specific langauges.
* debugging.
* @-XTemplateHaskellQuotes@.

== Functions

* `prettyGUI`

-}

module Prelude.Spiros.GUI

  ( module Prelude.Spiros.GUI

  , Name
  , NameSpace(..)
  , OccName(..)
  , ModName(..)
  , PkgName(..)

  ) where

--------------------------------------------------
-- Includes --------------------------------------
--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities
import Prelude.Spiros.Classes

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text.Lazy.Builder as Lazy

--------------------------------------------------

import "template-haskell" Language.Haskell.TH.Syntax

  ( Name(..)
  , NameFlavour(NameG,NameL)
  , NameSpace(..)
  , OccName(..)
  , ModName(..)
  , PkgName(..)

  , mkName
  )

--------------------------------------------------

import "base" GHC.Generics (Generic)
import "base" Data.Data    (Data)

--------------------------------

import "base" Data.Typeable

  ( tyConModule
  , tyConName
  , tyConPackage
  , typeRepTyCon
  )

--------------------------------------------------

import Prelude hiding

  ( -- shadowed:
    (<), (>)
    -- deprecated:   
  , fail
  )

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | A globally unique haskell identifier,
for either a value or type,
fully-qualified with its module and package. 

-}

data GUI = GUI

  { _guiPackage    :: !PkgName
  , _guiModule     :: !ModName
  , _guiIdentifier :: !OccName
  , _guiNamespace  :: !NameSpace
  -- , _guiVersion    :: !(Maybe Version)
  }
  deriving ( Generic, Data
           , Eq, Ord
#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
           , Show
             -- « GHC 8 » introduced « instance Show NameSpace ».
#endif
           )

-- TODO new field: @Version@.

--------------------------------------------------

#if !MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)

showsPrec_NameSpace :: Int -> NameSpace -> ShowS
showsPrec_NameSpace _ = showString . go
  where

  go = \case
   VarName   -> "VarName"
   DataName  -> "DataName"
   TcClsName -> "TcClsName"

--------------------------------------------------

instance Show GUI where

  showsPrec precedence (GUI p m i n) =

    showParen (precedence >= 11) (foldr (.) id ss)

    where

    ss =
      [ (showString "GUI {")
      , (showString "_guiPackage = ")
      , (showsPrec 0 p)
      , (showString ", ")
      , (showString "_guiModule = ")
      , (showsPrec 0 m)
      , (showString ", ")
      , (showString "_guiIdentifier = ")
      , (showsPrec 0 i)
      , (showString ", ")
      , (showString "_guiNamespace = ")
      , (showsPrec_NameSpace 0 n)
      , (showString "}")
      ]

#endif
--------------------------------------------------

instance NFData GUI where
  rnf GUI{..}
        = rnfString (coerce _guiPackage)
    `seq` rnfString (coerce _guiModule)
    `seq` rnfString (coerce _guiIdentifier)
    `seq` rwhnf             _guiNamespace
    where
    rnfString :: String -> ()
    rnfString = rnf

--------------------------------------------------

instance Hashable GUI where

  -- hashWithSalt s GUI{..} = s `hashWithSalt` ...

  hashWithSalt s GUI{..} =

    s
    `hashStringWithSalt` (coerce _guiPackage)
    `hashStringWithSalt` (coerce _guiModule)
    `hashStringWithSalt` (coerce _guiIdentifier)
    `hashWithSalt`       (fromEnumNameSpace  _guiNamespace) -- lol
    -- `hashWithSalt`       (fromEnum _guiNamespace)

    where

    hashStringWithSalt :: Int -> String -> Int
    hashStringWithSalt = hashWithSalt

    fromEnumNameSpace :: NameSpace -> Int
    fromEnumNameSpace = \case
      VarName   -> 0
      DataName  -> 1
      TcClsName -> 2

--------------------------------------------------

-- {- | An identifer of a haskell /value/,
-- fully-qualified with its module and package,
-- and tagged with the type of the value.

-- @t@ is phantom.

-- -}
-- data GUI' t = GUI'
--  { _guiPackage'    :: !PkgName
--  , _guiModule'     :: !ModName
--  , _guiIdentifier' :: !OccName
--  -- , _guiVersion    :: !(Maybe Version)
--  } deriving (Show,Eq,Ord,Data,Generic)

-- -- instance NFData   (GUI' t) where
-- -- instance Hashable (GUI' t) where

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

{- | A “nameless” default 'Name'.

Useful as a placeholder for testing.

-}

anonymous :: Name
anonymous = mkName ""

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

displayName :: Name -> String
displayName name = go (fromGlobalName name)
  where

  go = maybe (show name) displayGUI

--------------------------------------------------

{-| Return a globally unique identifier from a Template Haskell 'Name', even if it's local (i.e. not global). 

== Implementation: @TemplateHaskellQuotes@ return only "local names" ('NameL') and "global names" ('NameG', which 'fromGlobalName' validates). 

* 'NameG'

See 'Name':

* 'NameS':
An unqualified name; dynamically bound

* 'NameQ' ModName:
A qualified name; dynamically bound

* 'NameU' !Int:
A unique local name

* 'NameL' !Int:
Local name bound outside of the TH AST

* 'NameG' NameSpace PkgName ModName:
Global name bound outside of the TH AST: An original name (occurrences only, not binders) Need the namespace too to be sure which thing we are naming

-}

unsafeGUI :: Name -> GUI
unsafeGUI name = go name
  where

  go :: Name -> GUI
  go = fromGlobalName > maybe (fakeGUI name) id
  
  fakeGUI :: Name -> GUI
  fakeGUI (Name nIdentifier nFlavor) = case nFlavor of
    NameL i -> localFakeGUI nIdentifier i
    _       -> defaultFakeGUI nIdentifier

  localFakeGUI :: (Integral i) => OccName -> i -> GUI
  localFakeGUI (OccName n) i =
    GUI fakePkgName fakeModName qualifiedLocalOccName defaultNameSpace
        where
        qualifiedLocalOccName = OccName $ n ++ "$" ++ show (toInteger i)

  defaultFakeGUI :: OccName -> GUI
  defaultFakeGUI realOccName
    = GUI fakePkgName fakeModName realOccName defaultNameSpace

  fakePkgName :: PkgName
  fakePkgName = PkgName "?"
  
  fakeModName :: ModName
  fakeModName = ModName "?"
  
  defaultNameSpace :: NameSpace
  defaultNameSpace = VarName

--------------------------------------------------

{-| if the given identifier is [1] global and [2] a value, then return it as a GUI. 

e.g.

@
> fromGlobalName 'fromGlobalName 
Just (PkgName "spiros-0.0.1-inplace",ModName "Prelude.Spiros.Exception",OccName "fromGlobalName")
@

Implementation Note: 'Name' use is compatible with @template-haskell >=2.11@.

-}

fromGlobalName :: Name -> Maybe GUI
fromGlobalName = \case

  Name nIdentifier (NameG nNameSpace nPackage nModule)
    -> Just $ GUI nPackage nModule nIdentifier nNameSpace

  _
    -> Nothing

--------------------------------------------------

{-| like 'fromGlobalName', but restricted to /identifiers/
(i.e. not types\/classes, not constructors\/patterns). 

e.g.

-}

fromValueName :: Name -> Maybe GUI
fromValueName = fromGlobalName >=> go

  where

  go gui = gui & \case

    GUI _ _ _ VarName -> Just gui
    _                 -> Nothing

--------------------------------------------------

{- | The globally unique identifier for a type: @(pkg,
<https://www.haskell.org/onlinereport/lexemes.html modid>,
<https://www.haskell.org/onlinereport/lexemes.html tycon>)@

>>> :set -XPolyKinds
>>> import Data.Proxy
>>> displayGUI $ fromTypeProxy (Proxy :: Proxy [])
"ghc-prim:GHC.Types.(type [])"

-}

fromTypeProxy
  :: forall a proxy.
     ( Typeable a
     )
  => proxy a
  -> GUI

fromTypeProxy
  = typeRep
  > typeRepTyCon
  > go

  where

  go t = GUI{..}

    where

    _guiPackage    = PkgName (tyConPackage t)
    _guiModule     = ModName (tyConModule  t)
    _guiIdentifier = OccName (tyConName    t)
    _guiNamespace  = TcClsName

-- {-| 
-- -}
-- failure :: Name -> Possibly a
-- failure = throwM . userError . showName

--------------------------------------------------
-- Functions: Printing ---------------------------
--------------------------------------------------

{- | Pretty-Pretty a `GUI`.

== Examples

>>> displayGUI (GUI (PkgName "package-name") (ModName "Module.SubModule") (OccName "identifierName") VarName)
"package-name:Module.SubModule.identifierName"
>>> displayGUI (GUI (PkgName "package-name") (ModName "Module.SubModule") (OccName "ConstructorName") DataName)
"package-name:Module.SubModule.ConstructorName"
>>> displayGUI (GUI (PkgName "package-name") (ModName "Module.SubModule") (OccName "TypeName") TcClsName)
"package-name:Module.SubModule.(type TypeName)"

-}

displayGUI :: GUI -> String
displayGUI (GUI (PkgName p) (ModName m) (OccName o) n) =

  p ++ ":" ++ m ++ "." ++ i

  where
  
  i = displayNamespacePrefix n & \case

    Nothing ->             o           -- un-paranthesized
    Just s  -> "(" ++ s ++ o ++ ")"    -- paranthesized, for ExplicitNamespace syntax

  displayNamespacePrefix :: NameSpace -> Maybe String
  displayNamespacePrefix = \case

      VarName   -> Nothing -- ""
      DataName  -> Nothing -- ""
      TcClsName -> Just "type "

--------------------------------------------------

{- | Pretty-Print a `GUI`.

Render as /lazily-built/ text, for the @formatting@ package.

== Examples

>>> :set -XTemplateHaskellQuotes
>>> Just gui1 = fromGlobalName 'buildGUI
>>> gui2 = gui1 { _guiPackage = PkgName "spiros" }
>>> buildGUI gui2
"spiros:Prelude.Spiros.GUI:buildGUI"

-}

buildGUI :: GUI -> Lazy.Builder
buildGUI GUI{ _guiPackage, _guiModule, _guiIdentifier, _guiNamespace } = tGUI

  where

  tGUI = tP <> tSep <> tM <> tSep <> tN <> tI

  tSep = Lazy.fromString ":"

  tP = Lazy.fromString sP
  tM = Lazy.fromString sM
  tI = Lazy.fromString sI

  tN = case _guiNamespace of

    VarName   -> mempty
    DataName  -> mempty
    TcClsName -> Lazy.fromString "(type)"        -- c.f. « -XExplicitNamespaces ».

  PkgName sP = _guiPackage
  ModName sM = _guiModule
  OccName sI = _guiIdentifier

{- TODO

import qualified "formatting" Formatting.Buildable as Format

-- | @≡ `buildGUI`@
instance Format.Buildable GUI where build = buildGUI

-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

{-NOTES

data Name
Name OccName NameFlavour   

the built-in syntax 'f and ''T can be used to construct names, The expression 'f gives a Name which refers to the value f currently in scope, and ''T gives a Name which refers to the type T currently in scope. These names can never be captured.

data NameFlavour
NameG NameSpace PkgName ModName
...

Global name bound outside of the TH AST: An original name (occurrences only, not binders) Need the namespace too to be sure which thing we are naming

data NameSpace
VarName
...

Variables


-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------