{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, PackageImports, LambdaCase, RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass, AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RankNTypes, PolyKinds #-}

{-|

-}
module Prelude.Spiros.GUI where

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities
import Prelude.Spiros.Classes

import Prelude hiding
 ( (<), (>)
   -- shadowed
 , fail
   -- deprecated
 )

--

import "template-haskell" Language.Haskell.TH.Syntax
  ( Name(..)
  , NameFlavour(NameG)
  , NameSpace(..)
  , OccName(..)
  , ModName(..)
  , PkgName(..)
  )

--

import "base" GHC.Generics (Generic)
import "base" Data.Data    (Data)
import "base" Data.Typeable
 (Typeable, tyConModule, tyConName, tyConPackage, typeRep, typeRepTyCon)

----------------------------------------

{- | A globally unique haskell identifier,
for either a value or type,
fully-qualified with its module and package. 

TODO new field: @Version@.

-}
data GUI = GUI
 { _guiPackage    :: !PkgName
 , _guiModule     :: !ModName
 , _guiIdentifier :: !OccName
 , _guiNamespace  :: !NameSpace
 -- , _guiVersion    :: !(Maybe Version)
 } deriving (Show,Eq,Ord,Data,Generic)

instance NFData GUI where
  rnf GUI{..}
        = rnfString (coerce _guiPackage)
    `seq` rnfString (coerce _guiModule)
    `seq` rnfString (coerce _guiIdentifier)
    `seq` rwhnf             _guiNamespace
    where
    rnfString :: String -> ()
    rnfString = rnf

instance Hashable GUI where
  -- hashWithSalt s GUI{..} = s `hashWithSalt` ...
  hashWithSalt s GUI{..}
    = s
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
      VarName -> 0
      DataName -> 1
      TcClsName -> 2
    
----------------------------------------

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

----------------------------------------

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
 go t = GUI
   (PkgName $ tyConPackage t)
   (ModName $ tyConModule  t)
   (OccName $ tyConName    t)
   TcClsName

-- {-| 
-- -}
-- failure :: Name -> Possibly a
-- failure = throwM . userError . showName

----------------------------------------

{-|

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

----------------------------------------

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

