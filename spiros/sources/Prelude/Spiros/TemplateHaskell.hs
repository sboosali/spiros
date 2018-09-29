-- {-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

{- |

re-exports 'GHC80LanguageExtension', an aliased 'GHC.Extension':

@
data Extension

   = Cpp
   | OverlappingInstances
   | UndecidableInstances
   | IncoherentInstances
   | UndecidableSuperClasses
   | MonomorphismRestriction
   | MonoPatBinds
   | MonoLocalBinds
   | RelaxedPolyRec           -- Deprecated
   | ExtendedDefaultRules     -- Use GHC's extended rules for defaulting
   | ForeignFunctionInterface
   | UnliftedFFITypes
   | InterruptibleFFI
   | CApiFFI
   | GHCForeignImportPrim
   | JavaScriptFFI
   | ParallelArrays           -- Syntactic support for parallel arrays
   | Arrows                   -- Arrow-notation syntax
   | TemplateHaskell
   | TemplateHaskellQuotes    -- subset of TH supported by stage1, no splice
   | QuasiQuotes
   | ImplicitParams
   | ImplicitPrelude
   | ScopedTypeVariables
   | AllowAmbiguousTypes
   | UnboxedTuples
   | UnboxedSums
   | BangPatterns
   | TypeFamilies
   | TypeFamilyDependencies
   | TypeInType
   | OverloadedStrings
   | OverloadedLists
   | NumDecimals
   | DisambiguateRecordFields
   | RecordWildCards
   | RecordPuns
   | ViewPatterns
   | GADTs
   | GADTSyntax
   | NPlusKPatterns
   | DoAndIfThenElse
   | RebindableSyntax
   | ConstraintKinds
   | PolyKinds                -- Kind polymorphism
   | DataKinds                -- Datatype promotion
   | InstanceSigs
   | ApplicativeDo

   | StandaloneDeriving
   | DeriveDataTypeable
   | AutoDeriveTypeable       -- Automatic derivation of Typeable
   | DeriveFunctor
   | DeriveTraversable
   | DeriveFoldable
   | DeriveGeneric            -- Allow deriving Generic/1
   | DefaultSignatures        -- Allow extra signatures for defmeths
   | DeriveAnyClass           -- Allow deriving any class
   | DeriveLift               -- Allow deriving Lift
   | DerivingStrategies

   | TypeSynonymInstances
   | FlexibleContexts
   | FlexibleInstances
   | ConstrainedClassMethods
   | MultiParamTypeClasses
   | NullaryTypeClasses
   | FunctionalDependencies
   | UnicodeSyntax
   | ExistentialQuantification
   | MagicHash
   | EmptyDataDecls
   | KindSignatures
   | RoleAnnotations
   | ParallelListComp
   | TransformListComp
   | MonadComprehensions
   | GeneralizedNewtypeDeriving
   | RecursiveDo
   | PostfixOperators
   | TupleSections
   | PatternGuards
   | LiberalTypeSynonyms
   | RankNTypes
   | ImpredicativeTypes
   | TypeOperators
   | ExplicitNamespaces
   | PackageImports
   | ExplicitForAll
   | AlternativeLayoutRule
   | AlternativeLayoutRuleTransitional
   | DatatypeContexts
   | NondecreasingIndentation
   | RelaxedLayout
   | TraditionalRecordSyntax
   | LambdaCase
   | MultiWayIf
   | BinaryLiterals
   | NegativeLiterals
   | DuplicateRecordFields
   | OverloadedLabels
   | EmptyCase
   | PatternSynonyms
   | PartialTypeSignatures
   | NamedWildCards
   | StaticPointers
   | TypeApplications
   | Strict
   | StrictData
   | MonadFailDesugaring
   deriving (Enum,...)
@

-}
module Prelude.Spiros.TemplateHaskell where

import Prelude.Spiros.Types

----------------------------------------

import qualified "template-haskell" Language.Haskell.TH.LanguageExtensions as GHC

import "base" Control.Arrow ((>>>))
import "base" Prelude

----------------------------------------

{-|

The aliased type are language extensions known to be supported by the GHC version associated with the current version of the @template-haskell@ pacakge. This type implies, from its name, that only @GHC 8.0@ extensions should be used, for compatibility.

-}
type GHC80LanguageExtension = GHC.Extension

----------------------------------------

{-| 

@
= fmap 'extension2flag'
@

-}
extensions2flags :: [Enablement GHC80LanguageExtension] -> [String]
extensions2flags = fmap extension2flag

{-| 

@
@

-}
disabledExtensionFlags :: [GHC80LanguageExtension] -> [String]
disabledExtensionFlags = fmap disabled >>> extensions2flags

{-| 

@
@

-}
enabledExtensionFlags :: [GHC80LanguageExtension] -> [String]
enabledExtensionFlags = fmap enabled >>> extensions2flags

{-| trivial helper for writing the @main = doctest [...]@ boilerplate with fewer typos.

@
either "-X..." or "-XNo..." 
@

>>> import Language.Haskell.TH.LanguageExtensions
>>> import Spiros.Enable
>>> extension2flag (enabled LambdaCase)
"-XLambdaCase"
>>> extension2flag (disabled ImplicitPrelude)
"-XNoImplicitPrelude"

-}
extension2flag :: Enablement GHC80LanguageExtension -> String
extension2flag = fmap show >>> enablement ("-XNo"++) ("-X"++)

----------------------------------------

{-| @default-extensions: ...@ 

The language extensions included among the @default-extensions@ of many of my pacakges (i.e. the @default-extensions@ field of the library stanza in the @.cabal@ file). 

Most are benign.

@
"NoImplicitPrelude PackageImports AutoDeriveTypeable DeriveDataTypeable DeriveGeneric DeriveFunctor DeriveFoldable DeriveTraversable LambdaCase EmptyCase TypeOperators PostfixOperators ViewPatterns BangPatterns KindSignatures NamedFieldPuns RecordWildCards TupleSections MultiWayIf DoAndIfThenElse EmptyDataDecls InstanceSigs MultiParamTypeClasses FlexibleContexts FlexibleInstances TypeFamilies FunctionalDependencies ScopedTypeVariables StandaloneDeriving"
@

-}
myDefaultLanguageExtensions:: [Enablement GHC80LanguageExtension]
myDefaultLanguageExtensions =
  [ disabled GHC.ImplicitPrelude
  
  , enabled GHC.PackageImports
  , enabled GHC.AutoDeriveTypeable
  , enabled GHC.DeriveDataTypeable
  , enabled GHC.DeriveGeneric
  , enabled GHC.DeriveFunctor
  , enabled GHC.DeriveFoldable
  , enabled GHC.DeriveTraversable
  , enabled GHC.LambdaCase
  , enabled GHC.EmptyCase
  , enabled GHC.TypeOperators
  , enabled GHC.PostfixOperators
  , enabled GHC.ViewPatterns
  , enabled GHC.BangPatterns
  , enabled GHC.KindSignatures
  , enabled GHC.RecordPuns
  , enabled GHC.RecordWildCards
  , enabled GHC.TupleSections
  , enabled GHC.MultiWayIf
  , enabled GHC.DoAndIfThenElse
  , enabled GHC.EmptyDataDecls
  , enabled GHC.InstanceSigs
  , enabled GHC.MultiParamTypeClasses
  , enabled GHC.FlexibleContexts
  , enabled GHC.FlexibleInstances
  , enabled GHC.TypeFamilies
  , enabled GHC.FunctionalDependencies
  , enabled GHC.ScopedTypeVariables
  , enabled GHC.StandaloneDeriving
  ]

{-| @enabled other-extensions: ...@ 

The language extensions included among the @other-extensions@ of many of my pacakges (i.e. the @other-extensions@ field of the library stanza in the @.cabal@ file). 

These have more drawbacks than those in 'myDefaultLanguageExtensions': worse type inference, increased build times, more strained portability, etc. 

-}
myOtherLanguageExtensions:: [Enablement GHC80LanguageExtension]
myOtherLanguageExtensions =
  [ enabled GHC.Cpp

  , enabled GHC.TemplateHaskell
  , enabled GHC.TemplateHaskellQuotes
  
  , enabled GHC.OverloadedStrings
  , enabled GHC.OverloadedLists
  
  , enabled GHC.ForeignFunctionInterface

  , enabled GHC.RankNTypes

  , enabled GHC.GADTs

  -- , enabled GHC.DerivingStrategies
  , enabled GHC.DeriveAnyClass
  , enabled GHC.GeneralizedNewtypeDeriving
  
  ]

----------------------------------------