{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NamedFieldPuns        #-}

{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , DeriveGeneric
           , DeriveAnyClass
 #-}

--------------------------------------------------
--------------------------------------------------

{-| "Enrich" some standard functions.

In particular:

* wrap some over-broad types in @newtype@s;
* perform additional validation.

-}

module Prelude.Spiros.Enriched where

#include <sboo-base-feature-macros.h>

--------------------------------------------------
--------------------------------------------------

import qualified "text" Data.Text      as Strict
import qualified "text" Data.Text.Lazy as Lazy

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude.Spiros.Reexports

--------------------------------------------------
--------------------------------------------------

{-|

Intended Specializations:

* @'Replace' 'Strict.Text'@
* @'Replace' 'Lazy.Text'@

-}

data Replace a = Replace

  { old :: !a
  , new :: !a
  }

  deriving ( Show,Read,Eq,Ord,Generic --TODO CPP for Generic
#if HAS_EXTENSION_DeriveFunctor
           , Functor,Foldable,Traversable
#endif
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )
  
  -- deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| Ensure that 'Replace.old' is nonempty.

-}

checkReplace :: (IsString t, Eq t) => Replace t -> Maybe (Replace t)
checkReplace x@Replace{old} =
  if   old == ""
  then Nothing
  else Just x

--------------------------------------------------
--------------------------------------------------

{-|

=(Original Documention...)

/O(m+n)/ Replace every non-overlapping occurrence of @needle@ (a.k.a. @'Replace.old'@)
in @haystack@ with @replacement@ (a.k.a. @'Replace.new'@).

This function behaves as though it was defined as follows:

@
replace_StrictText Replace{old,new} haystack =
  'Strict.intercalate' new ('Strict.splitOn' needle haystack)
@

As this suggests, each occurrence is replaced exactly once.  So if
@needle@ occurs in @replacement@, that occurrence will /not/ itself
be replaced recursively:

>>> replace_StrictText Replace{ old = "oo", new = "foo" } "oo"
"foo"

In cases where several instances of @needle@ overlap, only the
first one will be replaced:

>>> replace_StrictText Replace{ old = "ofo", new = "bar" } "ofofo"
"barfo"

=(Additional Documention...)

this function has two differences from the function it wraps:

* the "enriched" argument record 'Replace', done for clarity of argument order.
* the behavior when @'Replace.new'@ (i.e. @needle@), is empty (i.e. @""@), which simply outputs the input unchanged, rather than erroring.

i.e.

>>> replace_StrictText Replace{ old = "", new = "anything" } "unchanged"
"unchanged"

You can use (the trivial) 'checkReplace'.

-}

replace_StrictText
  :: Replace Strict.Text
  -> Strict.Text
  -> Strict.Text

replace_StrictText Replace{old = ""} t = t
replace_StrictText Replace{old, new} t = Strict.replace old new t

--------------------------------------------------

{-| Alias for 'replace_StrictText'.

(See 'replace_StrictText' for documentation).

-}

replace
  :: Replace Strict.Text
  -> Strict.Text
  -> Strict.Text

replace = replace_StrictText

--------------------------------------------------

{-| Lazy analogue to 'replace_StrictText'.

(See 'replace_StrictText' for documentation).

-}

replace_LazyText
  :: Replace Lazy.Text
  -> Lazy.Text
  -> Lazy.Text

replace_LazyText Replace{old = ""} t = t
replace_LazyText Replace{old, new} t = Lazy.replace old new t

--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------