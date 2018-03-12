{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE
    DeriveDataTypeable,
    DeriveGeneric,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveLift,
    DeriveAnyClass,
    LambdaCase,
    AutoDeriveTypeable
 #-}

{-| 

-}
module Spiros.Enable where

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities

----------------------------------------

{-|

Philosophically, 'Enablement' is "disabled by default": the 'Disabled' constructor is ordered first; the 'eliminator' takes the 'Disabled' continuation first.

It's Like @Either a a@, but with different instances (and different kind-arity). Relatedly, it's isomorphic to @(Bool, a)@. 

-}
data Enablement a
  = Disabled !a
  | Enabled  !a
  deriving
    (Generic
    ,Functor,Foldable,Traversable
    ,Show,Read,Eq,Ord,Data,Lift
    ,NFData,Hashable
    )

--TODO ; the 'Default' instance defaults to the underlying default (i.e. @'def' :: a@) being present but disabled ??

-- newtype Enablement a = Enable
--   { getEnablement :: Either a a
--   } deriving

----------------------------------------

{-| constructor.

-}
enabled :: a -> Enablement a
enabled = Enabled

{-| constructor.

-}
disabled :: a -> Enablement a
disabled = Disabled

----------------------------------------

{-|

idempotent.

-}
enable :: Enablement a -> Enablement a
enable = abled > Enabled

{-| 

idempotent.

-}
disable :: Enablement a -> Enablement a
disable = abled > Disabled

{-| 

involution.

-}
toggle :: Enablement a -> Enablement a
toggle = \case
  Enabled  a -> Disabled a
  Disabled a -> Enabled a

----------------------------------------

{-| eliminator.

@
enablement 
@

-}
enablement
  ::            (a -> r)
  ->            (a -> r)
  -> (Enablement a -> r) 
enablement kDisabled kEnabled = \case
  Enabled  a -> kEnabled  a
  Disabled a -> kDisabled a

{-| extract the value, forgetting whether it's enabled or disabled.

-}
abled :: Enablement a -> a
abled = \case
  Enabled  a -> a
  Disabled a -> a

----------------------------------------

{-| 'Left' is 'Disabled', 'Right' is 'Enabled'. 

-}
toEnablement :: Either a a -> Enablement a
toEnablement = either Disabled Enabled

{-| 'Disabled' is 'Left', 'Enabled' is 'Right'. 

-}
fromEnablement :: Enablement a -> Either a a 
fromEnablement = enablement Left Right

----------------------------------------


----------------------------------------


----------------------------------------

{-| 

-}
