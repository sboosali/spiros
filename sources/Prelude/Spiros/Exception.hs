-- {-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskellQuotes, RecordWildCards, PackageImports, LambdaCase, PatternSynonyms, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

see 'throwS', 'throwN', 'throwL'. 

-}
module Prelude.Spiros.Exception where

--import Prelude.Spiros.Utilities

--

import "safe-exceptions" Control.Exception.Safe 


import "data-default-class" Data.Default.Class 
 (Default(..))

--

import "template-haskell" Language.Haskell.TH.Syntax -- (Name)

--

import "base" Control.Applicative
import "base" Data.Function
import "base" GHC.Stack
import "base" GHC.Stack.Types (CallStack,HasCallStack)
import "base" GHC.Exts (IsString(..))
import "base" Control.Monad (MonadPlus(..))
import "base" Control.Monad.Fail (MonadFail(..))

--

--import qualified "base" Prelude
import           "base" Prelude hiding
 ( fail
 )

----------------------------------------

data SimpleException = SimpleException
 { _SimpleException_message :: !String
 }

instance Exception SimpleException 

{- | non-@Read@able, for @Exception@.

@= 'displaySimpleException'@

-}
instance Show SimpleException where
  show = displaySimpleException

-- -- 'displayLocatedException', not @show@. 
-- instance Exception SimpleException where
--   displayException = displaySimpleException

-- | 'SimpleException'
instance IsString SimpleException where
  fromString = SimpleException

-- | @'SimpleException' ""@
instance Default SimpleException where
  def = SimpleException ""

{-|

'formatCustomExceptionWithCaller' if the message is empty, 
'formatCustomExceptionWithMessage' otherwise. 

-}
displaySimpleException :: SimpleException -> String
displaySimpleException SimpleException{..} =
  case _SimpleException_message of
    "" -> noMessage
    s  -> withMessage s

  where
  noMessage = formatCustomExceptionWithCaller caller

  withMessage s = formatCustomExceptionWithMessage caller s

  -- "spiros:Prelude.Spiros.throwS"
  caller  = displayQualifiedVariable 'throwS
  -- caller_ = displayQualifiedVariable 'throwS_

----------------------------------------
  
data QuotedException = QuotedException
 { _QuotedException_caller   :: !Name
 , _QuotedException_message  :: !String
 }

instance Exception QuotedException

{- | non-@Read@able, for @Exception@.

@= 'displayQuotedException'@

-}
instance Show QuotedException where
  show = displayQuotedException

-- -- 'displayQuotedException', not @show@. 
-- instance Exception QuotedException where
--   displayException = displayQuotedException

-- | @= QuotedException \''throwM'@. 
instance IsString QuotedException where
  fromString = QuotedException 'throwM

-- | @"" :: QuotedException@ (see the 'IsString' instance).
instance Default QuotedException where
  def = fromString ""

-- | 'formatCustomExceptionWithMessage'
displayQuotedException :: QuotedException -> String
displayQuotedException QuotedException{..}
  = formatCustomExceptionWithMessage
      caller
      _QuotedException_message

  where
  caller = _QuotedException_caller & displayQualifiedVariable

----------------------------------------
  
data LocatedException = LocatedException
 { _LocatedException_stack    :: !CallStack
 , _LocatedException_message  :: !String
 }

instance Exception LocatedException

{- | non-@Read@able, for @Exception@.

@= 'displayLocatedException'@

-}
instance Show LocatedException where
  show = displayLocatedException

-- -- 'displayLocatedException', not @show@. 
-- instance Exception LocatedException where
--   displayException = displayLocatedException

-- | Requires 'HasCallStack' around wherever the string literal is (i.e. at the "call-site" of @fromString@). 
instance IsString LocatedException where
  fromString = LocatedException callStack 

-- | @"" :: LocatedException@ (see the 'IsString' instance).
instance Default LocatedException where
  def = fromString ""

-- | @'LocatedException' 'callStack' _@
toLocatedException :: (HasCallStack) => String -> LocatedException
toLocatedException _LocatedException_message =
  LocatedException{..}
  where
  _LocatedException_stack = callStack

-- | 'formatCustomExceptionWithCallStack'
displayLocatedException :: LocatedException -> String
displayLocatedException LocatedException{..}
  = formatCustomExceptionWithCallStack
      caller
      _LocatedException_message
      callstack
  where
  caller = 'throwM & displayQualifiedVariable
  callstack = prettyCallStack _LocatedException_stack 

{-

    • Illegal implicit parameter ‘?callStack::CallStack’
    • In the context: HasCallStack
      While checking an instance declaration
      In the instance declaration for ‘IsString LocatedException’
   |
75 | instance (HasCallStack) => IsString LocatedException where
   |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


-- | @'LocatedException' 'callStack' \''throwM' _@
instance (HasCallStack) => IsString LocatedException where
  fromString = LocatedException callStack 'throwM


  --   , map go (_LocatedException_location & getCallStack)
  --   ]
  -- where
  -- go (x, y) = concat
  --   [ "  "
  --   , x
  --   , " ("
  --   , prettySrcLoc y
  --   , ")\n"
  --   ]


-}

----------------------------------------

formatCustomExceptionWithCaller
  :: String -> String 
formatCustomExceptionWithCaller caller =
  concat $
      [ "\n\n"
      , "[", caller, "]", " was called."
      , "\n"
      ]

formatCustomExceptionWithMessage
  :: String -> String -> String
formatCustomExceptionWithMessage caller message =
  concat $
      [ "\n\n"
      , "[", caller, "]", " was called with:"
      , "\n\n"
      , message
      , "\n"
      ]

formatCustomExceptionWithCallStack
  :: String -> String -> String -> String
formatCustomExceptionWithCallStack caller message stack =
  concat $
      [ "\n\n"
      , "[", caller, "]", " was called with:"
      , "\n\n"
      , message
      , "\n\n"
      , "... and called from:"
      , "\n\n"
      , stack
      , "\n"
      ]

----------------------------------------

{-|

>>> :set -XTemplateHaskellQuotes
>>> displayQualifiedVariable 'length
"base:Data.Foldable.length"
>>> import qualified Prelude
>>> displayQualifiedVariable 'Prelude.length
"base:Data.Foldable.length"

@
let x = undefined in displayQualifiedVariable 'x == "?"
@

-}
displayQualifiedVariable :: Name -> String
displayQualifiedVariable name
  = fromGlobalName name
  & maybe "?" go
  where
  -- globalName = fromGlobalName name
  go (PkgName p, ModName m, OccName i) = concat [p,":",m,".",i]

{-|

'Name' use is compatible with @template-haskell >=2.11@. 

-}
fromGlobalName :: Name -> Maybe (PkgName, ModName, OccName)
fromGlobalName = \case
  Name nIdentifier (NameG VarName nPackage nModule)
    -> Just (nPackage, nModule, nIdentifier)
  _
    -> Nothing

{-

fromGlobalName (Name nIdentifier scope) =
  case scope of
    NameG VarName nPackage nModule ->
      Just (nPackage, nModule, nIdentifier)
    _ -> Nothing


fromGlobalName :: Name -> Maybe (PkgName, ModName, OccName)
fromGlobalName (Name nIdentifier scope) =
  case scope of
    NameG VarName nPackage nModule -> case namespace of
      VarName -> Just (nPackage, nModule, nIdentifier)
      _ -> Nothing
    _ -> Nothing
-}


{-

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

----------------------------------------
-- MONAD THROW

{- | @S@ for 'String',

'throwM's a 'SimpleException'.

e.g.

@
> 'throwS' "this is an example"
*** Exception: 

[spiros-0.0.1-inplace:Prelude.Spiros.Exception.throwS] was called with:

this is an example
@

e.g.

@
> throwS ""
*** Exception: 

[spiros-0.0.1-inplace:Prelude.Spiros.Exception.throwS] was called.
@

-}
throwS :: (MonadThrow m) => String -> m a
throwS s = throwM (SimpleException s)

{- | @N@ for 'Name',

'throwM's a 'QuotedException' with the given caller and message.

e.g.

@
> 'throwN' \'throwN "this is an example"
*** Exception: 

[spiros-0.0.1-inplace:Prelude.Spiros.Exception.throwN] was called with:

this is an example
@

-}
throwN
  :: (MonadThrow m)
  => Name -> String -> m a
throwN name s = throwM (QuotedException name s)

{- | @L@ for @Location@ or 'CallStack' (@caLLstack@, lol). 

'throwM's a 'LocatedException' with the given call-stack and message.

e.g.

@
> caller = 'throwL' "this is an example"

> caller
*** Exception: 

[safe-exceptions-0.1.6.0-HpnSY2upHz4DtQ1B03RoNw:Control.Exception.Safe.throwM] was called with:

this is an example

... and called from:

CallStack (from HasCallStack):
  toLocatedException, called at sources/Prelude/Spiros/Exception.hs:385:20 in spiros-0.0.1-inplace:Prelude.Spiros.Exception
  throwL, called at <interactive>:28:1 in interactive:Ghci1
@

-}
throwL
  :: (MonadThrow m, HasCallStack)
  => String -> m a
throwL s = throwM (toLocatedException s)

{-

> import GHC.Stack.Types (HasCallStack)

> caller = 'throwL' "this is an example" :: (MonadThrow m, HasCallStack) => m ()

-}

----------------------------------------

{- | @E@ for 'Exception', calls 'throwM'.

NOTE if [1] you don't like the naming convention of the convenience functions below, or [2] if you need custom exceptions that aren't just a message with some location information, then directly use some exception (like when using the @exceptions@ pacakge).

e.g.:

>>> import Control.Exception (ArithException(..))
>>> divideM x y = guardE DivideByZero (y /= (0::Double)) >> return (x / y)
>>> :t divideM
divideM :: MonadThrow m => Double -> Double -> m Double
>>> divideM 1 4
0.25
>>> divideM 1 0
*** Exception: divide by zero
>>> divideM 1 4 :: Maybe Double
Just 0.25
>>> divideM 1 0 :: Maybe Double
Nothing

-}
guardE
  :: ( MonadThrow m
     , Exception e
     )
  => e
  -> Bool -> m ()
guardE e = \case
  True  -> pure ()
  False -> throwM (toException e)

-- | @M@ for 'MonadThrow', like 'throwM'. 
--
-- 'MonadThrow' analogue of @base@'s @guard@.
--
-- @= 'guardE' 'uninformative'@
guardM
  :: (MonadThrow m)
  => Bool -> m ()
guardM = guardE uninformative

-- | @S@ for 'String', calls 'throwM'.   
guardS
  :: (MonadThrow m)
  => String
  -> Bool -> m ()
guardS s = guardE e
  where
  e = someSimpleException s

-- | @N@ for 'Name', calls 'throwM'.
guardN
  :: (MonadThrow m)
  => Name
  -> Bool -> m ()
guardN n = guardS s
  where
  s = displayQualifiedVariable n

-- | @L@ for @Location@ or 'CallStack' (@caLLstack@). 
--
guardL 
  :: (MonadThrow m, HasCallStack)
  => Bool -> m ()
guardL = \case
  True  -> pure ()
  False -> throwM someLocatedException_

-- | @F@ for 'MonadFail', calls 'fail'. 
guardF
  :: (MonadFail m)
  => String -> Bool -> m ()
guardF s = \case
  True  -> pure ()
  False -> fail s

-- | @P@ for 'MonadPlus', calls 'mzero'. 
guardP
  :: (MonadPlus m)
  => Bool -> m ()
guardP = \case
  True  -> pure ()
  False -> mzero

{-
-- | 'guardL' with no error message.


where:

@
> :t divide 
divide :: (MonadThrow m, Ord b, Fractional b) => b -> b -> m b
@

-}

----------------------------------------

-- | 'someSimpleException_'
uninformative :: SomeException
uninformative = someSimpleException_

-- | the 'def'ault 'SimpleException'. 
someSimpleException_ :: SomeException
someSimpleException_ = SomeException
 (def :: SimpleException)

-- | the 'def'ault 'QuotedException'. 
someQuotedException_ :: SomeException
someQuotedException_ = SomeException
 (def :: QuotedException)

-- | the 'def'ault 'LocatedException'. 
someLocatedException_ :: HasCallStack => SomeException
someLocatedException_ = SomeException
 (def :: LocatedException)

-- | 
someSimpleException
  :: String
  -> SomeException
someSimpleException s = SomeException $ 
 (SimpleException s)

-- | 
someQuotedException
  :: Name
  -> String
  -> SomeException
someQuotedException n s = SomeException $
 (QuotedException n s)

-- | 
someLocatedException
  :: HasCallStack
  => String
  -> SomeException
someLocatedException s = SomeException $
 (toLocatedException s)

----------------------------------------

-- -- | 
-- throwE :: (MonadThrow m, HasCallStack) => String -> m a
-- throwE = throwM 

{-
-- | @'guard' b@ is @'pure' ()@ if @b@ is 'True',
-- and 'empty' if @b@ is 'False'.
guard           :: (Alternative f) => Bool -> f ()
guard True      =  pure ()
guard False     =  empty

guardM :: (MonadThrow f) => SomeException -> Bool -> f ()
guardM _ True      =  pure ()
guardM e False     =  throwM e

guardM :: (MonadThrow f) => Bool -> f ()
guardM True  =  pure ()
guardM False =  throwM uninformativeException

uninformativeException :: SomeException
uninformativeException = ErrorCall ""

guardM_ :: (MonadThrow f, HasCallStack) => Bool -> f ()
guardM_ = \case
  True -> pure ()
  False -> throwM locatedException

locatedException :: HasCallStack => SomeException
locatedException = SomeException $ errorCallWithCallStackException "" ?callStack

-}

----------------------------------------
