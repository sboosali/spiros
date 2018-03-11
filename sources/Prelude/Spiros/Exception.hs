-- {-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskellQuotes, RecordWildCards, PackageImports, LambdaCase, PatternSynonyms, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

see 'throwS', 'throwN', 'throwL', 'either2throw'. 

-}
module Prelude.Spiros.Exception where

import Prelude.Spiros.Utilities
import Prelude.Spiros.GUI

--

import "safe-exceptions" Control.Exception.Safe 


import "data-default-class" Data.Default.Class 
 (Default(..))

--

import "template-haskell" Language.Haskell.TH.Syntax -- (Name)

--

import "base" Control.Applicative
import "base" Data.Function
import "base" Data.Bifunctor (first)
import "base" GHC.Stack
import "base" GHC.Stack.Types (CallStack,HasCallStack)
import "base" GHC.Exts (IsString(..))
import "base" Control.Monad (MonadPlus(..))
import "base" Control.Monad.Fail (MonadFail(..))

--

--import qualified "base" Prelude
import           "base" Prelude hiding
 ( fail
 , (>), (<)
 )

----------------------------------------

{-| 

@
either2throw = 'either'
 'throwE'
 'return'
@

-}
either2throw
  :: ( MonadThrow m
     , Exception e
     )    
  => Either e a
  -> m a
either2throw = either
 throwE
 return

either2throw_
  :: ( MonadThrow m
     , Show e
     )    
  => Either e a
  -> m a
either2throw_
  = (first (show > someQuotedException 'either2throw_))
  > either2throw

----------------------------------------

maybe2throw_
  :: ( MonadThrow m
     )    
  => Maybe a
  -> m a
maybe2throw_ 
  = maybe2either (someQuotedException 'maybe2throw_ "")
  > either2throw

maybe2throw
  :: ( MonadThrow m
     , Exception e
     )    
  => e
  -> Maybe a
  -> m a
maybe2throw e
 = maybe2either e
 > either2throw

----------------------------------------

list2throw_
  :: ( MonadThrow m
     )    
  => List a
  -> m a
list2throw_ = list2throw
  (someQuotedException 'list2throw_ "")

list2throw
  :: ( MonadThrow m
     , Exception e
     )    
  => e
  -> List a
  -> m a
list2throw e
  = list2maybe
  > maybe2throw e

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
--
-- NOTE the prefixing apostrophe is a @TemplateHaskellQuotes@ name quote
-- (not a typo)
-- 
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
  & maybe "?" displayGUI
  where
  -- globalName = fromGlobalName name
  -- go (PkgName p, ModName m, OccName i) = concat [p,":",m,".",i]

----------------------------------------
-- MONAD THROW

{- | @E@ for 'Exception',

'throwM's a 'SimpleException'.

-}
throwE :: (MonadThrow m, Exception e) => e -> m a
throwE e = throwM e

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


Useful for easily defining smart constructors, whose error message has a fully-qualified name for debugging.
If you rename the module, the error message changes automatically;
and if you rename the identifier, you will get a compile time error from Template Haskell if you don't simultaneously update the useage of 'throwN'
(unless another name is captured).

e.g. validating naturals:

@
natural :: Integer -> 'Possibly' Natural
natural i
 | i >= 0    = return $ fromIntegral i
 | otherwise = throwN \'natural $ "must be non-negative"
@

-}
throwN
  :: (MonadThrow m)
  => Name -> String -> m a
throwN name s = throwM (QuotedException name s)

{-| @throwN_ name = 'throwN' name ""@

e.g. validating naturals:

@
natural :: Integer -> 'Possibly' Natural
natural i
 | i >= 0    = return $ fromIntegral i
 | otherwise = 'throwN_' \'natural
@

-}
throwN_
  :: (MonadThrow m)
  => Name -> m a
throwN_ name = throwN name ""

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
