-- {-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskellQuotes, RecordWildCards, PackageImports, LambdaCase, PatternSynonyms, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prelude.Spiros.Exception where

--import Prelude.Spiros.Utilities

--

import "safe-exceptions" Control.Exception.Safe 


import "data-default-class" Data.Default.Class 
 (Default(..))

--

import "template-haskell" Language.Haskell.TH (Name)

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
 } deriving (Show)

-- | 'displayLocatedException', not @show@. 
instance Exception SimpleException where
  displayException = displaySimpleException

-- | 'SimpleException'
instance IsString SimpleException where
  fromString = SimpleException

-- | @'SimpleException' ""@
instance Default SimpleException where
  def = SimpleException ""

displaySimpleException :: SimpleException -> String
displaySimpleException SimpleException{..} =
  case _SimpleException_message of
    "" -> noMessage
    s  -> withMessage s

  where
  caller = "spiros:Prelude.Spiros.throwS_"

  noMessage = concat $
    [ "[", caller, "] was called."
    ]

  withMessage s = concat $ 
    [ "[", caller, "] was called with:\n" 
    , s
    ]

----------------------------------------
  
data QuotedException = QuotedException
 { _QuotedException_caller   :: !Name
 , _QuotedException_message  :: !String
 } deriving (Show)

-- | 'displayQuotedException', not @show@. 
instance Exception QuotedException where
  displayException = displayQuotedException

-- | @= QuotedException \''throwM'@. 
instance IsString QuotedException where
  fromString = QuotedException 'throwM

-- | @"" :: QuotedException@ (see the 'IsString' instance).
instance Default QuotedException where
  def = fromString ""

-- -- | @'QuotedException' 'callStack' _@
-- toQuotedException :: (HasCallStack) => Name -> String -> QuotedException
-- toQuotedException _QuotedException_caller _QuotedException_message =
--   QuotedException{..}
--   where
--   _QuotedException_stack = callStack

displayQuotedException :: QuotedException -> String
displayQuotedException QuotedException{..} =
  concat $
      [ "["
      , caller
      , "]"
      , " called with:\n\n"
      , _QuotedException_message
      ]

  where
  caller = _QuotedException_caller & show

----------------------------------------
  
data LocatedException = LocatedException
 { _LocatedException_stack    :: !CallStack
 , _LocatedException_message  :: !String
 } deriving (Show)

-- | 'displayLocatedException', not @show@. 
instance Exception LocatedException where
  displayException = displayLocatedException

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

displayLocatedException :: LocatedException -> String
displayLocatedException LocatedException{..} =
  concat $
      [ "["
      , caller
      , "]"
      , " called with:\n\n"
      , _LocatedException_message
      , "\n\n... and called from:\n\n"
      , prettyCallStack _LocatedException_stack 
      ]

  where
  caller = 'throwM & displayQualifiedName

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

displayQualifiedName :: Name -> String
displayQualifiedName = show

----------------------------------------
-- MONAD THROW

-- | @S@ for 'String',
--
-- 'throwM's a 'SimpleException'. 
throwS :: (MonadThrow m) => String -> m a
throwS s = throwM (SimpleException s)

-- | @N@ for 'Name',
--
-- 'throwM's a 'QuotedException' with the given caller and message. 
throwN
  :: (MonadThrow m)
  => Name -> String -> m a
throwN name s = throwM (QuotedException name s)

-- | @L@ for @Location@ or 'CallStack' (@caLLstack@, lol). 
--
-- 'throwM's a 'LocatedException' with the given call-stack and message. 
throwL
  :: (MonadThrow m, HasCallStack)
  => String -> m a
throwL s = throwM (toLocatedException s)

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
  s = displayQualifiedName n

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
