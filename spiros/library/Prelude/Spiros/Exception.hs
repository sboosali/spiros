{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE TemplateHaskell #-} -- TODO rm this, create two modules Prelude.Spiros.Exception.Quoted.{GHC7,GHC8}. why? to use only TemplateHaskellQuotes.

--------------------------------------------------

{- | Utilities for 'MonadThrow' (from the @exceptions@ package).

Throwers (Generalizers):

* 'throwEitherM'
* 'throwMaybeM'
* 'throwListM'

Throwers (custom @Exception@s):

* 'throwS'
* 'throwN'
* 'throwL'

Custom @Exception@s types:

* 'SimpleException'
* 'QuotedException'
* 'LocatedException' 

-}

module Prelude.Spiros.Exception where

--------------------------------------------------
--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

import Prelude.Spiros.Types
import Prelude.Spiros.Compatibility

import Prelude.Spiros.Utilities
import Prelude.Spiros.GUI
import Prelude.Spiros.Reexports

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

import qualified "base" Control.Exception as E

--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..))

--import "exceptions" Control.Monad.Catch hiding (throwM)
--import "safe-exceptions" Control.Exception.Safe 

--------------------------------------------------

import "data-default-class" Data.Default.Class 
 (Default(..))

--------------------------------------------------

--import qualified "containers" Data.Sequence as Seq
import           "containers" Data.Sequence (Seq)

--------------------------------------------------

import "template-haskell" Language.Haskell.TH.Syntax -- (Name)

--------------------------------------------------

import qualified "safe" Safe

--------------------------------------------------

--import qualified "base" GHC.Stack.Types as GHC

--------------------------------------------------

--import "base" Control.Applicative
--import "base" Data.Function
--import "base" Data.List.NonEmpty (NonEmpty(..))
import "base" Data.Bifunctor (first)
import "base" GHC.Exts (IsString(..))
import "base" Control.Monad (MonadPlus(..))

--------------------------------------------------

--import qualified "base" Prelude
import           "base" Prelude hiding
 ( fail
 , (>), (<)
 )

--------------------------------------------------
-- Imports: CPP ----------------------------------
--------------------------------------------------

#if !HAS_PRELUDE_OPERATOR_Append
import "base" Data.Monoid ((<>))
#endif

--------------------------------------------------

#if HAS_GHC_HasCallStack
import           "base" GHC.Stack.Types (HasCallStack)
import           "base" GHC.Stack       (CallStack,callStack,prettyCallStack)--,getCallStack
#endif

--------------------------------------------------
--------------------------------------------------

{-
  
{-|
-}
type CallStack' = [(String, SrcLoc)]
 
callStack' :: HasCallStack => CallStack'
callStack' = getCallStack callStack

getCallStack' :: CallStack -> CallStack'
getCallStack' = getCallStack

-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| A default 'E.Exception', useful when manipulating 'MonadThrow' instances.

An 'ErrorCall' (whose message is uninformative).

-}

someMonadThrowException
  :: (Show a)
  => a -> E.SomeException
someMonadThrowException x = E.toException exception
  where
  exception = E.ErrorCall message
  message   = "[MonadThrow] " <> s
  s         = show x

--------------------------------------------------

{-|

Generalize 'Maybe' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
≡ 'maybe' ('throwM' _) 'return'
@

-}

throwMaybeM
  :: (MonadThrow m)
  => Maybe a -> m a

throwMaybeM = throwMaybeWithM e
  where
  e = someMonadThrowException (Nothing :: Maybe ())

--------------------------------------------------

{-|

Generalize 'Maybe' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
'throwMaybeWithM' ≡ 'maybe' ('throwM' e) 'return'
@

-}

throwMaybeWithM
  :: (MonadThrow m)
  => E.SomeException
  -> Maybe a -> m a
 
throwMaybeWithM e = maybe (throwM e) return

--------------------------------------------------

{-|

Generalize '[]' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
'throwListM' ≡ \\case
  []    -> 'throwM' _
  (x:_) -> 'return' x
@

Only return the first success (i.e. the head of the "list of successes").

-}

throwListM
  :: (MonadThrow m)
  => [a] -> m a

throwListM = throwListWithM e
  where
  e = someMonadThrowException ([] :: [()])

--------------------------------------------------

{-|

Generalize '[]' (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

-}

throwListWithM
  :: (MonadThrow m)
  => E.SomeException
  -> [a] -> m a
 
throwListWithM e
  = Safe.headMay
  > throwMaybeWithM e

--------------------------------------------------

{-|

Generalize @'Either' 'E.SomeException'@ (a concrete, pure 'MonadThrow' instance),
to an abstract 'MonadThrow' @m@.

@
≡ 'either' 'throwM' 'return'
@

-}

throwEitherM
  :: (MonadThrow m)
  => Either E.SomeException a -> m a

throwEitherM = either throwM return

--------------------------------------------------
--------------------------------------------------

newtype CallStack' = CallStack'
  { toCallFrames :: Seq CallFrame
  } deriving (Show,Eq,Ord,Generic,NFData)

instance Hashable CallStack' where
  hashWithSalt s (CallStack' frames)
    = foldl' hashWithSalt s frames

data CallFrame = CallFrame
 { _CallFrame_caller   :: !GUI
 , _CallFrame_callSite :: !Source
 } deriving (Show,Eq,Ord,Generic,NFData,Hashable)

{-| A single location in the source code.

Equivalent to 'SrcLoc':

@
srcLocPackage   :: String
srcLocModule    :: String
srcLocFile      :: String
srcLocStartLine :: Int	 
srcLocStartCol  :: Int	 
srcLocEndLine   :: Int	 
srcLocEndCol    :: Int
@

but with more instances. 

-}
data Source = Source
  { _sourcePackage     :: !Text
  , _sourceModule      :: !Text
  , _sourceFilename    :: !Text
  , _sourceFileSpan    :: !FileSpan
  }  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

{-| The location of something spanning a contiguous region in a file.

The @[start .. end]@ range is inclusive.

e.g. a highlighted region. 

-}
data FileSpan = FileSpan
  { _spanStart   :: !FilePosition
  , _spanEnd     :: !FilePosition
  }  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

{-| The location of a single cell (e.g. a character) in a file.

We conceive text files as grids, so this is equivalent to a 2 dimensional point, with different naming. The line number '_fileLine' is like the y-coordinate (descending vertically); the column number '_fileColumn' being the x-coordinate. 

TODO One-indexed ("the first line") versus Zero-indexed?

-}
data FilePosition = FilePosition
  { _fileLine     :: !Int -- !Natural
  , _fileColumn   :: !Int -- !Natural
  }  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- data Source = Source
--   { _sourcePackage     :: String
--   , _sourceModule      :: String
--   , _sourceFile        :: String
--   , _sourceStartLine   :: Int
--   , _sourceStartColumn :: Int
--   , _sourceEndLine     :: Int
--   , _sourceEndColumn   :: Int
--   }  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Exception)

--------------------------------------------------

{-| 

@
throwEither = 'either'
 'throwE'
 'return'
@

-}

throwEither
  :: ( MonadThrow m
     , E.Exception e
     )    
  => Either e a
  -> m a
throwEither = either
 throwE
 return

throwEitherWith
  :: ( MonadThrow m
     , Show e
     )    
  => Either e a
  -> m a
throwEitherWith
  = (first (show > someQuotedException 'throwEitherWith))
  > throwEither

--------------------------------------------------

throwMaybe
  :: ( MonadThrow m
     )    
  => Maybe a
  -> m a
throwMaybe = throwMaybeWith
  (someQuotedException 'throwMaybeWith "")

throwMaybeWith
  :: ( MonadThrow m
     , E.Exception e
     )    
  => e
  -> Maybe a
  -> m a
throwMaybeWith e
 = maybe2either e
 > throwEither

--------------------------------------------------

throwList
  :: ( MonadThrow m
     )    
  => List a
  -> m a
throwList = throwListWith
  (someQuotedException 'throwList "")

throwListWith
  :: ( MonadThrow m
     , E.Exception e
     )    
  => e
  -> List a
  -> m a
throwListWith e
  = list2maybe
  > throwMaybeWith e

--------------------------------------------------

-- throwNonEmpty
--   :: ( MonadThrow m
--      , Show a
--      )
--   => NonEmpty a
--   -> m a
-- throwNonEmpty = throwNonEmptyWith
--   (someQuotedException 'throwNonEmpty "")

-- throwNonEmptyWith
--   :: ( MonadThrow m
--      , E.Exception e 
--      )    
--   => e
--   -> NonEmpty a
--   -> m a
-- throwNonEmptyWith e
--   = list2maybe
--   > throwMaybeWith e

--------------------------------------------------

data SimpleException = SimpleException
 { _SimpleException_message :: !String
 } deriving (Read,Eq,Ord,Generic,NFData,Hashable)

instance E.Exception SimpleException 

{- | custom for @Exception@ (non-@Read@able).

@= 'displaySimpleException'@

-}
instance Show SimpleException where
  show = displaySimpleException

-- | @'SimpleException' ""@
instance Default SimpleException where
  def = SimpleException ""

-- | 'SimpleException'
instance IsString SimpleException where
  fromString = SimpleException

--------------------------------------------------

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

--------------------------------------------------
  
data QuotedException = QuotedException
 { _QuotedException_caller   :: !GUI
 , _QuotedException_message  :: !String
 } deriving (Eq,Ord,Generic,NFData,Hashable)

instance E.Exception QuotedException

{- | custom for @Exception@ (non-@Read@able).

@= 'displayQuotedException'@

-}
instance Show QuotedException where
  show = displayQuotedException

-- | @"" :: QuotedException@ (see the 'IsString' instance).
instance Default QuotedException where
  def = fromString ""

{- | @= QuotedException \''throwM'@.

 NOTE the prefixing apostrophe is a @TemplateHaskellQuotes@ name quote
 (not a typo)
 
-}
instance IsString QuotedException where
  fromString = QuotedException (unsafeGUI 'throwM)

-- instance NFData QuotedException where
--   rnf QuotedException{..}
--         = rnfName _QuotedException_caller
--     `seq` rnf     _QuotedException_message 

-- instance Hashable QuotedException where
--   hashWithSalt s QuotedException{..}
--     = s
--     `hashNameWithSalt`   _QuotedException_caller
--     `hashStringWithSalt` _QuotedException_message
--     where
--     hashStringWithSalt :: Int -> String -> Int
--     hashStringWithSalt = hashWithSalt

--------------------------------------------------

{-NOTES


    default hashWithSalt :: (Generic a, GHashable Zero (Rep a)) => Int -> a -> Int
    hashWithSalt salt = ghashWithSalt HashArgs0 salt . from




rnfName :: Name -> ()
rnfName = g
-- rnfName = rnf

hashNameWithSalt :: Int -> Name -> Int
hashNameWithSalt = g

-}



-- | 'formatCustomExceptionWithMessage'
displayQuotedException :: QuotedException -> String
displayQuotedException QuotedException{..}
  = formatCustomExceptionWithMessage
      caller
      _QuotedException_message

  where
  caller = _QuotedException_caller & displayGUI -- QualifiedVariable

--------------------------------------------------

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

--------------------------------------------------

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

--------------------------------------------------
-- MONAD THROW

{- | @E@ for 'Exception',

'throwM's a 'SimpleException'.

-}
throwE :: (MonadThrow m, E.Exception e) => e -> m a
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
throwN name s = throwM (QuotedException (unsafeGUI name) s)

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

{-

> import GHC.Stack.Types (HasCallStack)

> caller = 'throwL' "this is an example" :: (MonadThrow m, HasCallStack) => m ()

-}

--------------------------------------------------

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
     , E.Exception e
     )
  => e
  -> Bool -> m ()
guardE e = \case
  True  -> pure ()
  False -> throwM (E.toException e)

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

--------------------------------------------------

-- | 'someSimpleException_'
uninformative :: E.SomeException
uninformative = someSimpleException_

-- | the 'def'ault 'SimpleException'. 
someSimpleException_ :: E.SomeException
someSimpleException_ = E.SomeException
 (def :: SimpleException)

-- | the 'def'ault 'QuotedException'. 
someQuotedException_ :: E.SomeException
someQuotedException_ = E.SomeException
 (def :: QuotedException)

--------------------------------------------------

-- | 
someSimpleException
  :: String
  -> E.SomeException
someSimpleException s = E.SomeException $ 
 (SimpleException s)

-- | 
someQuotedException
  :: Name
  -> String
  -> E.SomeException
someQuotedException n s = E.SomeException $
 (QuotedException (unsafeGUI n) s)

--------------------------------------------------
--------------------------------------------------

#if HAS_GHC_HasCallStack

--------------------------------------------------


--------------------------------------------------

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

--------------------------------------------------

-- | @L@ for @Location@ or 'CallStack' (@caLLstack@). 
--
guardL 
  :: (MonadThrow m, HasCallStack)
  => Bool -> m ()
guardL = \case
  True  -> pure ()
  False -> throwM someLocatedException_

--------------------------------------------------

-- | the 'def'ault 'LocatedException'. 
someLocatedException_ :: HasCallStack => E.SomeException
someLocatedException_ = E.SomeException
 (def :: LocatedException)

--------------------------------------------------

-- | 
someLocatedException
  :: HasCallStack
  => String
  -> E.SomeException
someLocatedException s = E.SomeException $
 (toLocatedException s)

--------------------------------------------------

data LocatedException = LocatedException

 { _LocatedException_stack    :: !CallStack
 , _LocatedException_message  :: !String

 --} deriving (Eq,Ord,Generic,NFData,Hashable)
 } deriving (Generic)

--------------------------------------------------

instance E.Exception LocatedException

--------------------------------------------------

{- | custom for @Exception@ (non-@Read@able).

@= 'displayLocatedException'@

-}

instance Show LocatedException where
  show = displayLocatedException

--------------------------------------------------

-- | @"" :: LocatedException@ (see the 'IsString' instance).
instance Default LocatedException where
  def = fromString ""

--------------------------------------------------

-- | Requires 'HasCallStack' around wherever the string literal is (i.e. at the "call-site" of @fromString@). 
instance IsString LocatedException where
  fromString = LocatedException callStack 

--------------------------------------------------

-- | @'LocatedException' 'callStack' _@
toLocatedException :: (HasCallStack) => String -> LocatedException
toLocatedException _LocatedException_message =
  LocatedException{..}
  where
  _LocatedException_stack = callStack

--------------------------------------------------

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

--------------------------------------------------

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

#endif

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-- -- | 
-- throwE :: (MonadThrow m, HasCallStack) => String -> m a
-- throwE = throwM 

{-
-- | @'guard' b@ is @'pure' ()@ if @b@ is 'True',
-- and 'empty' if @b@ is 'False'.
guard           :: (Alternative f) => Bool -> f ()
guard True      =  pure ()
guard False     =  empty

guardM :: (MonadThrow f) => E.SomeException -> Bool -> f ()
guardM _ True      =  pure ()
guardM e False     =  throwM e

guardM :: (MonadThrow f) => Bool -> f ()
guardM True  =  pure ()
guardM False =  throwM uninformativeException

uninformativeException :: E.SomeException
uninformativeException = ErrorCall ""

guardM_ :: (MonadThrow f, HasCallStack) => Bool -> f ()
guardM_ = \case
  True -> pure ()
  False -> throwM locatedException

locatedException :: HasCallStack => E.SomeException
locatedException = E.SomeException $ errorCallWithCallStackException "" ?callStack

-}

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------