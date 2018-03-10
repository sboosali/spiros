{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, PackageImports #-}

{-| This module re-exports several "standard" typeclasses (that every type should derive, if it can), and their methods. It only re-exports other (non-typeclass, non-method) definitions when they are convenient for manually defining instances.

--

@deepseq@:
* 'NFData'
* 'NFData1' (and 'rnf1')
* 'NFData2' (and 'rnf2')
* 'seq', 'whnf' (helpers for manual instances)

--

@hashable@:
* 'Hashable'
* 'hashUsing' (helpers for manual instances)

--

@data-default-class@:
* 'Default'

--

@exceptions@:
* 'MonadThrow'
* 'MonadCatch'
* 'MonadMask'

--

@mtl@:
* 'MonadReader'
* 'MonadWriter'
* 'MonadState'
* 'MonadError'

@transformers@: 
* 'MonadTrans'

--

@base@ "stock deriveable": 
* 'Show'
Read
Eq
Ord
Enum
Bounded
Ix

syntax: 
* 'IsList'
* 'IsString'
* (and, sans extensions, 'Num', 'Enum')

@base@ numbers:
* 'Num'
* 'Real'
* 'Integral'
* 'Fractional'
* 'Floating'
* 'RealFrac'
* 'RealFloat'

@base@ monoid:
* 'Semigroup'
* 'Monoid'

@base@ functor:
* 'Functor'

@base@ containers:
* 'Foldable'
* 'Traversable'

@base@ applicative:
* 'Applicative'
* 'Alternative'

@base@ monad:
* 'Monad'
* 'MonadPlus'
* 'MonadFail'
* 'MonadFix'

@base@ arrow (which I don't use):
* 'Arrow'
* 'ArrowZero'
* 'ArrowPlus'
* 'ArrowChoice'
* 'ArrowApply'
* 'ArrowLoop'

@base@ category:
* 'Category'

@base@ bifunctors:
* 'Bifunctor'
* 'Bifoldable'
* 'Bitraversable'

@base@ generics:
* 'Generic'
* 'Data'
* 'Typeable'

@base@ ffi:
* 'Storable'

@base@ (miscellaneous):
* 'MonadIO'
* 'Exception'

@base@ unary liftings (of standard nullary classes):
* 'Eq1' (and 'eq1')
* 'Ord1' (and 'compare1')
* 'Show1' (and 'showsPrec1')
* 'Read1' (and 'readsPrec1')

@base@ binary liftings (of standard nullary classes):
* 'Eq2' (and 'eq2')
* 'Ord2' (and 'compare2')
* 'Show2' (and 'showsPrec2')
* 'Read2' (and 'readsPrec2')

--



NOTES

Standard Numeric Classes (copied from the @The Haskell 98 Report@):

@
class  ('Eq' a) => 'Num' a  where
    (+), (-), (*)   :: a -> a -> a
    'negate'        :: a -> a
    'abs', 'signum' :: a -> a
    'fromInteger'   :: Integer -> a

class  (Num 'a', Ord a) => 'Real' a  where
    'toRational' ::  a -> Rational

class  (Real a, 'Enum' a) => 'Integral' a  where
    ''quot'', rem, 'div', 'mod' :: a -> a -> a
    'quotRem', 'divMod'         :: a -> a -> (a,a)
    'toInteger'                 :: a -> Integer

class  (Num a) => 'Fractional' a  where
    (/)            :: a -> a -> a
    'recip'        :: a -> a
    'fromRational' :: Rational -> a

class  (Fractional a) => 'Floating' a  where
    'pi'                      :: a
    'exp', 'log', 'sqrt'      :: a -> a
    (**), 'logBase'           :: a -> a -> a
    'sin', 'cos', 'tan'       :: a -> a
    'asin', 'acos', 'atan'    :: a -> a
    'sinh', 'cosh', 'tanh'    :: a -> a
    'asinh', 'acosh', 'atanh' :: a -> a

class  (Real a, Fractional a) => 'RealFrac' a  where
    'properFraction'    :: (Integral b) => a -> (b,a)
    'truncate', 'round' :: (Integral b) => a -> b
    'ceiling', 'floor'  :: (Integral b) => a -> b

class  (RealFrac a, Floating a) => 'RealFloat' a  where
    'floatRadix'  :: a -> Integer
    'floatDigits' :: a -> Int
    'floatRange'  :: a -> (Int,Int)
    'decodeFloat' :: a -> (Integer,Int)
    'encodeFloat' :: Integer -> Int -> a
    'exponent'    :: a -> Int
    'significand' :: a -> a
    'scaleFloat'  :: Int -> a -> a
    'isNaN', 'isInfinite', 'isDenormalized', 'isNegativeZero', 'isIEEE' 
                  :: a -> Bool
    'atan2'       :: a -> a -> a

-- 
'gcd', 'lcm' :: (Integral a) => a -> a-> a
(^)      :: (Num a, Integral b) => a -> b -> a
(^^)     :: (Fractional a, Integral b) => a -> b -> a
-- 
'fromIntegral' :: (Integral a, Num b) => a -> b
'realToFrac'   :: (Real a, Fractional b) => a -> b

@


Standard Enumeration Classes:

@

class Ord a => 'Ix' a where

For an enumeration, the nullary constructors are assumed to be numbered left-to-right with the indices being 0 to n-1 inclusive, like @Enum@. For example:

       data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet

       'range'   (Yellow,Blue)        ==  [Yellow,Green,Blue]
       'index'   (Yellow,Blue) Green  ==  1
       'inRange' (Yellow,Blue) Red    ==  False
@

-}
module Prelude.Spiros.Classes
  ( module X
  ) where

--

import "deepseq" Control.DeepSeq                     as X (NFData(..))
import "deepseq" Control.DeepSeq                     as X (NFData1(..))
import "deepseq" Control.DeepSeq                     as X (NFData2(..))
import "deepseq" Control.DeepSeq                     as X (rnf1,rnf2,rwhnf)
import "base"    Prelude                             as X (seq)

import "hashable" Data.Hashable                      as X (Hashable(..))
import "hashable" Data.Hashable                      as X (hashUsing)

import "data-default-class" Data.Default.Class       as X (Default(..))

import "exceptions" Control.Monad.Catch              as X (MonadThrow(..))
import "exceptions" Control.Monad.Catch              as X (MonadCatch(..))
import "exceptions" Control.Monad.Catch              as X (MonadMask(..))

--

import "mtl" Control.Monad.Reader.Class              as X (MonadReader(..))
import "mtl" Control.Monad.Writer.Class              as X (MonadWriter(..))
import "mtl" Control.Monad.State.Class               as X (MonadState(..))
import "mtl" Control.Monad.Error.Class               as X (MonadError(..))

import "transformers" Control.Monad.Trans.Class      as X (MonadTrans(..))

--

import "base" Prelude                                as X (Show(..))
import "base" Prelude                                as X (Read(..))

import "base" Prelude                                as X (Eq(..))
import "base" Prelude                                as X
 (Ord((<=),(>=))) -- hide `>` / `<`

import "base" Prelude                                as X (Enum(..))
import "base" Prelude                                as X (Bounded(..))

import "base" Prelude                                as X (Num(..))
import "base" Prelude                                as X (Real(..))
import "base" Prelude                                as X (Integral(..))
import "base" Prelude                                as X (Fractional(..))
import "base" Prelude                                as X (Floating(..))
import "base" Prelude                                as X (RealFrac(..))
import "base" Prelude                                as X (RealFloat(..))

import "base" Data.Ix                                as X (Ix(..))
import "base" Data.Bits                              as X (Bits(..))
import "base" Data.Bits                              as X (FiniteBits(..))

import "base" Data.Semigroup                         as X (Semigroup(..))
import "base" Data.Monoid                            as X (Monoid(..))

import "base" Data.Functor                           as X (Functor(..))
import "base" Data.Foldable                          as X (Foldable(..))
import "base" Data.Traversable                       as X (Traversable(..))

import "base" Control.Applicative                    as X (Applicative(..))
import "base" Control.Applicative                    as X (Alternative(..))

import "base" Control.Monad                          as X
 (Monad((>>=),return,(>>))) -- hide `fail`
import "base" Control.Monad                          as X (MonadPlus(..))
import "base" Control.Monad.Fail                     as X (MonadFail(..))
import "base" Control.Monad.Fix                      as X (MonadFix(..))

import "base" Control.Arrow                          as X (Arrow)
import "base" Control.Arrow                          as X (ArrowZero)
import "base" Control.Arrow                          as X (ArrowPlus)
import "base" Control.Arrow                          as X (ArrowChoice)
import "base" Control.Arrow                          as X (ArrowApply)
import "base" Control.Arrow                          as X (ArrowLoop)

import "base" Control.Category                       as X
 (Category) -- can't export `(.)` and `id`, which conflict with their specializations TODO?

import "base" Data.Bifunctor                         as X (Bifunctor(..))
import "base" Data.Bifoldable                        as X (Bifoldable(..))
import "base" Data.Bitraversable                     as X (Bitraversable(..))

import "base" Control.Monad.IO.Class                 as X (MonadIO(..))
import "base" Control.Exception                      as X (Exception(..))

import "base" Foreign.Storable                       as X (Storable(..))

import "base" GHC.Generics                           as X (Generic)
import "base" Data.Data                              as X (Data)
import "base" Data.Typeable                          as X (Typeable)

import "base" GHC.Exts                               as X (IsList(Item,fromList))
 -- hide `toList`
import "base" GHC.Exts                               as X (IsString(..))

-- unary lifted
import Data.Functor.Classes                          as X (Eq1(..),eq1)
import Data.Functor.Classes                          as X (Ord1(..),compare1)
import Data.Functor.Classes                          as X (Show1(..),showsPrec1)
import Data.Functor.Classes                          as X (Read1(..),readsPrec1)

-- biary lifted
import Data.Functor.Classes                          as X (Eq2(..),eq2)
import Data.Functor.Classes                          as X (Ord2(..),compare2)
import Data.Functor.Classes                          as X (Show2(..),showsPrec2)
import Data.Functor.Classes                          as X (Read2(..),readsPrec2)

{-

import "base" Text.Printf                            as X (PrintfArg(..))

import "base" Control.Category                       as X (Category(..))

import "string-conv" Data.String.Conv                as X (StringConv (..),Leniency (..))

import "base" Control.Arrow                          as X (Arrow(..))
import "base" Control.Arrow                          as X (ArrowZero(..))
import "base" Control.Arrow                          as X (ArrowPlus(..))
import "base" Control.Arrow                          as X (ArrowChoice(..))
import "base" Control.Arrow                          as X (ArrowApply(..))
import "base" Control.Arrow                          as X (ArrowLoop(..))

-}

----------------------------------------

----------------------------------------

{-TODO

class PrintfArg a where

Extending To New Types
This printf can be extended to format types other than those provided for by default. This is done by instantiating PrintfArg and providing a formatArg for the type. It is possible to provide a parseFormat to process type-specific modifiers, but the default instance is usually the best choice.

For example:

instance PrintfArg () where
  formatArg x fmt | fmtChar (vFmt 'U' fmt) == 'U' =
    formatString "()" (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

main :: IO ()
main = printf "[%-3.1U]\n" ()
prints "[() ]". Note the use of formatString to take care of field formatting specifications in a convenient way.


-}
