{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------

{-| This module re-exports several "standard" typeclasses (that every type should derive, if it can), and their methods. It only re-exports other (non-typeclass, non-method) definitions when they are convenient for manually defining instances.

Also include some shims for backwards-compability (motivated by the 2018 @reflex-platform@). 


= Notes

Derive 'Lift':

* either automatically via @-XDeriveLift@;
* or manually, which requires importing the method too, @import Language.Haskell.TH.Syntax (Lift(..))@ (this doesn't re-export it because 'Language.Haskell.TH.Syntax.lift' is too broad a name). 


= Re-Exports

@deepseq@:

* 'NFData'
* 'NFData1' (and 'rnf1')
* 'NFData2' (and 'rnf2')
* 'seq', 'whnf' (helpers for manual instances)



@hashable@:

* 'Hashable'
* 'hashUsing' (helpers for manual instances)



@data-default-class@:

* 'Default'



@exceptions@:

* 'MonadThrow'
* 'MonadCatch'
* 'MonadMask'



@mtl@:

* 'MonadReader'
* 'MonadWriter'
* 'MonadState'
* 'MonadError'

@transformers@:

* 'MonadTrans'

@template-haskell@

* 'Lift'


@base@ "stock deriveable":

* 'Show'
* 'Read'
* 'Eq'
* 'Ord'
* 'Enum'
* 'Bounded'
* 'Ix'

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





NOTES

Foldable:

@Foldable@ doesn't subclass @Functor@, and it absorbs several "secondary functions" as "primary methods", for efficiency:

@
-- e.g. list, the canonical foldable

instance Foldable [] where
    elem    = List.elem
    foldl   = List.foldl
    foldl'  = List.foldl'
    foldl1  = List.foldl1
    foldr   = List.foldr
    foldr1  = List.foldr1
    length  = List.length
    maximum = List.maximum
    minimum = List.minimum
    null    = List.null
    product = List.product
    sum     = List.sum
    toList  = id
@


@

Lifted Classes:

@
-- e.g. given already-defined `<C>1` instances for some "functor" (unary type constructor) or "transformer" (binary type constructor)...

instance (Eq1   f) => Eq1   (Validated f) where ...
instance (Ord1  f) => Ord1  (Validated f) where ...
instance (Read1 f) => Read1 (Validated f) where ...
instance (Show1 f) => Show1 (Validated f) where ...

-- .. you can derive the `<C>` instances...

instance (Eq1 f,   Eq a,   Eq b)   => Eq   (Validated f a b) where
  (==) = eq1

instance (Ord1 f,  Ord a,  Ord b)  => Ord  (Validated f a b) where
  compare = compare1

instance (Read1 f, Read a, Read b) => Read (Validated f a b) where
  readPrec     = readPrec1
  readListPrec = readListPrecDefault
  
instance (Show1 f, Show a, Show b) => Show (Validated f a b) where
  showsPrec = showsPrec1
@


@
-- e.g. the `C1` lifted instances for `Maybe`...

instance 'Eq1' Maybe where
    liftEq _  Nothing  Nothing  = True
    liftEq _  Nothing  (Just _) = False
    liftEq _  (Just _) Nothing  = False
    liftEq eq (Just x) (Just y) = eq x y

instance 'Ord1' Maybe where
    liftCompare _ Nothing Nothing = EQ
    liftCompare _ Nothing (Just _) = LT
    liftCompare _ (Just _) Nothing = GT
    liftCompare comp (Just x) (Just y) = comp x y

instance 'Read1' Maybe where
    liftReadPrec rp _ =
        parens ('expectP' ('Ident' "Nothing") *> pure Nothing)
        <|>
        readData ('readUnaryWith' rp "Just" Just)

    liftReadListPrec = 'liftReadListPrecDefault'
    liftReadList     = 'liftReadListDefault'

instance 'Show1' Maybe where
    liftShowsPrec _  _ _ Nothing  = 'showString' "Nothing"
    liftShowsPrec sp _ d (Just x) = 'showsUnaryWith' sp "Just" d x
@


@
-- e.g. the `C2` lifted instances for `Either`...

instance 'Eq2' Either where
    liftEq2 eq1 _  (Left  x) (Left  y) = eq1 x y
    liftEq2 _   _  (Left  _) (Right _) = False
    liftEq2 _   _  (Right _) (Left  _) = False
    liftEq2 _  eq2 (Right x) (Right y) = eq2 x y

instance 'Ord2' Either where
    liftCompare2 comp1 _     (Left  x) (Left  y) = comp1 x y
    liftCompare2 _     _     (Left  _) (Right _) = LT
    liftCompare2 _     _     (Right _) (Left  _) = GT
    liftCompare2 _     comp2 (Right x) (Right y) = comp2 x y

instance 'Read2' Either where
    liftReadPrec2 rp1 _ rp2 _ = 'readData' $ 'asum'
         [ readUnaryWith rp1 "Left"  Left 
         , readUnaryWith rp2 "Right" Right
         ]

    liftReadListPrec2 = 'liftReadListPrec2Default'
    liftReadList2     = 'liftReadList2Default'

instance 'Show2' Either where
    liftShowsPrec2 sp1 _ _   _ d (Left  x)
        = showsUnaryWith sp1 "Left" d x
    liftShowsPrec2 _   _ sp2 _ d (Right x)
        = showsUnaryWith sp2 "Right" d x

@


Standard Numeric Classes (copied from the @The Haskell 98 Report@):

@
class  ('Eq' a) => 'Num' a  where
    ('+'), ('-'), ('*')   :: a -> a -> a
    'negate'          :: a -> a
    'abs', 'signum'     :: a -> a
    'fromInteger'     :: Integer -> a

class  (Num 'a', Ord a) => 'Real' a  where
    'toRational' ::  a -> Rational

class  (Real a, 'Enum' a) => 'Integral' a  where
    'quot', 'rem', 'div', 'mod' :: a -> a -> a
    'quotRem', 'divMod'     :: a -> a -> (a,a)
    'toInteger'           :: a -> Integer

class  (Num a) => 'Fractional' a  where
    ('/')          :: a -> a -> a
    'recip'        :: a -> a
    'fromRational' :: Rational -> a

class  (Fractional a) => 'Floating' a  where
    'pi'                  :: a
    'exp', 'log', 'sqrt'      :: a -> a
    ('**'), 'logBase'       :: a -> a -> a
    'sin', 'cos', 'tan'       :: a -> a
    'asin', 'acos', 'atan'    :: a -> a
    'sinh', 'cosh', 'tanh'    :: a -> a
    'asinh', 'acosh', 'atanh' :: a -> a

class  (Real a, Fractional a) => 'RealFrac' a  where
    'properFraction'  :: (Integral b) => a -> (b,a)
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
('^')      :: (Num a, Integral b) => a -> b -> a
('^^')     :: (Fractional a, Integral b) => a -> b -> a
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

--------------------------------------------------

module Prelude.Spiros.Classes

--------------------------------------------------

  ( module X
  , module Prelude.Spiros.Classes
  ) where

--------------------------------------------------
-- `deepseq`
--------------------------------------------------

import "deepseq" Control.DeepSeq                     as X (NFData(..))
import "base"    Prelude                             as X (seq)

#if MIN_VERSION_deepseq(1,4,3)
import "deepseq" Control.DeepSeq                     as X (NFData1(..))
import "deepseq" Control.DeepSeq                     as X (NFData2(..))
import "deepseq" Control.DeepSeq                     as X (rnf1,rnf2)
#endif

#if MIN_VERSION_deepseq(1,4,3)
import "deepseq" Control.DeepSeq                     as X (rwhnf)  
#else
-- see below
#endif

--------------------------------------------------
-- `hashable`
--------------------------------------------------

import "hashable" Data.Hashable                      as X (Hashable(..))
import "hashable" Data.Hashable                      as X (hashUsing)

--------------------------------------------------
-- `data-default-class`
--------------------------------------------------

import "data-default-class" Data.Default.Class       as X (Default(..))

--------------------------------------------------
-- `semigroups`
--------------------------------------------------

import "semigroups" Data.Semigroup.Generic           as X
 ( gmappend, gmempty
 )

--------------------------------------------------
-- `exceptions`
--------------------------------------------------

import "exceptions" Control.Monad.Catch              as X (MonadThrow(..))
import "exceptions" Control.Monad.Catch              as X (MonadCatch(..))
import "exceptions" Control.Monad.Catch              as X (MonadMask(..))

--------------------------------------------------
-- `generic-deriving`
--------------------------------------------------

import "generic-deriving" Generics.Deriving.Enum     as X
 ( GEnum(..)
 -- , GIx -- NOTE GIx's methods conflict with Ix.
 )

--------------------------------------------------
-- `mtl`
--------------------------------------------------

import "mtl" Control.Monad.Reader.Class              as X (MonadReader(..))
import "mtl" Control.Monad.Writer.Class              as X (MonadWriter(..))
import "mtl" Control.Monad.State.Class               as X (MonadState(..))
import "mtl" Control.Monad.Error.Class               as X (MonadError(..))

--------------------------------------------------
-- `transformers`
--------------------------------------------------

import "transformers" Control.Monad.Trans.Class      as X (MonadTrans(..))

--------------------------------------------------
-- `template-haskell`
--------------------------------------------------

import "template-haskell" Language.Haskell.TH.Syntax as X (Lift)

--------------------------------------------------
-- `base:Prelude`
--------------------------------------------------

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

--------------------------------------------------
-- `base`
--------------------------------------------------

import "base" Data.Ix                                as X (Ix(..))
import "base" Data.Bits                              as X (Bits(..))
import "base" Data.Bits                              as X (FiniteBits(..))

import "base" Data.Monoid                            as X (Monoid(..))
import "base" Data.Semigroup                         as X (Semigroup(..))

import "base" Data.Functor                           as X (Functor(..))
import "base" Data.Foldable                          as X (Foldable(elem,foldl,foldl',foldl1,foldr,foldr1,length,maximum,minimum,null,product,sum)) -- `Foldable(toList)` conflicts with `IsList(toList)`
import "base" Data.Traversable                       as X (Traversable(..))

import "base" Control.Applicative                    as X (Applicative(..))
import "base" Control.Applicative                    as X (Alternative(..))

import "base" Control.Monad                          as X
 (Monad((>>=),return,(>>))) -- hide `fail`
import "base" Control.Monad                          as X (MonadPlus(..))

#ifdef HAVE_MONAD_FAIL
import "base" Control.Monad.Fail                     as X (MonadFail(..))
#endif

import "base" Control.Monad.Fix                      as X (MonadFix(..))

import "base" Control.Arrow                          as X (Arrow)
import "base" Control.Arrow                          as X (ArrowZero)
import "base" Control.Arrow                          as X (ArrowPlus)
import "base" Control.Arrow                          as X (ArrowChoice)
import "base" Control.Arrow                          as X (ArrowApply)
import "base" Control.Arrow                          as X (ArrowLoop)

import "base" Control.Category                       as X
 (Category) -- can't export `(.)` and `id`, which conflict with their specializations TODO?

#if MIN_VERSION_base(4,8,0)
import "base" Data.Bifunctor                         as X (Bifunctor(..))
#else
#endif

#if MIN_VERSION_base(4,10,0)
import "base" Data.Bifoldable                        as X (Bifoldable(..))
import "base" Data.Bitraversable                     as X (Bitraversable(..))
#else
#endif

import "base" Control.Monad.IO.Class                 as X (MonadIO(..))
import "base" Control.Exception                      as X (Exception(..))

import "base" Text.ParserCombinators.ReadP           as X
 (ReadP,ReadS,readP_to_S,readS_to_P)
 -- for writing `Read` instances

import "base" Foreign.Storable                       as X (Storable(..))

import "base" GHC.Generics                           as X (Generic, Rep)
import "base" GHC.Generics                           as X (Generic1, Rep1)
import "base" Data.Data                              as X (Data)
--import "base" Data.Typeable                          as X (Typeable)

import "base" GHC.Exts                               as X (IsList(Item,fromList))
 -- hide `toList`
import "base" GHC.Exts                               as X (IsString(..))

-- unary lifted
import Data.Functor.Classes                          as X (Eq1(..),eq1)
import Data.Functor.Classes                          as X (Ord1(..),compare1)
import Data.Functor.Classes                          as X (Show1(..),showsPrec1)
import Data.Functor.Classes                          as X (Read1(..),readsPrec1)

-- binary lifted
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

--------------------------------------------------
-- Non-Export Imports ----------------------------
--------------------------------------------------

import qualified "base" GHC.Generics                 as Generic

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

-- shims for backwards-compability (motivated by the `2018 reflex-platform`)

#if MIN_VERSION_deepseq(1,4,3)
-- see above
#else
rwhnf :: a -> ()
rwhnf = (`seq` ())
#endif
  
--------------------------------------------------
-- generics

-- | A generic representation, "specialized" to no additional metadata. 
--
-- (it still has the normal metadata about arity, constructor source location, field properties, etc). 
-- 
type Rep_ a = Rep a ()

-- | Convert from the datatype to its generic representation. 
--
-- @= 'Generic.from'@
--
-- Naming: like @fromEnum :: a -> Int@,
-- i.e. from the perspective of the instance type @a@. 
-- 
fromGeneric :: (Generic a) => a -> Rep a x
fromGeneric = Generic.from

-- | Convert to a generic representation from the datatype. 
--
-- @= 'Generic.to'@
--
-- Naming: like @toEnum :: Int -> a@,
-- i.e. from the perspective of the instance type @a@. 
-- 
toGeneric :: (Generic a) => Rep a x -> a
toGeneric = Generic.to

-- | Convert from the datatype to its generic representation. 
--
-- @= 'Generic.from1'@
--
-- Naming: like @fromEnum :: a -> Int@,
-- i.e. from the perspective of the instance type @a@. 
-- 
fromGeneric1 :: (Generic1 f) => f a -> Rep1 f a
fromGeneric1 = Generic.from1

-- | Convert to a generic representation from the datatype. 
--
-- @= 'Generic.to1'@
--
-- Naming: like @toEnum :: Int -> a@,
-- i.e. from the perspective of the instance type @a@. 
-- 
toGeneric1 :: (Generic1 f) => Rep1 f a -> f a
toGeneric1 = Generic.to1

--warning: [-Wdodgy-exports]
--    The export item `module Prelude.Spiros.Classes' exports nothing

--------------------------------------------------
-- Notes / Old Code / Other Comments -------------
--------------------------------------------------

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
--------------------------------------------------