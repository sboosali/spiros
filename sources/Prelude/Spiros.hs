{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-|

exports:

* single-character composition, i.e. '(>)' an '(<)'
* frequently(/universally) derived classes,
i.e. @deriving (...,'Data','Generic','NFData','Semigroup')@

hides:

* partial functions, e.g. 'head'

see:

* <http://www.stephendiehl.com/posts/protolude.html>

-}
module Prelude.Spiros
 ( module Base
 , module X -- re-eXports
 )
where

import Spiros.Utilities as X

import Control.DeepSeq as X (NFData)
import Data.Semigroup  as X (Semigroup)
import Safe            as X

import GHC.Generics    as X (Generic)
import Data.Data       as X (Data)
import Data.Function   as X ((&),on)
import Data.Foldable   as X (traverse_)
import Control.Arrow   as X ((>>>),(<<<))
import Data.Set        as X (Set)
import Data.Map        as X (Map)
import Numeric.Natural as X (Natural)

#if MIN_VERSION_base(4,8,0)
#else
import Data.Functor((<$>))
import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..))
#endif

import Data.List as Base hiding
 ( minimumBy
 , maximumBy
 , (!!)
 , find
 )

import Prelude as Base hiding
 ( (<), (>)
 -- partials
 , error, undefined
 , tail
 , init
 , head
 , last
 , minimum
 , maximum
 , foldr1
 , foldl1
 , foldl1
 , scanl1
 , scanr1
 , read
 , toEnum
 )
