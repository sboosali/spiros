{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      :  Prelude.Spiros
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

(Re-)Exports:

* single-character composition, i.e. ('>') an ('<')
* universally (or frequently) derived classes,
i.e. @deriving (...,'Data','Generic','NFData','Semigroup')@
* @safe-exceptions@'s @'throw'@, which generalizes @IO@ to 'MonadThrow'
* type names for common types (lazy text, lazy bytes, etc)
* and more (see the source)

Hides:

* partial functions, e.g. 'head'
* some aliased functions (like @sequence@, which is generalized into @sequenceA@).

Also see:

* <http://www.stephendiehl.com/posts/protolude.html>

-}
module Prelude.Spiros
 ( module X -- re-eXports
 )
where

import Prelude.Spiros.Reexports                      as X
import Prelude.Spiros.Utilities                      as X
import Prelude.Spiros.Exception                      as X
import Prelude.Spiros.Validator                      as X
import Prelude.Spiros.GUI                            as X
