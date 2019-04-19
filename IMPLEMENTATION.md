# `spiros` Implementation

## `ghc` compiler ↔ `base` package

GHC 8.8:

* `ghc` v`8.8` — `base` v`TODO`

GHC 8.6:

* `ghc` v`8.6.4` — `base` v`4.12.0.0`
* `ghc` v`8.6.2` — `base` v`4.11.1.0`
* `ghc` v`8.6.1` — `base` v`4.11.0.0`

GHC 8.4:

* `ghc` v`8.4.3` — `base` v`4.10.1.0`
* `ghc` v`8.4.2` — `base` v`4.10.0.0`
* `ghc` v`8.4.1` — `base` v`4.9.1.0`

GHC 8.2:

* `ghc` v`8.2.2` — `base` v`4.9.0.0`
* `ghc` v`8.2.1` — `base` v`4.8.2.0`

GHC 8.0:

* `ghc` v`8.0.2` — `base` v`4.8.1.0`
* `ghc` v`8.0.1` — `base` v`4.8.0.0`

GHC 7.10:

* `ghc` v`7.10.3` — `base` v`4.7.0.2`
* `ghc` v`7.10.2` — `base` v`4.7.0.1`
* `ghc` v`7.10.1` — `base` v`4.7.0.0`

## Conditional-Compilation

See `./spiros/include/sboo-base-feature-macros.h` for named macros (inspired by `@hvr`'s `base-feature-macros.h`).

Compiler Macros:

* `#define IS_COMPILER_ghc`
* `#define IS_COMPILER_ghcjs`

Operating-System Macros:

* `#define IS_OS_LINUX`
* `#define IS_OS_WINDOWS`
* `#define IS_OS_APPLE`

Architecture Macros:

* `#define IS_ARCH_64_BIT_INTEL`
* `#define IS_ARCH_32_BIT_INTEL`

`base` Macros:

* `#define HAS_APPLICATIVE_MONAD`
* `#define HAS_BASE_BINARY_LIFTED_CLASSES`
* `#define HAS_BASE_Bifoldable_Bitraversable`
* `#define HAS_BASE_Bifunctor`
* `#define HAS_BASE_Contravariant`
* `#define HAS_BASE_Functors`
* `#define HAS_BASE_Identity`
* `#define HAS_BASE_MonadIO`
* `#define HAS_BASE_Natural`
* `#define HAS_BASE_NonEmpty`
* `#define HAS_BASE_Semigroup`
* `#define HAS_BASE_UNARY_LIFTED_CLASSES`
* `#define HAS_FOLDABLE_TRAVERSABLE_IN_PRELUDE`
* `#define HAS_GHC_CallStack`
* `#define HAS_METHOD_Exception_displayException`
* `#define HAS_MONAD_FAIL`
* `#define HAS_PRELUDE_Monoid`
* `#define HAS_PRELUDE_OPERATOR_Append`

GHC Extension Macros:

* `#define HAS_EXTENSION_DeriveAnyClass`
* `#define HAS_EXTENSION_DeriveFunctor`
* `#define HAS_EXTENSION_DerivingLift`
* `#define HAS_EXTENSION_DuplicateRecordFields`
* `#define HAS_EXTENSION_DerivingStrategies`

GHC Pragma Macros:

* `#define HAS_PRAGMA_COMPLETE`

Package Macros:

* `#define HAS_HASHABLE_Hashable1`
* `#define HAS_HASHABLE_Hashable2`
* `#define HAS_DEEPSEQ_NFData1`
* `#define HAS_DEEPSEQ_NFData2`

## Links

* <>
* <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory>

## 