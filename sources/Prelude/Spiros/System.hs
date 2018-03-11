{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

{-|

(this module is similar to @Foundation.System@). 

-}
module Prelude.Spiros.System
  ( module Prelude.Spiros.System
  -- , module System.Info
  ) where

----------------------------------------

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities

import qualified "base" System.Info as Base
import qualified "base" GHC.Conc as GHC

----------------------------------------

-- | Enumeration of the known GHC supported operating systems.
--
data KnownOperatingSystem
  = Windows
  | OSX
  | Linux
  | Android
  | BSD
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Typeable)

-- | get the operating system on which the program is running.
--
-- Either return the known `OS` or a strict `String` of the OS name.
--
-- This function uses the `base`'s `System.Info.os` function.
--
currentOperatingSystem :: Either String KnownOperatingSystem
currentOperatingSystem = case Base.os of
    "darwin"        -> Right OSX
    "mingw32"       -> Right Windows
    "linux"         -> Right Linux
    "linux-android" -> Right Android
    "openbsd"       -> Right BSD
    "netbsd"        -> Right BSD
    "freebsd"       -> Right BSD
    s               -> Left s

----------------------------------------

-- | Enumeration of the known GHC supported architecture.
--
data KnownArchitecture
  = I386
  | X86_64
  | PowerPC
  | PowerPC64
  | Sparc
  | Sparc64
  | ARM
  | ARM64
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Typeable)

-- | get the machine architecture on which the program is running
--
-- Either return the known architecture or a Strict `String` of the
-- architecture name.
--
-- This function uses the `base`'s `System.Info.arch` function.
--
currentArchitecture :: Either String KnownArchitecture
currentArchitecture = case Base.arch of
    "i386"          -> Right I386
    "x86_64"        -> Right X86_64
    "powerpc"       -> Right PowerPC
    "powerpc64"     -> Right PowerPC64
    "powerpc64le"   -> Right PowerPC64
    "sparc"         -> Right Sparc
    "sparc64"       -> Right Sparc64
    "arm"           -> Right ARM
    "aarch64"       -> Right ARM64
    s               -> Left s

----------------------------------------

-- | Enumeration of the known GHC-based compilers. 
--
data KnownHaskellCompiler
  = GHC
  | GHCJS
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Typeable)

-- | get the compiler name
--
-- This function uses the `base`'s `System.Info.compilerName` function.
--
currentCompiler :: Either String KnownHaskellCompiler
currentCompiler = case Base.compilerName of
    "ghc"    -> Right GHC
    "ghcjs"  -> Right GHCJS
    s        -> Left s

----------------------------------------

-- | returns the number of CPUs the machine has
currentNumberOfCPUs :: IO Natural
currentNumberOfCPUs = unsafeNatural <$> GHC.getNumProcessors 

----------------------------------------


{-

https://guide.aelve.com/haskell/cpp-vww0qd72


Cpphs handles single quotes and /**/ correctly, doesn't mangle Haddock comments and knows about Haskell's multiline strings.

To use cpphs, you need to add these options to all sections of your .cabal file:

library
  ...
  build-tools: cpphs >= 1.19
  ghc-options: -pgmP cpphs -optP --cpp


The base-feature-macros package lets you write some macros more conveniently – e.g. instead of #if MIN_VERSION_base(4,8,0) you can write #if HAVE_FOLDABLE_TRAVERSABLE_IN_PRELUDE, which is much more understandable to a casual reader.





Detect OS
other
 move item up  move item down  edit item info  delete item
Summary  edit summary
The following variables are defined depending on OS:

mingw32_HOST_OS – Windows
darwin_HOST_OS – macOS
ghcjs_HOST_OS – Javascript (when compiling with GHCJS)
linux_HOST_OS – Linux (shouldn't be needed most of the time)
freebsd_HOST_OS – FreeBSD
netbsd_HOST_OS – NetBSD
openbsd_HOST_OS – OpenBSD
solaris_HOST_OS – Solaris
For instance, here's how you can detect macOS:

 #ifdef darwin_HOST_OS
  ...
 #endif
Note that despite lots of libraries using #if defined(mingw32_HOST_OS) || defined(__MINGW32__) for detecting Windows, you don't need to do it – just mingw32_HOST_OS will suffice. See this Trac ticket.


Detect architecture
other
 move item up  move item down  edit item info  delete item
Summary  edit summary
There are variables for detecting architecture:

i386_HOST_ARCH – x86
x86_64_HOST_ARCH – x64
arm_HOST_ARCH – ARM (there's also arm_HOST_ARCH_PRE_ARMv7)
powerpc_HOST_ARCH
sparc_HOST_ARCH

-}
