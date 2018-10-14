{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

{-|

(this module is similar to @Foundation.System@). 

-}

module Prelude.Spiros.System
  ( module Prelude.Spiros.System
  -- , module System.Info
  ) where

----------------------------------------
----------------------------------------

import qualified "cpuinfo" System.CPU as CPU

----------------------------------------

import         Prelude.Spiros.Reexports
import         Prelude.Spiros.Utilities

----------------------------------------

import qualified "base" System.Info as Base
import qualified "base" GHC.Conc as GHC

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

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

getCPUsVerbose :: IO [CPU.CPU]

getCPUsVerbose = do
  allCpuInfo <- CPU.tryGetCPUs
  return $ (allCpuInfo & maybe [] id)

----------------------------------------

getCPUsSummary :: IO CPUsSummary

getCPUsSummary = do
  cpus <- getCPUsVerbose
  go cpus

  where
  go []   = return def
  go cpus = do

    let isHyperthreading = if CPU.hyperthreadingInUse cpus
          then HyperthreadingIsEnabled
          else HyperthreadingIsDisabled
  
    let physicalCores = CPU.physicalCores cpus & fromIntegral
  
    let logicalCores  = CPU.logicalCores  cpus & fromIntegral
  
    return CPUsSummary{..}
  
----------------------------------------

data CPUsSummary = CPUsSummary

  { isHyperthreading :: IsHyperthreading
  , physicalCores    :: Natural
  , logicalCores     :: Natural
  } 

  deriving (Show,Read,Eq,Ord,Lift,Generic,NFData,Hashable)

instance Default CPUsSummary where
  def = CPUsSummary{..}
    where
    isHyperthreading = def
    physicalCores    = 0
    logicalCores     = 0

----------------------------------------

{-| Whether the system is currently using any Hyperthreading.

-}

data IsHyperthreading

  = HyperthreadingIsDisabled
  | HyperthreadingIsEnabled

  deriving (Enum,Bounded,Ix,Show,Read,Eq,Ord,Lift,Generic,NFData,Hashable)

  -- deriving stock    (Enum,Bounded,Ix)
  -- deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  -- deriving anyclass (NFData,Hashable)

instance Default IsHyperthreading where
  def = HyperthreadingIsDisabled

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{- NOTES







physicalProcessors :: [CPU] -> Int

Counts the number of physical processors in the system. A physical processor corresponds to a single CPU unit in a single socket, i.e. unless you have a multi-socket motherboard, this number will be one.


physicalCores :: [CPU] -> Int

Counts the number of physical cores in the system. A physical core is an independent processing unit that reads and executes instructions on its own, but potentially shares its die (and other resources) with other cores.


logicalCores :: [CPU] -> Int

Counts the number of logical cores in the system. A logical core is a virtual processing unit exposed to the operating system, that may or may not directly correspond with an independent physical processing unit, e.g. a hyperthread appears as an independent processing unit to the operating system, but has no physically dedicated execution resources.


hyperthreadingFactor :: [CPU] -> Rational

The hyperthreading factor is the number of logical cores divided by the number of physical cores. This quantity indicates the degree to which physical execution resources are shared among logical processors, and may be used to tune parallel applications.


hyperthreadingInUse :: [CPU] -> Bool

If hyperthreading is in use, the hyperthreadingFactor will be greater than 1.










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
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------