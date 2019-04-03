{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

--------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------
--------------------------------------------------

{-| Information about the current system:

* operating system — 'currentOperatingSystem'.
* architecture — 'currentArchitecture'.
* endianness — 'currentEndianness'.
* processor — 'currentProcessorBits', 'currentNumberOfCPUs'.

**TODO: respect cross-compilation, i.e. the target/runtime system.**

And information about the current compiler:

* haskell compiler — 'currentCompiler'.

(This module is similar to the @Foundation.System@ module in the @foundation@ package.)

-}

module Prelude.Spiros.System
  ( module Prelude.Spiros.System
  -- , module System.Info
  ) where

--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude.Spiros.Compatibility()

import Prelude.Spiros.Reexports
import Prelude.Spiros.Utilities

--------------------------------------------------
--------------------------------------------------

import qualified "cpuinfo" System.CPU as CPU

--------------------------------------------------

import qualified "base" System.Info as Base
import qualified "base" GHC.Conc as GHC

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

-- | Enumeration of the known GHC supported operating systems.
--

data KnownOperatingSystem

  = Linux
  | Windows
  | OSX
  | Android
  | BSD

  deriving ( Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

-- | get the operating system on which the program is running.
--
-- Either return the known `OS` or a strict `String` of the OS name.
--
-- This function uses the `base`'s `System.Info.os` function.
--

currentOperatingSystem :: Either String KnownOperatingSystem
currentOperatingSystem = case Base.os of

    "linux"         -> Right Linux --TODO-- more linux strings.
    "mingw32"       -> Right Windows
    "mingw64"       -> Right Windows
    "darwin"        -> Right OSX
    "linux-android" -> Right Android
    "openbsd"       -> Right BSD
    "netbsd"        -> Right BSD
    "freebsd"       -> Right BSD

    s               -> Left s

--------------------------------------------------
--------------------------------------------------

-- | Enumeration of the known GHC supported architecture.
--

data KnownArchitecture = KnownArchitecture

  { architectureManufacturer  :: KnownManufacturer
  , architectureProcessorBits :: ProcessorBits -- vs « Maybe ProcessorBits »
  } 

  deriving ( Show,Read,Eq,Ord,Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

-- | @'IntelManufacturer'@ and @'Processor64Bit'@.

instance Default KnownArchitecture where

  def = KnownArchitecture{..}
    where

    architectureManufacturer  = def
    architectureProcessorBits = def

--------------------------------------------------

pattern I386      :: KnownArchitecture
pattern I386      = KnownArchitecture { architectureManufacturer = Intel_Manufacturer, architectureProcessorBits = Processor32Bit }

pattern X86_64    :: KnownArchitecture
pattern X86_64    = KnownArchitecture { architectureManufacturer = Intel_Manufacturer, architectureProcessorBits = Processor64Bit }

pattern PowerPC   :: KnownArchitecture
pattern PowerPC   = KnownArchitecture { architectureManufacturer = PowerPC_Manufacturer, architectureProcessorBits = Processor32Bit }

pattern PowerPC64 :: KnownArchitecture
pattern PowerPC64 = KnownArchitecture { architectureManufacturer = PowerPC_Manufacturer, architectureProcessorBits = Processor64Bit }

pattern Sparc     :: KnownArchitecture
pattern Sparc     = KnownArchitecture { architectureManufacturer = Sparc_Manufacturer, architectureProcessorBits = Processor32Bit }

pattern Sparc64   :: KnownArchitecture
pattern Sparc64   = KnownArchitecture { architectureManufacturer = Sparc_Manufacturer, architectureProcessorBits = Processor64Bit }

pattern ARM       :: KnownArchitecture
pattern ARM       = KnownArchitecture { architectureManufacturer = ARM_Manufacturer, architectureProcessorBits = Processor32Bit }

pattern ARM64     :: KnownArchitecture
pattern ARM64     = KnownArchitecture { architectureManufacturer = ARM_Manufacturer, architectureProcessorBits = Processor64Bit }

--------------------------------------------------

allKnownArchitectures :: [KnownArchitecture]
allKnownArchitectures = do

  architectureManufacturer  <- constructors'
  architectureProcessorBits <- constructors'

  return KnownArchitecture{..}

--------------------------------------------------

-- | Get the machine architecture on which the program is running.
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

--------------------------------------------------
--------------------------------------------------

data KnownManufacturer

  = Intel_Manufacturer
  | PowerPC_Manufacturer
  | Sparc_Manufacturer
  | ARM_Manufacturer

  deriving ( Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

-- | @≡ 'Intel_Manufacturer'@

instance Default KnownManufacturer where

  def = Intel_Manufacturer

--------------------------------------------------

-- | Get the manufacturer (if known) of the architecture on which the program is running.
--
-- Uses `base`'s `System.Info.arch` function.
--

currentManufacturer :: Maybe KnownManufacturer
currentManufacturer = case currentArchitecture of

  Left _ -> Nothing
  Right KnownArchitecture{ architectureManufacturer } -> Just architectureManufacturer

--------------------------------------------------
--------------------------------------------------

{-| Whether the processor is little-endian or big-endian.

<https://en.wikipedia.org/wiki/Endianness>: "Endianness is the sequential order in which bytes are arranged into larger numerical values when stored in memory or when transmitted over digital links."

-}

data Endianness

  = LittleEndian
  | BigEndian

  deriving ( Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

-- | @≡ 'LittleEndian'@

instance Default Endianness where

  def = LittleEndian

--------------------------------------------------

{- | The endianness of the current machine's architecture.

@Nothing@ represents:

* unknown endianness.

These endiannesses aren't represented:

* Bi-endianness. (the endianness, if swapped before the haskell program starts up, may differ).

For example, PowerPC processors start in big-endian, but PowerPC itself is bi-endian.

-}

currentEndianness :: Maybe Endianness
currentEndianness = case currentArchitecture of

  Left  _ -> Nothing

  Right KnownArchitecture{ architectureManufacturer } -> case architectureManufacturer of

      Intel_Manufacturer   -> Just LittleEndian
      PowerPC_Manufacturer -> Just BigEndian
      Sparc_Manufacturer   -> Just BigEndian
      ARM_Manufacturer     -> Just BigEndian

--------------------------------------------------
--------------------------------------------------

{- | Whether the processor is @64-bit@ or @32-bit@.

<https://en.wikipedia.org/wiki/64-bit_computing>: "In computer architecture, 64-bit computing is the use of processors that have datapath widths, integer size, and memory address widths of 64 bits (eight octets)."

-}

data ProcessorBits

  = Processor32Bit
  | Processor64Bit

  deriving ( Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

-- | @≡ 'Processor64Bit'@

instance Default ProcessorBits where

  def = Processor64Bit

--------------------------------------------------

-- | Get the number of bits (if known) of the processor on which the program is running.
--
-- Uses `base`'s `System.Info.arch` function.
--

currentProcessorBits :: Maybe ProcessorBits
currentProcessorBits = case currentArchitecture of

  Left _ -> Nothing
  Right KnownArchitecture{ architectureProcessorBits } -> Just architectureProcessorBits

--------------------------------------------------
--------------------------------------------------

-- | Enumeration of the known GHC-based compilers. 
--

data KnownHaskellCompiler

  = GHC     -- ^ C @FFI@.
  | GHCJS   -- ^ Javascript  @FFI@.
  | GHCETA  -- ^ Java @FFI@.

  deriving ( Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

-- | get the compiler name
--
-- This function uses the `base`'s `System.Info.compilerName` function.
--

currentCompiler :: Either String KnownHaskellCompiler
currentCompiler = case Base.compilerName of

    "ghc"    -> Right GHC
    "ghcjs"  -> Right GHCJS
    "eta"    -> Right GHCETA

    s        -> Left s

--------------------------------------------------

-- | @≡ 'GHC'@

instance Default KnownHaskellCompiler where

  def = GHC
  
--------------------------------------------------
--------------------------------------------------

data CPUsSummary = CPUsSummary

  { isHyperthreading :: IsHyperthreading
  , physicalCores    :: Natural
  , logicalCores     :: Natural
  } 

  deriving ( Show,Read,Eq,Ord,Generic --TODO CPP for Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

--------------------------------------------------

instance Default CPUsSummary where

  def = CPUsSummary{..}
    where

    isHyperthreading = def
    physicalCores    = 0
    logicalCores     = 0

--------------------------------------------------
--------------------------------------------------

{-| Whether the system is currently using any Hyperthreading.

-}

data IsHyperthreading

  = HyperthreadingIsDisabled
  | HyperthreadingIsEnabled

  deriving ( Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic --TODO CPP for Generic
#if HAS_EXTENSION_DeriveAnyClass
           , NFData, Hashable 
#endif
#if HAS_EXTENSION_DerivingLift 
           , Lift
#endif
           )

  -- deriving stock    (Enum,Bounded,Ix)
  -- deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  -- deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'HyperthreadingIsDisabled'@

instance Default IsHyperthreading where

  def = HyperthreadingIsDisabled

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | returns the number of CPUs the machine has
currentNumberOfCPUs :: IO Natural
currentNumberOfCPUs = unsafeNatural <$> GHC.getNumProcessors 

--------------------------------------------------

getCPUsVerbose :: IO [CPU.CPU]

getCPUsVerbose = do
  allCpuInfo <- CPU.tryGetCPUs
  return $ (allCpuInfo & maybe [] id)

--------------------------------------------------

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

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

{- 

--------------------------------------------------

TODO port « nixpkgs.platforms.* »

e.g.:

  nix-repl> :p pkgs.platforms
  
  {
    aarch64       = [ { cpu = { bits = 64; family = "arm"; }; } ];
  
    all           = [ { } ];
  
    arm           = [ { cpu = { bits = 32; family = "arm"; }; } ];
  
    cygwin        = [ { abi = { _type = "abi"; name = "cygnus"; }; kernel = { _type = "kernel"; execFormat = { _type = "exec-format"; name = "pe"; }; families = { }; name = "windows"; }; } ];
  
    darwin        = [ { kernel = { families = { darwin = { _type = "exec-format"; name = "darwin"; }; }; }; } ];
  
    freebsd       = [ { kernel = { _type = "kernel"; execFormat = { _type = "exec-format"; name = "elf"; }; families = { bsd = { _type = "exec-format"; name = "bsd"; }; }; name = "freebsd"; }; } ];
  
    gnu           = [ { abi = { _type = "abi"; name = "gnu"; }; kernel = { _type = "kernel"; execFormat = { _type = "exec-format"; name = "elf"; }; families = { }; name = "linux"; }; } { abi = { _type = "abi"; float = "soft"; name = "gnueabi"; }; kernel = { _type = "kernel"; execFormat = «repeated»; families = «repeated»; name = "linux"; }; } { abi = { _type = "abi"; float = "hard"; name = "gnueabihf"; }; kernel = { _type = "kernel"; execFormat = «repeated»; families = «repeated»; name = "linux"; }; } ];
  
    i686          = [ { cpu = { _type = "cpu-type"; bits = 32; family = "x86"; name = "i686"; significantByte = { _type = "significant-byte"; name = "littleEndian"; }; }; } ];
  
    illumos       = [ { kernel = { _type = "kernel"; execFormat = «repeated»; families = { }; name = "solaris"; }; } ];
  
    linux         = [ { kernel = { _type = "kernel"; execFormat = «repeated»; families = { }; name = "linux"; }; } ];
  
    mesaPlatforms = [ "i686-linux" "x86_64-linux" "x86_64-darwin" "armv5tel-linux" "armv6l-linux" "armv7l-linux" "aarch64-linux" "powerpc64le-linux" ];
  
    mips          = [ { cpu = { family = "mips"; }; } ];
  
    netbsd        = [ { kernel = { _type = "kernel"; execFormat = «repeated»; families = { bsd = «repeated»; }; name = "netbsd"; }; } ];
  
    none          = [ ];
  
    openbsd       = [ { kernel = { _type = "kernel"; execFormat = «repeated»; families = { bsd = «repeated»; }; name = "openbsd"; }; } ];
  
    riscv         = [ { cpu = { family = "riscv"; }; } ];
  
    unix          = [ { kernel = { families = { bsd = { _type = "exec-format"; name = "bsd"; }; }; }; } { kernel = «repeated»; } { kernel = «repeated»; } { kernel = «repeated»; } { abi = «repeated»; kernel = «repeated»; } ];
  
    windows       = [ { kernel = { _type = "kernel"; execFormat = «repeated»; families = «repeated»; name = "windows"; }; } ];
  
    x86           = [ { cpu = { family = "x86"; }; } ];
  
    x86_64        = [ { cpu = { bits = 64; family = "x86"; }; } ];

  }


--------------------------------------------------

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