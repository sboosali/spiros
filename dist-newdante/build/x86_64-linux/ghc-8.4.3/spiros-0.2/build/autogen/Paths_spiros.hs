{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_spiros (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sboo/.cabal/bin"
libdir     = "/home/sboo/.cabal/lib/x86_64-linux-ghc-8.4.3/spiros-0.2-inplace"
dynlibdir  = "/home/sboo/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/sboo/.cabal/share/x86_64-linux-ghc-8.4.3/spiros-0.2"
libexecdir = "/home/sboo/.cabal/libexec/x86_64-linux-ghc-8.4.3/spiros-0.2"
sysconfdir = "/home/sboo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spiros_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spiros_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "spiros_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "spiros_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spiros_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spiros_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
