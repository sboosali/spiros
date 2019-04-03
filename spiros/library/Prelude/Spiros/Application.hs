{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}

--------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------
--------------------------------------------------

{-| Utilities for application metadata and application-specific filepaths.

== API

* 'ApplicationInformation'
* 

== Links

* <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>
* <https://stackoverflow.com/questions/43853548/xdg-basedir-directories-for-windows>
* <https://wiki.archlinux.org/index.php/XDG_Base_Directory>

-}

module Prelude.Spiros.Application
  ( module Prelude.Spiros.Application
  -- , module Application.Info
  ) where

--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude.Spiros.Compatibility()
import Prelude.Spiros.Classes

import Prelude.Spiros.System
import Prelude.Spiros.Utilities

--------------------------------------------------
--------------------------------------------------

import qualified "filepath" System.FilePath as File
import           "filepath" System.FilePath ((</>))

--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "unix-compat" System.PosixCompat.Files as UNIX

--------------------------------------------------
--------------------------------------------------

--import qualified "text" Data.Text    as T
import qualified "text" Data.Text.IO as T

import           "text" Data.Text (Text)

--------------------------------------------------

--import qualified "base" Control.Exception   as E
--import qualified "base" System.IO.Error     as E
import qualified "base" System.Environment  as IO
--import qualified "base" System.Info         as Base
--import qualified "base" GHC.Conc            as GHC

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Metadata for a (cross-platform) application.

== Fields

Metadata:

* 'name'       — The name of the application. In english, should be capitalized and have spaces.
* 'version'    — The application's (current) version. Should be parseable by @base@'s 'Data.Version.parseVersion' (custom development versions, like injecting a git commit, should be separated from the version numbers by a hyphen).
* 'license'    — The application's license. Should be an SPDX License Identifier.
* 'executable' — The basename (or shell identifier) of the executable file. Should match the @executable@ stanza in the package's @.cabal@. For legibility and portability, words should be separated by hyphens (i.e. @-@) or underscores (i.e. @_@).
  'interface'  — Whether the application has a Command-Line Interface or a Graphical-User Interface.

Platform-specific metadata:

* 'platforms'             — Supported platforms (i.e. that the application is known to run on).
* 'posixSubDirectory'     — .
* 'windowsSubDirectory'   — .
* 'macintoshSubDirectory' — .

== Example

e.g.:

@
myApplicationInformation :: ApplicationInformation
myApplicationInformation = 'ApplicationInformation'{..}
  where

  'name'                  = "My Application"
  'version'               = "0.0.0"
  'license'               = "GPL-3.0-or-later"

  'executable'            = "my-application"
  'interface'             = 'ApplicationCLI'
  'platforms'             = [ 'DesktopPOSIX', 'DesktopMacintosh' ]

  'posixSubDirectory'     = "myapplication/"
  'windowsSubDirectory'   = "sboosali/My Application/"
  'macintoshSubDirectory' = "io.sboosali.My-Application/"
@

-}

data ApplicationInformation = ApplicationInformation

 { name       :: String
 , version    :: String
 , license    :: String

 , executable :: String
 , interface  :: ApplicationInterface
 , platforms  :: [DesktopPlatform]

 , posixSubDirectory     :: String
 , windowsSubDirectory   :: String
 , macintoshSubDirectory :: String
 }

--------------------------------------------------
--------------------------------------------------

{-| Platform for desktop applications.

-}

data DesktopPlatform

  = DesktopPOSIX
  | DesktopWindows
  | DesktopMacintosh

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| The kind of application interface.

Cases:

* 'ApplicationCLI' — the application has a Command-Line Interface.
* 'ApplicationGUI' — the application has a Graphical-User Interface.

-}

data ApplicationInterface

  = ApplicationCLI
  | ApplicationGUI

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

currentDesktopPlatform :: Maybe DesktopPlatform
currentDesktopPlatform = case currentOperatingSystem of

  Right Linux   -> Just DesktopPOSIX
  Right Windows -> Just DesktopWindows
  Right OSX     -> Just DesktopMacintosh
  _             -> Nothing

--TODO runtimeDesktopPlatform:
--     respect cross-compilation, i.e. TARGET_OS, not HOST_OS.

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------
-- Application Files...


{- Return a (platform-specific) **configuration file** for the application.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_CONFIG_DIR@ — on POSIX.
* @%APPDATA%@       — on Windows.

Wraps 'getApplicationSpecificConfigFile'.

-}

getApplicationSpecificConfigFile
  :: ApplicationInformation
  -> FilePath -> IO FilePath

getApplicationSpecificConfigFile application path = do

  directory <- getApplicationSpecificConfigurationDirectory application

  return (directory </> path)

--------------------------------------------------

{- Return a (platform-specific) **data file** for the application.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_DATA_DIR@  — on POSIX.
* @%APPDATA%@      — on Windows.

Wraps 'getApplicationSpecificDataFile'.

-}

getApplicationSpecificDataFile
  :: ApplicationInformation
  -> FilePath -> IO FilePath

getApplicationSpecificDataFile application path = do

  directory <- getApplicationSpecificDataDirectory application

  return (directory </> path)

--------------------------------------------------

{- Return a **cached file** for the running application.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_CACHE_DIR@ — on POSIX.
* @%LOCALAPPDATA%@ — on Windows.

Wraps 'getApplicationSpecificCacheFile'.

-}

getApplicationSpecificCacheFile
  :: ApplicationInformation
  -> FilePath -> IO FilePath

getApplicationSpecificCacheFile application path = do

  directory <- getApplicationSpecificCacheDirectory application

  return (directory </> path)

--------------------------------------------------

{- Return an **ephemeral file** (or pipe, socket, etc) for the running application.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_RUNTIME_DIR@ — on POSIX.
* @%TEMP%@           — on Windows.

Wraps 'getApplicationSpecificRuntimeFile'.

-}

getApplicationSpecificRuntimeFile
  :: ApplicationInformation
  -> FilePath -> IO FilePath

getApplicationSpecificRuntimeFile application path = do

  directory <- getApplicationSpecificRuntimeDirectory application

  return (directory </> path)

--------------------------------------------------
-- Application Directories...

{- Return the application's (platform-specific) **configuration directory**.

Output:

* is an absolute path
* should be a user-writeable directory.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_CONFIG_DIR@ — on POSIX.
* @%APPDATA%@       — on Windows.

Notes:

> 

-}

getApplicationSpecificConfigurationDirectory
  :: ApplicationInformation
  -> IO FilePath

getApplicationSpecificConfigurationDirectory application =

  getApplicationSpecificXdgDirectory Directory.XdgConfig application ""

--------------------------------------------------

{- Return the application's (platform-specific) **data directory**.

Output:

* is an absolute path
* should be a user-writeable directory.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_DATA_DIR@  — on POSIX.
* @%APPDATA%@      — on Windows.

Notes:

> 

-}

getApplicationSpecificDataDirectory
  :: ApplicationInformation
  -> IO FilePath

getApplicationSpecificDataDirectory application =

  getApplicationSpecificXdgDirectory Directory.XdgData application ""

--------------------------------------------------

{- Return the application's (platform-specific) **cache directory**.

Output:

* is an absolute path
* should be a user-writeable directory.

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_CACHE_DIR@ — on POSIX.
* @%LOCALAPPDATA%@ — on Windows.

Notes:

> 

-}

getApplicationSpecificCacheDirectory
  :: ApplicationInformation
  -> IO FilePath

getApplicationSpecificCacheDirectory application =

  getApplicationSpecificXdgDirectory Directory.XdgCache application ""

--------------------------------------------------

{- Return the application's (platform-specific) **runtime directory**.

Output:

* is an absolute path
* should be a user-writeable directory.
* should be \"private\" (**not** group-readable or word-readable).

Conforms to the @XDG BaseDir@s specification.

Environment Variables:

* @$XDG_RUNTIME_DIR@ — on POSIX.
* @%TEMP%@           — on Windows.

Notes:

> The runtime dir should be used for storing ephemeral things like pipes and sockets or other objects restricted to the current run of the program. It is likely created when a user logs in and deleted when the user logs out. It probably lives in a RAM-based file system. 

-}

getApplicationSpecificRuntimeDirectory
  :: ApplicationInformation
  -> IO FilePath

getApplicationSpecificRuntimeDirectory application = wrap $ do

  let getDefaultPath = Directory.getHomeDirectory

  runtimeDirectory <- getPathFromEnvironmentOrDefault getDefaultPath environmentVariable

  let subdirectory = currentApplicationSpecificSubDirectory application
  let directory = runtimeDirectory </> subdirectory

  absolutePath <- Directory.makeAbsolute directory

  return absolutePath

  where

  wrap action = {-(`E.ioeAddLocation` "getXdgDirectory") `E.modifyIOError`-} action

  environmentVariable :: String
  environmentVariable = case currentDesktopPlatform of

    Just DesktopPOSIX     -> "XDG_RUNTIME_DIR"
    Just DesktopWindows   -> "TEMP"
    Just DesktopMacintosh -> "TEMP" --TODO-- "TEMP" or "XDG_RUNTIME_DIR"?

    Nothing -> ""

--------------------------------------------------
-- File Readers...

readApplicationSpecificConfigFile :: ApplicationInformation -> FilePath -> IO (Maybe Text)
readApplicationSpecificConfigFile application relativePath = readTextFileWith getFilePath
  where

  getFilePath = getApplicationSpecificConfigFile application relativePath

--------------------------------------------------

readApplicationSpecificDataFile :: ApplicationInformation -> FilePath -> IO (Maybe Text)
readApplicationSpecificDataFile application relativePath = readTextFileWith getFilePath
  where

  getFilePath = getApplicationSpecificDataFile application relativePath

--------------------------------------------------

readApplicationSpecificCacheFile :: ApplicationInformation -> FilePath -> IO (Maybe Text)
readApplicationSpecificCacheFile application relativePath = readTextFileWith getFilePath
  where

  getFilePath = getApplicationSpecificCacheFile application relativePath

--------------------------------------------------
-- File Writers..

writeApplicationSpecificConfigFile :: ApplicationInformation -> FilePath -> Text -> IO ()
writeApplicationSpecificConfigFile application relativePath fileContents =

  writeTextFileWith getFilePath fileContents

  where

  getFilePath = getApplicationSpecificConfigFile application relativePath

--------------------------------------------------

writeApplicationSpecificDataFile :: ApplicationInformation -> FilePath -> Text -> IO ()
writeApplicationSpecificDataFile application relativePath fileContents =

  writeTextFileWith getFilePath fileContents

  where

  getFilePath = getApplicationSpecificDataFile application relativePath

--------------------------------------------------

writeApplicationSpecificCacheFile :: ApplicationInformation -> FilePath -> Text -> IO ()
writeApplicationSpecificCacheFile application relativePath fileContents =

  writeTextFileWith getFilePath fileContents

  where

  getFilePath = getApplicationSpecificCacheFile application relativePath

--------------------------------------------------
-- Custom File Operations...

listApplicationSpecificDataFiles :: ApplicationInformation -> IO [FilePath]
listApplicationSpecificDataFiles application = do

  absolutePath <- getApplicationSpecificCacheDirectory application

  exists <- Directory.doesDirectoryExist absolutePath
  if exists

  then do
    filePaths <- Directory.listDirectory absolutePath
    return filePaths
  -- TODO list files recursively

  else do
    return []
    
--------------------------------------------------

removeApplicationSpecificCache :: ApplicationInformation -> IO FilePath
removeApplicationSpecificCache application = do

  absolutePath <- getApplicationSpecificCacheDirectory application

  whenM (Directory.doesDirectoryExist absolutePath) $ do
    Directory.removeDirectoryRecursive absolutePath

  whenM (Directory.doesFileExist absolutePath) $ do
    Directory.removeFile absolutePath

  return absolutePath

--------------------------------------------------

touchApplicationSpecificRuntimeFile :: ApplicationInformation -> FilePath -> IO Bool
touchApplicationSpecificRuntimeFile application relativePath = do

  absolutePath <- getApplicationSpecificRuntimeFile application relativePath

  exists <- Directory.doesFileExist absolutePath
  if exists

  then do
    UNIX.touchFile absolutePath
    return False
    -- "no", already created (just touch it, to keep it alive).

  else do
    let directoryPath = File.takeDirectory absolutePath
    Directory.createDirectoryIfMissing True directoryPath

    UNIX.touchFile absolutePath
    return True
    -- "yes", newly created.

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--createDirectoryIfMissing

{- |

the input @FilePath@ __must__ be /strictly relative/. i.e.:

* no @../@
* 

-}

getApplicationSpecificXdgDirectory
  :: Directory.XdgDirectory -> ApplicationInformation -> FilePath -> IO FilePath

getApplicationSpecificXdgDirectory xdgDirectoryKind application path = do

  let subdirectory = currentApplicationSpecificSubDirectory application
  let relativePath = subdirectory File.</> path

  xdgPath      <- Directory.getXdgDirectory xdgDirectoryKind relativePath
  absolutePath <- Directory.makeAbsolute xdgPath

  return absolutePath

--TODO validation:
--     fail (either throw or clean) on non-true-relative subdirectories (i.e. no « ../ »).

--------------------------------------------------

currentApplicationSpecificSubDirectory :: ApplicationInformation -> FilePath
currentApplicationSpecificSubDirectory ApplicationInformation{..} = case currentDesktopPlatform of

  Just DesktopPOSIX     -> posixSubDirectory
  Just DesktopWindows   -> windowsSubDirectory
  Just DesktopMacintosh -> macintoshSubDirectory

  Nothing -> executable

--------------------------------------------------

readTextFileWith :: IO FilePath -> IO (Maybe Text)
readTextFileWith getFilePath = do

  absolutePath <- getFilePath

  exists <- Directory.doesFileExist absolutePath
  if exists

  then do
    fileContents <- T.readFile absolutePath
    return (Just fileContents)

  else do
    return Nothing

--------------------------------------------------

{-|

Creates any (non-existant) parent directories
(in Bash, like a @mkdir -p@ on the @dirname@).

-}

writeTextFileWith :: IO FilePath -> Text -> IO ()
writeTextFileWith getFilePath fileContents = do

  absolutePath <- getFilePath

  let directoryPath = File.takeDirectory absolutePath
  Directory.createDirectoryIfMissing True directoryPath

  T.writeFile absolutePath fileContents

--if Directory.doesFileExist absolutePath

--------------------------------------------------

getPathFromEnvironmentOrDefault :: IO FilePath -> String -> IO FilePath
getPathFromEnvironmentOrDefault getDefaultPath variable = do

  valueDynamicPath <- go

  case valueDynamicPath of

    Nothing -> do
      defaultPath <- getDefaultPath
      pure defaultPath

    Just dynamicPath -> pure dynamicPath

  where

  go =

    if   isValidEnvironmentVariableName variable
    then IO.lookupEnv variable
    else pure Nothing

--------------------------------------------------

isValidEnvironmentVariableName :: String -> Bool
isValidEnvironmentVariableName s =

  s == ""
    
--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

--------------------------------------------------
`turtle`

touch :: MonadIO io => FilePath -> io () 

-- Touch a file, updating the access and modification times to the current time
--
-- Creates an empty file if it does not exist

  touch file = do

      exists <- testfile file

      liftIO (if exists

  #ifdef mingw32_HOST_OS
          then do
              handle <- Win32.createFile
                  (Filesystem.encodeString file)
                  Win32.gENERIC_WRITE
                  Win32.fILE_SHARE_NONE
                  Nothing
                  Win32.oPEN_EXISTING
                  Win32.fILE_ATTRIBUTE_NORMAL
                  Nothing
              (creationTime, _, _) <- Win32.getFileTime handle
              systemTime <- Win32.getSystemTimeAsFileTime
              Win32.setFileTime handle creationTime systemTime systemTime

  #else
          then touchFile (Filesystem.encodeString file)

  #endif
          else output file empty )

--------------------------------------------------
`system-filepath`

import qualified Filesystem.Path.CurrentOS as Filesystem

--------------------------------------------------
`unix`

import System.Posix ( touchFile )

touchFile :: FilePath -> IO ()

-- | @touchFile path@ sets the access and modification times associated with
-- file @path@ to the current time.
--
-- Note: calls @utime@.

  touchFile name = do
    withFilePath name $ \s ->
     throwErrnoPathIfMinus1_ "touchFile" name (c_utime s nullPtr)

--------------------------------------------------
`unix-compat`

touchFile :: FilePath -> IO ()

-- touchFile path sets the access and modification times associated with file path to the current time.
--
-- Note: calls utime.

  touchFile 

--------------------------------------------------
-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------