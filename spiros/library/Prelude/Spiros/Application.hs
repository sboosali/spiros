{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

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
import Prelude.Spiros.Reexports

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

import qualified "split" Data.List.Split as Split

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

import qualified "base" Data.Char as Char

--------------------------------------------------

import qualified "base" Prelude

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
* 'posixDirectory'     — .
* 'windowsDirectory'   — .
* 'macintoshDirectory' — .

== Example

e.g.:

@
myApplicationInformation :: ApplicationInformation
myApplicationInformation = 'ApplicationInformation'{..}
  where

  'name'                  = "My Application"
  'version'               = "0.0.0"
  'license'               = "GPL-3.0-or-later"
  'vendor'                = "sboosali.io"
  'executable'            = "my-application"

  'interface'             = 'ApplicationCLI'
  'platforms'             = 'allDesktopPlatforms'
                              -- [ 'DesktopLinux', 'DesktopWindows', 'DesktopMacintosh' ]

  'posixDirectory'     = "myapplication/"
  'windowsDirectory'   = "sboosali/My Application/"
  'macintoshDirectory' = "io.sboosali.My-Application/"
@

-}

data ApplicationInformation = ApplicationInformation

 { name       :: String
 , version    :: String
 , license    :: String

 , vendor     :: String
 , executable :: String

 , interface  :: ApplicationInterface
 , platforms  :: [DesktopPlatform]

 , posixDirectory     :: String
 , windowsDirectory   :: String
 , macintoshDirectory :: String
 }

#if HAS_EXTENSION_DerivingStrategies
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)
#else
  deriving (Show,Read,Eq,Ord,Generic)
#endif

--------------------------------------------------
--------------------------------------------------

{-| The primary fields of 'ApplicationInformation', from which the rest are derived (via 'defaultApplicationInformation').

Required fields share type with their namesake field (implicitly under @Identity@).
Optional fields are wrapped under @Maybe@.

-}

data ApplicationInformation0 = ApplicationInformation0

 { name0       :: String
 , version0    :: String
 , license0    :: String
 , vendor0     :: String

 , executable0            :: Maybe String
 , interface0             :: Maybe ApplicationInterface
 , platforms0             :: Maybe [DesktopPlatform]
 , posixDirectory0     :: Maybe String
 , windowsDirectory0   :: Maybe String
 , macintoshDirectory0 :: Maybe String
 }

#if HAS_EXTENSION_DerivingStrategies
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)
#else
  deriving (Show,Read,Eq,Ord,Generic)
#endif

--------------------------------------------------

-- | @= 'defaultApplicationInformation0'@

instance Default ApplicationInformation0 where
  def = defaultApplicationInformation0

--------------------------------------------------

{-| Default 'ApplicationInformation'.

May call:

* `asExecutableName`
* `allDesktopPlatforms`
* `asPosixDirectory`
* `windowsDirectory`
* `macintoshDirectory`

-}

defaultApplicationInformation :: ApplicationInformation0 -> ApplicationInformation
defaultApplicationInformation ApplicationInformation0{..} = ApplicationInformation{..}
  where
  
  name                  = name0
  version               = version0
  license               = license0
  vendor                = vendor0

  executable            = executable0 & fromMaybe (asExecutableName name)
  interface             = interface0  & fromMaybe def
  platforms             = platforms0  & fromMaybe allDesktopPlatforms

  posixDirectory     = posixDirectory0     & fromMaybe (asPosixDirectory     name vendor)
  windowsDirectory   = windowsDirectory0   & fromMaybe (asWindowsDirectory   name vendor)
  macintoshDirectory = macintoshDirectory0 & fromMaybe (asMacintoshDirectory name vendor)

--------------------------------------------------

-- | Required fields are @""@. Optional fields are @Nothing@.

defaultApplicationInformation0 :: ApplicationInformation0
defaultApplicationInformation0 = ApplicationInformation0{..}
 where

 name0    = ""
 version0 = ""
 license0 = ""
 vendor0  = ""

 executable0         = Nothing
 interface0          = Nothing
 platforms0          = Nothing
 posixDirectory0     = Nothing
 windowsDirectory0   = Nothing
 macintoshDirectory0 = Nothing

--------------------------------------------------
--------------------------------------------------

{-| Platform for desktop applications.

-}

data DesktopPlatform

  = DesktopLinux
  | DesktopWindows
  | DesktopMacintosh

#if HAS_EXTENSION_DerivingStrategies
  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)
#else
  deriving (Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic)
#endif

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

#if HAS_EXTENSION_DerivingStrategies
  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)
#else
  deriving (Enum,Bounded,Ix,Show,Read,Eq,Ord,Generic)
#endif

--------------------------------------------------

-- | @= 'ApplicationCLI'@

instance Default ApplicationInterface where
  def = ApplicationCLI

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

currentDesktopPlatform :: Maybe DesktopPlatform
currentDesktopPlatform = case currentOperatingSystem of

  Right Linux   -> Just DesktopLinux
  Right Windows -> Just DesktopWindows
  Right OSX     -> Just DesktopMacintosh
  _             -> Nothing

--TODO runtimeDesktopPlatform:
--     respect cross-compilation, i.e. TARGET_OS, not HOST_OS.

--------------------------------------------------

allDesktopPlatforms :: [DesktopPlatform]
allDesktopPlatforms = constructors'

--------------------------------------------------

posixDesktopPlatforms :: [DesktopPlatform]
posixDesktopPlatforms = [ DesktopLinux, DesktopMacintosh ]

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

    Just DesktopLinux     -> "XDG_RUNTIME_DIR"
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

  Just DesktopLinux     -> posixDirectory
  Just DesktopWindows   -> windowsDirectory
  Just DesktopMacintosh -> macintoshDirectory

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

{- | Construct a 'executable' from a 'name'.

== Examples

>>> asExecutableName "My Application"
"my-application"

-}

asExecutableName :: String -> String
asExecutableName name = go name
  where

  go
    = fmap Char.toLower
    > convertSpacesToHyphens

--------------------------------------------------
 
{- | Construct a 'posixDirectory' from a 'name'.

== Examples

>>> import qualified Prelude
>>> asPosixDirectory "My Application" Prelude.undefined
"myapplication/"

-}

asPosixDirectory :: String -> String -> String
asPosixDirectory name _vendor = path
  where

  path = go name

  go
    = fmap Char.toLower
    > filter (not . Char.isSpace)
    > (++ "/")

--------------------------------------------------

{- | Construct a 'windowsDirectory' from a 'vendor' and a 'name'.

== Examples

>>> asWindowsDirectory "My Application" "www.sboosali.com"
"sboosali/My Application/"

-}

asWindowsDirectory :: String -> String -> String
asWindowsDirectory name vendor = path
  where

  path = concat [ host, "/", name, "/" ]

  host = vendor &

    ( Split.splitOn "."
    > getSecondToLast
    > maybe vendor id
    )

  getSecondToLast :: [a] -> Maybe a
  getSecondToLast

    = reverse
    > nth 1

--------------------------------------------------

{- | Construct a 'macintoshDirectory' from a 'vendor' and a 'name'.

== Examples

>>> asMacintoshDirectory "My Application" "www.sboosali.com"
"com.sboosali.www.My-Application/"

-}

asMacintoshDirectory :: String -> String -> String
asMacintoshDirectory name vendor = path
  where

  path = concat [ vendor', ".", name', "/" ]

  name' = name
    & convertSpacesToHyphens

  vendor' = vendor
    & reverseByDot

  reverseByDot :: String -> String
  reverseByDot

    = Split.splitOn "."
    > reverse
    > intercalate "."

--------------------------------------------------

{- | Convert multiple whitespace characters to a single hyphen character.

== Examples

>>> convertSpacesToHyphens " phrase with     spaces "
"phrase-with-spaces"

-}

convertSpacesToHyphens :: String -> String
convertSpacesToHyphens

  = Split.split splitter
  > intercalate "-"

  where

  splitter :: Split.Splitter Char
  splitter = Split.whenElt Char.isSpace

    & ( Split.condense
      > Split.dropBlanks
      > Split.dropDelims
      )

  -- Split.defaultSplitter

  -- replace " " "-"

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
-- `split`

-- | Split on the given sublist.  Equivalent to @'split'
--   . 'dropDelims' . 'onSublist'@.  For example:
--
-- > splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
--
--   In some parsing combinator frameworks this is also known as
--   @sepBy@.
--
--   Note that this is the right inverse of the 'Data.List.intercalate' function
--   from "Data.List", that is,
--
--   > intercalate x . splitOn x === id
--
--   @'splitOn' x . 'Data.List.intercalate' x@ is the identity on
--   certain lists, but it is tricky to state the precise conditions
--   under which this holds.  (For example, it is not enough to say
--   that @x@ does not occur in any elements of the input list.
--   Working out why is left as an exercise for the reader.)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn   = split . dropDelims . onSublist

-- | Split on elements satisfying the given predicate.  Equivalent to
--   @'split' . 'dropDelims' . 'whenElt'@.  For example:
--
-- > splitWhen (<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = split . dropDelims . whenElt

--------------------------------------------------
-- ``



--------------------------------------------------
-- ``

--------------------------------------------------
-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------