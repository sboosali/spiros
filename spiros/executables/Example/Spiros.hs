{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------

{-|
-}

module Example.Spiros where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Prelude.Spiros
import Prelude.Spiros.Application

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative as P

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

data Options = Options

  { shouldPrintVersion :: Bool
  , shouldPrintLicense :: Bool
  }

  deriving (Show)

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

application :: ApplicationInformation
application = ApplicationInformation{..}
  where

  name                  = "My Application"
  version               = "0.3.1"
  license               = "Apache-2.0"

  executable            = "my-application"
  interface             = ApplicationCLI
  platforms             = allDesktopPlatforms

  posixSubDirectory     = "myapplication/"
  windowsSubDirectory   = "sboosali/My Application/"
  macintoshSubDirectory = "io.sboosali.My-Application/"

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main :: IO ()
main = do

  options <- getOptions

  mainWith options

--------------------------------------------------

mainWith :: Options -> IO ()
mainWith Options{..} = do

  let action = headDef nothing actions

  action

  where

  actions :: [IO ()]
  actions = concat

    [ (if shouldPrintVersion then [printVersion] else [])
    , (if shouldPrintLicense then [printLicense] else [])
    ]

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

printVersion :: IO ()
printVersion = do

  putStrLn (application & version)

--------------------------------------------------

printLicense :: IO ()
printLicense = do

  putStrLn (application & license)

--------------------------------------------------
-- CLI -------------------------------------------
--------------------------------------------------

{-|

Options, Arguments, and Flags include:

* @--version@

-}

options :: P.Parser Options
options = do

  shouldPrintVersion <- P.switch (mconcat

        [ P.long    "version"
        , P.help    "Print the version of this program. The format is « x.y.z ». (No other text is printed. If both « --version » and « --license » are specified, all options besides « --version » are ignored.)"
        ])

  shouldPrintLicense <- P.switch (mconcat

        [ P.long    "license"
        , P.help    "Print the SPDX License Identifier of this program. The format is « [-.a-z0-9A-z]+ ». (No other text is printed.)"
        ])

  return Options{..}

--------------------------------------------------

getOptions :: IO Options
getOptions = do

  options <- P.customExecParser preferences parser
  
  return options

--------------------------------------------------

parser :: P.ParserInfo Options
parser = P.info (options <**> P.helper) information

--------------------------------------------------

information :: P.InfoMod a
information = mconcat

  [ P.fullDesc
  , P.failureCode 2 -- exit code — when a parse error occurs.
  , P.headerDoc $ Just  "----------------------------------------"
  , P.footerDoc $ Just  "----------------------------------------"
  ]

--------------------------------------------------

preferences :: P.ParserPrefs
preferences = P.prefs (mconcat

  [ P.disambiguate
  , P.showHelpOnEmpty
--, P.showHelpOnError
  ])

--------------------------------------------------
-- Paths -----------------------------------------
--------------------------------------------------



--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------