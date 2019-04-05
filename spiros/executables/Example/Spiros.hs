{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------

{-|
-}

module Example.Spiros where

#include "cabal_macros.h"

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
application = defaultApplicationInformation ApplicationInformation0{..}
  where

  name0                  = "My Application"
  license0               = "Apache-2.0"

#ifdef CURRENT_PACKAGE_VERSION
  version0               = "#CURRENT_PACKAGE_VERSION"
#else
  version0               = "0.3"
#endif

  executable0            = Just "my-application"
  interface0             = Just ApplicationCLI
  platforms0             = Just allDesktopPlatforms

  -- posixSubDirectory     = "myapplication/"
  -- windowsSubDirectory   = "sboosali/My Application/"
  -- macintoshSubDirectory = "io.sboosali.My-Application/"

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