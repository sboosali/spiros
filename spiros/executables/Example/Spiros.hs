{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
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
  }

  deriving (Show)

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

  if   shouldPrintVersion
  then printVersion
  else nothing

--------------------------------------------------

printVersion :: IO ()
printVersion = do

  putStrLn "0.3.1"

--------------------------------------------------
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
  , P.showHelpOnError
  , P.showHelpOnEmpty
  ])

--------------------------------------------------
-- CLI -------------------------------------------
--------------------------------------------------

{-|

Options, Arguments, and Flags include:

* @--version@

-}

options :: P.Parser Options
options = do

  shouldPrintVersion <- (P.switch (mconcat

        [ P.long    "version"
        , P.help    "Print the version of this program. The format is « x.y.z ». (No other text is printed.)"
        ]))

  return Options{..}

--------------------------------------------------
