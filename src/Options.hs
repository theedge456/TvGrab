-----------------------------------------------------------------------------
--
-- Module      :  TvGrab.Options
-- Copyright   :
-- License     :  BSD2
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE Arrows #-}
module Options (
    parseTvOptions
    , getConfigure
    , getConfigFile
    , getOutputFile
    , getDays
    , getOffset
    , getDebug
    , getIsOffsetValid
    , TvOptDays
) where

import Options.Applicative
import Data.Semigroup hiding (option)

import Options.Applicative.Arrows

type TvOptDays = Int

data TvOptions = TvOptions
  {
   configure        :: Bool,
   configFile      :: String,
   outputFile      :: String,
   days             :: TvOptDays,
   offset           :: TvOptDays,
   quiet            :: Bool,
   debug            :: Bool,
   isOffsetValid    :: Bool     -- internal use to check that offset < days
  }
  deriving Show

optConfigure :: Parser Bool
optConfigure = switch
  (  long "configure"
    <> help "Choose which bouquets/channels to grab listings data for"
  )

optConfigFile :: Parser String
optConfigFile = strOption
  (  long "config-file"
  <> metavar "FILE"
  <> value "tv_grab_fr.conf"
  <> help "Use FILE as config file instead of the default config file\n\
           \ name. This allows for different config files for different applications"
  )
optOutputFile :: Parser String
optOutputFile = strOption
  (  long "output-file"
  <> metavar "FILE"
  <> value "stdout"
  <> help "Write to FILE rather than standard output"
  )
optDays :: Parser TvOptDays
optDays = option parseOptDays
  (  long "days"
    <> metavar "N"
    <> value 7
    <> help "Grab N days (default 7, maximum 14) starting from today"
  )
parseDays :: String -> Either String TvOptDays
parseDays s = let n = read s in
                if (n > 0) && (n < 15) then Right n
                else Left $ "invalid value " ++ show n

parseOptDays :: ReadM TvOptDays
parseOptDays = eitherReader $ \s -> parseDays s

optOffset :: Parser TvOptDays
optOffset = option parseOptDays
  (  long "offset"
    <> metavar "N"
    <> value 0
    <> help "Start grabbing N days from today, rather than starting today"
  )
optQuiet :: Parser Bool
optQuiet = switch
  (  long "quiet"
    <> help "Suppress the progress messages normally written to standard error"
  )
optDebug :: Parser Bool
optDebug = switch
  (  long "debug"
    <> help "Provide additional debugging messages during processing"
  )
optIsOffsetValid :: Parser Bool
optIsOffsetValid = switch
  (  long "isOffsetValid"
    <> help "Internal use"
    <> hidden
  )

tvOpts :: Parser TvOptions
tvOpts = runA $ proc () -> do
      configure <- asA optConfigure -< ()
      configFile <- asA optConfigFile -< ()
      outputFile  <- asA optOutputFile -< ()
      quiet <- asA optQuiet -< ()
      debug <- asA optDebug -< ()
      days <- asA optDays -< ()
      offset <- asA optOffset -< ()
      returnA -< TvOptions configure configFile outputFile days offset quiet debug
                            (offset < days)

getConfigure :: TvOptions -> Bool
getConfigure = configure

getConfigFile :: TvOptions -> String
getConfigFile = configFile

getOutputFile :: TvOptions -> String
getOutputFile = outputFile

getDays :: TvOptions -> TvOptDays
getDays = days

getOffset :: TvOptions -> TvOptDays
getOffset = offset

getDebug :: TvOptions -> Bool
getDebug = debug

getIsOffsetValid :: TvOptions -> Bool
getIsOffsetValid = isOffsetValid

parseTvOptions :: IO TvOptions
parseTvOptions = execParser opts
               where opts = info ( helper <*> tvOpts)
                              ( fullDesc
                               <> header "tv_grab_fr - Grab TV listings for France."
                               <> progDesc ""
                               )
