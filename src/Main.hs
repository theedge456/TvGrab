-----------------------------------------------------------------------------
--
-- Module      :  TvGrab.Main
-- Copyright   :  Fabien R
-- License     :  BSD2
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main (
    main
) where

import Control.Monad (unless, when)
import System.Exit (die)
import Data.Time.Clock

import Options
import Configuration
import Programs
import Common (printDebug, epgDomainName, rootUrl)

-- constants
gridForChannel = rootUrl ++ "/programme-tv"
gridForBouquets = gridForChannel ++ "/bouquets"
gridByChannel = gridForChannel ++ "/grille-chaine/"
bouquets = [ "Grandes chaînes et TNT" , "Orange", "Free", "bouygues",
                 "sfr", "numericable", "canal",  "cable-adsl-satellite",
                 "canal-et-canalsat", "belgique", "hors-bouquet" ]
bouquet = "Free"

-- Main code
main = do
     startTime <- getCurrentTime
     opts <- parseTvOptions
     -- print $ "options=" ++ (show opts)
     -- Checks the offset option
     unless (getIsOffsetValid opts) $ die "Error: --offset must be less than --days"
     -- Configures if required
     unless (elem bouquet bouquets) $ die $ "Error: invalid bouquet " ++ bouquet
     when (getConfigure opts) $ configureTvGrab (getDebug opts) (getConfigFile opts)
                                    gridForBouquets bouquet
     grabPrograms (getDebug opts) (getConfigFile opts) (getOutputFile opts)
                    (getDays opts) (getOffset opts) gridByChannel
     endTime <- getCurrentTime
     print $ "elapsed time:" ++ show (diffUTCTime endTime startTime)

