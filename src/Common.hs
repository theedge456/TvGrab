-----------------------------------------------------------------------------
--
-- Module      :  TvGrab.Common
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

module Common (
    epgDomainName,
    rootUrl,
    language,
    safeHead,
    safeTail,
    safeHeadTail,
    printDebug,
    terminate
) where

import System.Exit(exitSuccess)

-- | constants
epgDomainName = "telestar.fr"
!rootUrl = "https://www." ++ epgDomainName
language = "fr"

-- | List utility functions
safeHead :: [a] -> Maybe a
safeHead l = case l of
                [] -> Nothing
                x:xs -> Just x

safeTail :: [a] -> Maybe [a]
safeTail l = case l of
                [] -> Nothing
                x:xs -> Just xs
-- | Applies safeTail i times before calling safeHead
safeHeadTail :: Int -> Maybe [a] -> [a] -> Maybe a
safeHeadTail i res l = if i == 0
                       then case res of
                             Nothing -> Nothing
                             Just xs -> safeHead xs
                       else case l of
                             [] -> Nothing
                             x:xs -> safeHeadTail (i-1) (safeTail l) xs
--
-- | debug output
--
printDebug :: Show a => Bool -> a -> IO ()
printDebug b msg = if b then print msg else return ()
--
-- | Write given message to `stdout` and terminate with `exitSuccess`.
--
terminate :: String -> IO a
terminate msg = putStrLn msg >> exitSuccess

