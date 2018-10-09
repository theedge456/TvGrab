-----------------------------------------------------------------------------
--
-- Module      :  TvGrab.Configuration
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
{-# LANGUAGE BangPatterns #-}
module Configuration (
    configureTvGrab
) where

import qualified Data.ByteString.Lazy.UTF8 as U8
import           Network.HTTP.Client hiding (IsString)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status (statusCode, statusIsSuccessful)
import           Control.Exception          (try)
import           Text.HTML.TagSoup
import           Data.List.Split

import          Common (printDebug, terminate)

--
-- | Grabs the channels for a bouquet
--
grabChannels :: [Tag String] -> String -> [String]
grabChannels tagList bouquet =  take 1
            $ concatMap pickChannel (sections (~== "<a>")
                $ takeWhile (~/= "</p>")
                $ concatMap checkBouquet (sections (~== "<div class=bouquet>")
                     $ dropWhile (~/= "<div class=page-bouquets>") tagList))
            where checkBouquet :: [Tag String] -> [Tag String]
                  checkBouquet tl = let bq =  lines $ innerText $
                                            dropWhile (~/= "<h2>") tl in
                                            if unwords (words $  bq !! 0) == bouquet
                                            then tl else []
                  pickChannel :: [Tag String] -> [String]
                  pickChannel tl =  case tl of
                                    [] -> []
                                    t:ts -> if isTagOpen t
                                              then ("channel " ++ last (splitOn "/"
                                                    $ fromAttrib "href" t)) : pickChannel ts
                                              else pickChannel ts
--
-- | Saves the channels available at a bouquet URL in the configuration file
--
configureTvGrab :: Bool -> String -> String -> String -> IO ()
configureTvGrab isDbgOn cfgFile bouquetUrl bouquet = do
    printDebug isDbgOn $ "(dgb) configureTvGrab:processing URL:" ++ bouquetUrl
    connMgr <- newManager tlsManagerSettings
    req <- try $ parseRequest bouquetUrl
    case req of
      Left e -> do
            print "configureTvGrab:parseRequest:configuration failed"
            print (e :: HttpException)
      Right rq -> do
        !httpRsp <- try $ httpLBS rq
        case httpRsp of
          Left e -> do
                print "configureTvGrab:httpLBS:configuration failed"
                print (e :: HttpException)
          Right rsp -> let s = responseStatus rsp in
                        if statusIsSuccessful s
                        then do
                            printDebug isDbgOn $ "(dgb) configureTvGrab:httpLBS:status code: "
                                    ++ show (statusCode s)
                            let channels = grabChannels (parseTags $ U8.toString
                                    $ getResponseBody rsp) bouquet
                            printDebug isDbgOn channels    -- (renderTags $ concat channels)
                            writeFile cfgFile $ unlines channels
                            printDebug isDbgOn $ "(dgb) configureTvGrab:file written:" ++ cfgFile
                        else print $ "error:configureTvGrab:" ++ show (statusCode s)
    terminate "Configuration finished"

