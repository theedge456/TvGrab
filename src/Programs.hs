-----------------------------------------------------------------------------
--
-- Module      :  TvGrab.Programs
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
module Programs (
    grabPrograms
) where

import qualified Data.ByteString.Lazy.UTF8 as U8

import           Network.HTTP.Client hiding (IsString)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple
import           Text.Regex (splitRegex, mkRegex, matchRegex)
import           Network.HTTP.Types.Status (statusCode, statusIsSuccessful)
import           Text.XML.Light
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Data.List (isSuffixOf, isPrefixOf)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format
import           Data.Time.Calendar
import           Data.Foldable
import           Data.Maybe (isJust)
import           Control.Monad
import           System.IO
import           Control.Exception          (try)
import           Options (TvOptDays)
import           Common (printDebug, epgDomainName, rootUrl,language,
                         safeHead, safeTail, safeHeadTail)


--
-- | Keeps the an opening and its corresponding closing tag from a tag list
--
-- {-# SCC selectTags #-}
selectTags :: String -> Int -> [Tag String] -> [Tag String] -> [Tag String]
selectTags tag !level !res tl = case tl of
                                    [] -> res
                                    t:ts | tagOpenNameLit tag t ->
                                                selectTags tag (level+1) (res ++ [t]) ts
                                         | level == 0 -> res
                                         | tagCloseNameLit tag t ->
                                                      selectTags tag (level-1) (res ++ [t]) ts
                                         | otherwise -> selectTags tag level (res ++ [t]) ts
--
-- | Gets the description, date, credits and review of a program from an url
--
-- {-# SCC getProgramDetails "mySccGetProgramDetails" #-}
getProgramDetails :: Bool -> String -> IO [Element]
getProgramDetails dbg url = do
    printDebug dbg $ "(dgb) getProgramDetails: url=" ++ rootUrl ++ url
    req <- try $ parseRequest $ rootUrl ++ url
    case req of
      Left e -> do
            print $ "getProgramDetails:parseRequest:skipping URL:" ++ rootUrl ++ url
            print (e :: HttpException)
            return []
      Right rq -> do
        !httpRsp <- httpLBS rq
        let s = responseStatus httpRsp in
            if statusIsSuccessful s
            then do
              printDebug dbg $ "(dgb) getProgramDetails: status code="
                ++ show (statusCode $ responseStatus httpRsp)
              let -- {-# SCC tl "myScc_tl" #-}
                  !tl = parseTags $ U8.toString $ getResponseBody httpRsp
                  !progInfo = selectTags "div" 0 []
                                $ dropWhile (~/= "<div class=program-informations>") tl
                  !headInfo = foldl' (
                                \res e -> let !e1 = selectTags "li" 0 [] e
                                              !tn = innerText $ selectTags "span" 0 []
                                                $ dropWhile (~/= "<span class=label>") e1
                                              !tv = unwords $ words $ innerText $ selectTags "span" 0 []
                                                     $ dropWhile (~/= "<span class=value>") e1 in
                                   -- do not use mkRegex with UTF8
                                   if isPrefixOf "Année de production" tn
                                   then res ++ [unode "date" tv]
                                   else if isJust $ matchRegex (mkRegex "Genre") tn
                                        then res ++ [unode "category" tv]
                                        else res
                                     ) [] $ sections (~== "<li>") $ selectTags "ul" 0 []
                              $ dropWhile (~/= "<ul class=list-fiche>") progInfo
                  !desc = (\v -> case v of
                                  "" -> []
                                  _ -> [unode "desc" ([Attr (unqual "lang") language ], v)]
                         ) $ unwords $ words $ innerText
                      $  (\l -> case safeHead $ sections (~== "<p>") l of
                                 Nothing -> l
                                 Just tl -> selectTags "p" 0 [] tl
                         )
                      $ dropWhile (~/= "</div>")
                      $ selectTags "div" 0 []
                      $ foldl' (
                          \res e -> let !e1 = selectTags "div" 0 [] e
                                        !e2 = dropWhile (\t -> ((~/=) t "<h2>") && ((~/=) t "<h3>") )
                                          $ dropWhile (tagOpenAttrNameLit "div" "class"
                                            (not . isPrefixOf "title-block")) e1 in
                                          case e2 of
                                            [] -> res
                                            _ -> if isPrefixOf "Synopsis" $ innerText e2
                                                 then res ++ e
                                                 else res
                      ) [] $ sections (~== "<div class=section-fiche-program>") progInfo
                  !review = (\v -> case v of
                                     "" -> []
                                     _ -> [unode "review" ([Attr (unqual "type") "text",
                                          Attr (unqual "source") "",
                                          Attr (unqual "reviewer") epgDomainName,
                                          Attr (unqual "lang") language ], v)]
                           ) $ unwords $ words $ innerText
                      $ dropWhile (~/= "</div>")
                      $ selectTags "div" 0 [] $ dropWhile (~/= "<div class=section-fiche-program>")
                      $ dropWhile (~/= "<span id=block-avis>") progInfo
                  !credits = (\l -> case l of
                                    [[]] -> []
                                    _ -> [unode "credits" $ concat l]
                            ) $ map grabCredits $ sections (~== "<div class=col-xs-12>")
                      $ selectTags "div" 0 []
                      $ dropWhile (~/= "<div class=block-casting id=block-casting>") tl
              printDebug dbg $ "(dgb) getProgramDetails: headInfo=" ++ concatMap showElement headInfo
              printDebug dbg $ "(dgb) getProgramDetails: desc=" ++ concatMap showElement desc
              printDebug dbg $ "(dgb) getProgramDetails: review=" ++ concatMap showElement review
              printDebug dbg $ "(dgb) getProgramDetails: credits=" ++ concatMap showElement credits
              return $ headInfo ++ desc ++  review ++ credits
            else do
                 print $ "getProgramDetails:httpLBS:skipping URL:" ++ rootUrl ++ url
                 printDebug dbg $ "(dgb) getProgramDetails:httpLBS:status code="
                        ++ show (statusCode s)
                 return []
    where -- {-# SCC grabCredits #-}
          grabCredits :: [Tag String] -> [Element]
          grabCredits !tl = (\t -> case innerText $ selectTags "h3" 0 []
                                                    $ dropWhile (~/= "<h3 class=title>")
                                                    $ selectTags "div" 0 [] t of
                                   "Acteurs et actrices" -> foldl' (
                                        \res e -> let !e1 = selectTags "div" 0 [] e
                                                      !tn = innerText $ takeWhile (~/= "</span>")
                                                        $ dropWhile (~/= "<span class=name>") e1
                                                      !tv = innerText $ takeWhile (~/= "</span>")
                                                        $ dropWhile (~/= "<span class=role>") e1 in
                                         res ++ [unode "actor" ([Attr (unqual "role") tv], tn)]
                                                                ) [] $ drop 1
                                        $ sections (tagOpenAttrNameLit "div" "class"
                                            (== "row keep-cols")) t
                                   "Réalisateur" -> foldl' (
                                        \res e -> let !e1 = selectTags "div" 0 [] e
                                                      !tn = innerText $ takeWhile (~/= "</span>")
                                                        $ dropWhile (~/= "<span class=name>") e1 in
                                                res ++ [unode "director" tn]
                                                        ) []
                                                $ sections (~== "<div class=col-xs-6>") t
                                   "Scénario" -> foldl' (
                                         \res e -> let !e1 = selectTags "div" 0 [] e
                                                       !tn = innerText $ takeWhile (~/= "</span>")
                                                        $ dropWhile (~/= "<span class=name>") e1 in
                                                res ++ [unode "writer" tn]
                                                        ) []
                                                $ sections (~== "<div class=col-xs-6>") t
                                   _ -> []
                           ) $ selectTags "div" 0 [] tl
--
-- | Builds the start and stop times of a program
--
-- {-# SCC buildProgramTimes #-}
buildProgramTimes :: TvOptDays -> ZonedTime -> [Tag String] -> (String, String)
buildProgramTimes ofs tz tl = let sta = (\v -> case v of
                                                 Nothing -> ["0","0"]
                                                 Just x -> splitRegex (mkRegex "h") x
                                        ) $ safeHead $ splitRegex (mkRegex "-")
                                        $ innerText tl
                                  (shh, smm) = (read (head sta)::Int, read (last sta)::Int)
                                  start = ZonedTime { zonedTimeToLocalTime = LocalTime {
                                                          localDay = addDays (fromIntegral ofs)
                                                          $ localDay $ zonedTimeToLocalTime tz,
                                                            localTimeOfDay = TimeOfDay{
                                                            todHour = shh, todMin = smm, todSec = 0 }
                                                                                       },
                                                      zonedTimeZone = zonedTimeZone tz }
                                  duration = (\v -> case safeHeadTail 1 Nothing v of
                                                     Nothing -> 0
                                                     Just x -> 60 * read x::Integer
                                            ) $ splitRegex (mkRegex "[( min)]")
                                            $ innerText $ dropWhile (~/= "<span>") tl
                                  stop = utcToZonedTime (zonedTimeZone start)
                                                $ addUTCTime (fromInteger duration)
                                                $ zonedTimeToUTC start in
                              (formatTime defaultTimeLocale "%Y%m%d%H%M%S %z" start,
                                   formatTime defaultTimeLocale "%Y%m%d%H%M%S %z" stop)
--
-- | Grabs the programs of each channel
--
-- {-# SCC processChannel #-}
processChannel :: Bool -> [Tag String] -> TvOptDays -> TvOptDays
                    -> String -> Handle -> IO ()
processChannel dbg !tagList days offset !channelName fHandle = do
      let !progsByDay = take (days - offset)
            $ drop offset $ sections (~== "<div class=channel>")
            $ dropWhile (~/= "<div id=programs class=today>") tagList
      timeZ <- getZonedTime
      -- readStr <- readFile "/home/fabien/arte-barbier.html"
      -- processPrograms progsByDay offset timeZ channelName readStr
      processPrograms progsByDay offset timeZ channelName
      where -- {-# SCC processPrograms #-}
            processPrograms :: [[Tag String]] -> TvOptDays -> ZonedTime -> String -> IO ()
            processPrograms tl !ofs tz cn = do
                   printDebug dbg $ "(dgb) processing channel:" ++ cn
                        ++ ":offset:" ++ show (fromIntegral ofs)
                   case tl of
                           [] -> return ()
                           t:ts -> do
                                    let progs = sections (tagOpenAttrNameLit "div" "class"
                                            (\cVal -> (isPrefixOf "program" cVal)
                                                 && not (isSuffixOf "no-program" cVal)))
                                           $ selectTags "div" 0 [] t
                                    pickProgramDetails progs ofs tz cn
                                    processPrograms ts (ofs+1) tz cn
                   where pickProgramDetails :: [[Tag String]] -> TvOptDays -> ZonedTime -> String -> IO ()
                         pickProgramDetails !pgs !offs tz cn =
                                      case pgs of
                                            [] -> return ()
                                            p:ps -> do
                                                      let !detail = concat $ sections (~== "<p>")
                                                            $ takeWhile (~/= "</div>")
                                                            $ dropWhile (~/= "<div class=meta-datas>") p
                                                          (!startTime,!stopTime)  = buildProgramTimes offs tz
                                                              $ takeWhile (~/= "</p>")
                                                              $ dropWhile (~/= "<p class=time>") detail
                                                          !titleProg = unode "sub-title" $ innerText
                                                              $ takeWhile (~/= "</p>")
                                                              $ dropWhile (~/= "<p class=title-episode>") detail
                                                          !pgAttrs = [Attr (unqual "start") startTime,
                                                               Attr (unqual "stop") stopTime,
                                                               Attr (unqual "channel") cn]
                                                          (!title, !detailUrl) = (\tl -> ( unode "title" $ innerText tl,
                                                               case safeHeadTail 1 Nothing tl of
                                                                  Nothing -> ""
                                                                  Just x ->  fromAttrib "href" x
                                                                 ))
                                                              $ takeWhile (~/= "</p>")
                                                              $ dropWhile (~/= "<p class=title>") $ detail
                                                      !progDetails <- getProgramDetails  dbg detailUrl
                                                      hPutStrLn fHandle $! showElement
                                                        $ unode "programme" (pgAttrs, filter ((/= "") . strContent)
                                                           [title , titleProg] ++ progDetails)
                                                      hFlush fHandle
                                                      pickProgramDetails ps offs tz cn
--
-- | Picks the programs of each channel
--
pickPrograms :: Bool -> String -> [String] -> TvOptDays -> TvOptDays -> String -> IO ()
pickPrograms  dbg url channelList days offset outFile = do
      fHandle <- openFile outFile AppendMode
      hSetEncoding fHandle utf8 -- latin1
      processRequest fHandle $ map ((++) url) channelList
      hClose fHandle
      where -- {-# SCC processRequest #-}
            processRequest :: Handle -> [String] -> IO ()
            processRequest fHandle !urls = case urls of
                                             [] -> return ()
                                             u:us -> do
                                                printDebug dbg $ "(dgb) processRequest: URL=" ++ u
                                                req <- try $ parseRequest u
                                                case req of
                                                  Left e -> do
                                                    print $ "processRequest: skipping URL:" ++ u
                                                    print (e :: HttpException)
                                                  Right rq -> do
                                                     !httpRsp <- httpLBS rq -- mgr
                                                     let s = responseStatus httpRsp
                                                     printDebug dbg $ "(dgb) processRequest:httpLBS:status code="
                                                            ++ show (statusCode s)
                                                     case statusIsSuccessful s of
                                                        True -> processChannel dbg (parseTags $! U8.toString $ getResponseBody httpRsp)
                                                            days offset (last (splitRegex (mkRegex "/") u)
                                                            ++ "." ++ epgDomainName) fHandle
                                                        _ -> print $ "processRequest:httpLBS:skipping URL:" ++ u
                                                     processRequest fHandle us
--
-- | Grabs the programs
--
grabPrograms :: Bool -> String -> String -> TvOptDays -> TvOptDays -> String -> IO ()
grabPrograms isDbgOn cfgFile outFile days offset gridUrl = do
    strChannels <- readFile cfgFile
    let !selectedChannels = filter ((/=) "channel") $ words $ unlines
                            $ splitRegex (mkRegex "#.*\n") strChannels
        !channels = map (\c -> unode "channel" (Attr (unqual "id") (c ++ "." ++ epgDomainName),
                                                 Elem $ unode "display-name" c)
                       ) selectedChannels
    connMgr <- newManager tlsManagerSettings
    writeFile outFile $ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        ++ "<tv source-info-url=\"https://www.telestar.fr\" source-data-url=\"https://www.telestar.fr\" generator-info-name=\"XMLTV\" generator-info-url=\"http://xmltv.org/\">\n"
        ++ (concat $! map showElement channels)
    pickPrograms isDbgOn gridUrl selectedChannels days offset outFile
    appendFile outFile "</tv>"

