{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.String (IsString)

import Network.HTTP.Simple

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (anyAttr)

import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Text.RE.PCRE.ByteString.Lazy

-- |Programm that finds departure times from the deuthe bahn website and 
-- prints out a list of departures for all stations defined in the 
-- function "stations" sorted by departure time

main :: IO ()
main = do
    pages <- mapM fetchPage stations
    mapM_ BC.putStrLn $ sortBy (comparingTime (BL.take 5))
                      $ map formatRow $ concatMap getRows pages

comparingTime :: (IsString b, Ord b) => (a -> b) -> a -> a -> Ordering
comparingTime f x y = compareTime (f x) (f y) where 
  compareTime a b | a > "21:00" && b < "03:00" = LT
                  | a < "03:00" && b > "21:00" = GT
                  | otherwise                  = compare a b

getRows :: BL.ByteString -> [[Tag BL.ByteString]]
getRows = partitions isRow . parseTags

isRow :: Tag BL.ByteString -> Bool
isRow (TagOpen "tr" attr) = anyAttr testAttr attr where
    testAttr ("id",val) = BL.take 7 val == "journey"
    testAttr  _         = False
isRow _                   = False

formatRow :: [Tag BL.ByteString] -> BL.ByteString
formatRow a = BL.concat [ getTime a, ": ", getTrain a, " -> ", getDest a ]

getTime :: [Tag BL.ByteString] -> BL.ByteString
getTime [] = ""
getTime (TagOpen "td" [("class","time")]:TagText text:_) = text
getTime (_:xs) = getTime xs

getTrain :: [Tag BL.ByteString] -> BL.ByteString
getTrain [] = ""
getTrain (TagOpen "a" _:TagText text:_) 
  = fromMaybe "" $ matchedText $ text ?=~ [re|\w*\d+|]
getTrain (_:xs) = getTrain xs

getDest :: [Tag BL.ByteString] -> BL.ByteString
getDest [] = ""
getDest (TagOpen "span" _:_:TagOpen "a" _:TagText text:_)
  = (?=~/ [ed|BI-///|]) $ fromMaybe "" $ matchedText $ text ?=~ [re|\w+|]
getDest (_:xs) = getDest xs

now :: IO (String,String)
now = do
    time <- getZonedTime
    let hour = formatTime defaultTimeLocale "%H:%M"    time
    let day  = formatTime defaultTimeLocale "%d.%m.%y" time
    return (hour,day)

fetchPage :: String -> IO BL.ByteString
fetchPage s = do
  (h,d)   <- now
  request <- parseRequest ("POST " ++ url ++ s)
  getResponseBody <$> httpLBS (setRequestBodyLBS (postBody s h d) request)

stations :: [String]
stations =
  [ "Sudbrackstra%DFe%2C+Bielefeld"
  , "Meller+Stra%DFe%2C+Bielefeld"
  , "Nordpark%2C+Bielefeld"
  ]

url :: String
url = "https://reiseauskunft.bahn.de/bin/bhftafel.exe/dn?ld=9698&country=DEU&rt=1&" 
contentType :: String 
contentType = "application/x-www-form-urlencode"

postBody :: String -> String -> String -> BL.ByteString
postBody station hour day = BL.concat
    [ "input=" , BC.pack station
    , "&date=" , BC.pack day
    , "&time=" , BC.pack hour
    , "&boardType=dep"
    , "&REQTrain_name="
    , "&advancedProductMode="
    , "&productsFilter=000001011"
    , "&start=Suchen"
    ]

