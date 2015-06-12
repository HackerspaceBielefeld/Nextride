{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (sortBy)
import Data.Ord (comparing)

import Network.Browser
import Network.HTTP
import Network.URI
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (anyAttr)

import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Text.Regex.Posix

main :: IO ()
main = do
    BC.putStr "["
    pages <- mapM fetchPage stations
    BC.putStr $ BL.intercalate ","
              $ map formatRow
              $ sortBy (comparing getTime)
              $ concatMap getRows pages
    BC.putStrLn "]"

fetchPage :: String -> IO BL.ByteString
fetchPage s = do
    (hour, time) <- now
    fmap (rspBody . snd) $ browse $ defaultBrowser s url hour time

defaultBrowser :: String -> String -> String -> String ->
    BrowserAction (HandleStream BL.ByteString) (URI, Response BL.ByteString)
defaultBrowser s u h t = do
    setAllowRedirects True
    setOutHandler $ const( return () )
    let typ  = contentType
    let body = postBody s h t
    case parseURI u of
        Nothing  -> error ("Not a valid URL - " ++ u)
        Just uri -> request $ req { rqBody=body } where
            req :: Request BL.ByteString
            req = replaceHeader HdrContentType typ
                . replaceHeader HdrContentLength (show $ BL.length body)
                $ mkRequest POST uri

getRows :: BL.ByteString -> [[Tag BL.ByteString]]
getRows a = partitions isRow $ canonicalizeTags $ parseTags a

isRow :: Tag BL.ByteString -> Bool
isRow (TagOpen "tr" attr) = anyAttr testAttr attr where
    testAttr ("id",val) = BL.take 7 val == "journey"
    testAttr  _         = False
isRow _                   = False

formatRow :: [Tag BL.ByteString] -> BL.ByteString
formatRow a = BL.concat
    [ "{'line':'"     , getTrain a , "',"
    , "'direction':'" , getDest a  , "',"
    , "'departure':'" , getTime a  , "'}"
    ]

getTime :: [Tag BL.ByteString] -> BL.ByteString
getTime [] = ""
getTime (TagClose "tr":_) = ""
getTime (TagOpen "td" [("class","time")]:TagText text:_) = text
getTime (_:xs) = getTime xs

getTrain :: [Tag BL.ByteString] -> BL.ByteString
getTrain [] = ""
getTrain (TagClose "tr":_) = ""
getTrain (TagOpen "a" _:TagText text:_) = BL.tail text =~ rx where
    rx :: BL.ByteString
    rx = "[0-9]+"
getTrain (_:xs) = getTrain xs

getDest :: [Tag BL.ByteString] -> BL.ByteString
getDest [] = ""
getDest (TagClose "tr":_) = ""
getDest (TagOpen "span" _:_:TagOpen "a" _:TagText text:_) = text =~ rx where
    rx :: BL.ByteString
    rx = "[a-z,A-Z]+"
getDest (_:xs) = getDest xs

now :: IO (String,String)
now = do
    time <- getZonedTime
    let hour = formatTime defaultTimeLocale "%H:%M"    time
    let day  = formatTime defaultTimeLocale "%d.%m.%y" time
    return (hour,day)

stations :: [String]
stations =
  [ "Sudbrackstra%DFe%2C+Bielefeld"
  , "Meller Straße, Bielefeld"
  , "Nordpark, Bielefeld"
  ]

url :: String
url = "http://reiseauskunft.bahn.de/bin/bhftafel.exe/dn?ld=9698&country=DEU&rt=1&"

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
