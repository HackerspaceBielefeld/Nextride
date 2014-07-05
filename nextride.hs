{-
Copyright (c) 2013, Juergen Peters <taulmarill@xgn.de>

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC

import Network.Browser
import Network.HTTP
import Network.URI
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (anyAttr)

import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Text.Regex.Posix

main :: IO ()
main = do
    body <- fetchPage
    BC.putStr "["
    BC.putStr $ BL.intercalate "," $ map formatRow $ getRows body
    BC.putStrLn "]"

fetchPage :: IO BL.ByteString
fetchPage = do 
    (hour, time) <- now
    fmap (rspBody . snd) $ browse $ defaultBrowser url hour time

defaultBrowser :: String -> String -> String -> 
    BrowserAction (HandleStream BL.ByteString) (URI, Response BL.ByteString)
defaultBrowser u h t = do
    setAllowRedirects True
    setOutHandler $ const( return () )
    let typ  = contentType
    let body = postBody h t
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

url :: String
url = 
    "http://reiseauskunft.bahn.de/bin/bhftafel.exe/dn?ld=9698&country=DEU&rt=1&"

contentType :: String
contentType = "application/x-www-form-urlencode"

postBody :: String -> String -> BL.ByteString
postBody hour day = BL.concat
    [ "input=Sudbrackstra%DFe%2C+Bielefeld"
    , "&REQ0JourneyStopsSID=A%3D1%40O%3DSudbrackstra%DFe%2C+Bielefeld%40X%3D8538736%40Y%3D52035883%40U%3D80%40L%3D000925626%40B%3D1%40p%3D1363765172%40"
    , "&date=" , BC.pack day
    , "&time=" , BC.pack hour
    , "&boardType=dep"
    , "&REQTrain_name="
    , "&advancedProductMode="
    , "&GUIREQProduct_8=on"
    , "&start=Suchen"
    ]
