{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
import Data.Time.Format (FormatTime(..))
import Happstack.Server
import Happstack.Server.Internal.LogFormat (formatRequestCombined)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FilePath (FilePath, (<.>), (</>))
import System.IO (stdout)
import Data.Aeson as Aeson (decode)

import Data.Text as Text
import Data.FileEmbed
import Data.Maybe
import qualified Data.ByteString as B

import Text.Blaze.Html5 as H ((!))
import qualified Text.Blaze.Html5 as H (Markup, body, br, div, docTypeHtml, head, link, meta, stringTag, customAttribute, title, toMarkup, toValue, img)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, manifest, rel, type_, src, height, width, style)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)
import Network.URI (URI, parseURI, parseRelativeReference)
import OpenGraph

debugM x y = putStr x >> putStr ":" >> putStrLn y

myport = 8011

main :: IO ()
main = do
  let p = myport
  debugM "SimpleServer" $ "Starting server on port " ++ show p
  simpleHTTP (nullConf { port = p }) $ handlers
  

titleString:: String 
titleString = "Test OG:Image Title"

helloWorld = H.div $ do
  H.toMarkup titleString
  H.br
  H.div ! HA.style (H.toValue "width: 100%;") $ do
--    H.img ! HA.src (H.toValue $ show imageURI)  ! HA.width (H.toValue "600px")
    H.img ! HA.src (H.toValue $ show image2URI)  ! HA.width (H.toValue "180px")

handlers :: ServerPartT IO Response
handlers = msum [ rootHandler
--                , imageHandler
                , image2Handler
                , notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
                ]

domain :: String
domain = "xyzzy.stockwits.com"
-- domain = "c-73-51-188-220.hsd1.il.comcast.net"
-- domain = "localhost"

baseURI :: URI
baseURI = fromJust $ parseURI $ "http://" ++ domain ++ ":" ++ show myport



imageFullURI :: URI
imageFullURI = fromJust $ parseURI $ "http://" ++ domain ++ ":" ++ show myport ++ imagePath

imagePath =  "/og/image"

imageURI :: URI
imageURI = fromJust $ parseRelativeReference imagePath

imageFP :: FilePath
imageFP = "ogimage.jpg"

imageWidth, imageHeight :: Int
imageWidth = 800
imageHeight = 600

imageHandler :: ServerPartT IO Response
imageHandler = dirs (imagePath) $ serveFile (asContentType "image/jpeg") imageFP



image2FullURI :: URI
image2FullURI = fromJust $ parseURI $ "http://" ++ domain ++ ":" ++ show myport ++ image2Path

image2Path =  "/og/image2"

image2URI :: URI
image2URI = fromJust $ parseRelativeReference image2Path

image2FP :: FilePath
image2FP = "ogimage2.jpg"

image2Handler :: ServerPartT IO Response
image2Handler = dirs (image2Path) $ serveFile (asContentType "image/jpeg") image2FP

image2Width, image2Height :: Int
image2Width = 200
image2Height = 200


descriptionString :: String
descriptionString = "This site has minimal content, just OpenGraph meta tags."

application = htmlTemplate titleString ogs [helloWorld] 
  where ogs = [ ogType "website"
              , ogSiteName "TestOpenGraph"
              , ogTitle titleString
              , ogDescription descriptionString
              , ogURL baseURI
--              , ogImage imageFullURI (imageWidth, imageHeight)  "image/jpeg"
              , ogImage image2FullURI (image2Width, image2Height)  "image/jpeg"
              ]
 

htmlTemplate title imports bodies =  do 
  H.docTypeHtml $ do
      H.head $ do
        H.meta ! HA.httpEquiv (H.toValue "Content-Type") ! HA.content (H.toValue "text/html; charset=UTF-8")
        sequence_ $ imports
        H.title (H.toMarkup title)
      H.body $ do sequence_ bodies



rootHandler :: ServerPartT IO Response
rootHandler = msum [ nullDir >> ok (toResponse application)
                   , dirs "index.html" $ ok (toResponse application)
                   ]
