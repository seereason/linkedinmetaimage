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

myport = 8010

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
    H.img ! HA.src (H.toValue $ show imageURI)  ! HA.width (H.toValue "600px") -- ! HA.height (H.toValue imageHeight) ! HA.width (H.toValue imageWidth)

handlers :: ServerPartT IO Response
handlers = msum [ rootHandler
                , imageHandler
                , notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
                ]

domain :: String
domain = "c-73-51-188-220.hsd1.il.comcast.net"
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


descriptionString :: String
descriptionString = "This site has minimal content, just OpenGraph meta tags."

application = htmlTemplate titleString ogs [helloWorld] 
  where ogs = [ ogDescription descriptionString
              , ogImage imageFullURI (imageWidth, imageHeight)  "image/jpeg"
              , ogTitle titleString
              , ogType "product"
              , ogSiteName "TestOpenGraph"
              , ogURL baseURI
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
