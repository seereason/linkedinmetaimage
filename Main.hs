{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent
import Control.Monad as Monad (msum)
import Control.Monad.Trans as Trans (liftIO)
import Data.Time.Format (FormatTime(..))
import Happstack.Server
import Happstack.Server.SimpleHTTPS
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
import qualified Text.Blaze.Html5 as H (Markup, a, body, br, div, docTypeHtml, head, link, meta, stringTag, customAttribute, title, toMarkup, toValue, img)
import qualified Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, manifest, rel, type_, src, height, width, style)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)
import Network.URI (URI, parseURI, parseRelativeReference)
import OpenGraph

debugM x y = putStr x >> putStr ":" >> putStrLn y


httpPort = 8010
httpsPort = 8011


tlsConf :: TLSConf
tlsConf = nullTLSConf { tlsPort = httpsPort
                      , tlsCert = "sslcertificate/new.cert.cert"
                      , tlsKey = "sslcertificate/new.cert.key"
                      }

httpConf :: Conf
httpConf = nullConf { port = httpPort }

httpURL = "http://" ++ domain ++ ":" ++ show (port httpConf) ++ "/"
httpsURL = "https://" ++ domain ++ ":" ++ show (tlsPort tlsConf) ++ "/"

httpLink = H.a ! HA.href (H.toValue httpsURL) $ H.toMarkup "Link to secure https"
httpsLink = H.a ! HA.href (H.toValue httpURL) $ H.toMarkup "Link to plain http"


main :: IO ()
main = do
  let p = httpPort
  debugM "SimpleServer" $ "Starting server on port " ++ show (port httpConf) ++ 
    " sslport " ++ show (tlsPort tlsConf)
  forkIO $ simpleHTTPS tlsConf sslhandlers
  simpleHTTP httpConf handlers
  

titleString:: String 
titleString = "Test OG:Image Title"

helloWorld secure = H.div $ do
  H.toMarkup titleString
  H.br
  if secure then httpLink else httpsLink -- sic
  H.br
  H.div ! HA.style (H.toValue "width: 100%;") $ do
--    H.img ! HA.src (H.toValue $ show imageURI)  ! HA.width (H.toValue "600px")
    H.img ! HA.src (H.toValue $ show image2URI)  ! HA.width (H.toValue "180px")
    H.br
    H.img ! HA.src (H.toValue $ show image2FullSURI)  ! HA.width (H.toValue "180px")

sslhandlers :: ServerPartT IO Response
sslhandlers = msum [ rootHandler False
                   , imageHandler
                   , image2Handler
                   ]

handlers :: ServerPartT IO Response
handlers = msum [ rootHandler True
--                , imageHandler
                , image2Handler
                , notFound (toResponse ("plain text" ++ ":" ++ "notFound"))
                ]

-- httpEither

domain :: String
--domain = "xyzzy.stockwits.com"
domain = "c-73-51-188-220.hsd1.il.comcast.net"
-- domain = "localhost"

baseURI :: URI
baseURI = fromJust $ parseURI $ "http://" ++ domain ++ ":" ++ show httpPort



imageFullURI :: URI
imageFullURI = fromJust $ parseURI $ "http://" ++ domain ++ ":" ++ show httpPort ++ imagePath

imageFullSURI :: URI
imageFullSURI = fromJust $ parseURI $ "https://" ++ domain ++ ":" ++ show httpsPort ++ imagePath
  -- #($*@)#@$%$ imageFullURI { uriScheme = "https:" }

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
image2FullURI = fromJust $ parseURI $ "http://" ++ domain ++ ":" ++ show httpPort ++ image2Path

image2FullSURI :: URI
image2FullSURI = image2FullURI

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

application secure = htmlTemplate titleString ogs [helloWorld secure]
  where ogs = [ ogType "website"
              , ogSiteName "TestOpenGraph"
              , ogTitle titleString
              , ogDescription descriptionString
              , ogURL baseURI
--              , ogImage imageFullURI (imageWidth, imageHeight)  "image/jpeg"
              , ogImage image2FullURI image2FullSURI (image2Width, image2Height)  "image/jpeg"
              ]
 

htmlTemplate title imports bodies =  do 
  H.docTypeHtml $ do
      H.head $ do
        H.meta ! HA.httpEquiv (H.toValue "Content-Type") ! HA.content (H.toValue "text/html; charset=UTF-8")
        sequence_ $ imports
        H.title (H.toMarkup title)
      H.body $ do sequence_ bodies



rootHandler :: Bool -> ServerPartT IO Response
rootHandler secure = msum [ nullDir >> ok (toResponse $ application secure)
                          , dirs "index.html" $ ok (toResponse $ application secure)
                          ]
