module OpenGraph ( ogDescription
                 , ogImage
                 , ogSiteName
                 , ogTitle
                 , ogType
                 , ogURL
                 ) 
       where

import Data.List (intercalate)
import Text.Blaze.Html5 as H ((!))
import Text.Blaze.Html5 as H (Markup, body, br, div, docTypeHtml, head, link, meta, stringTag, customAttribute, title, toMarkup, ToValue(..), img)
import Text.Blaze.Html5.Attributes as HA (content, href, httpEquiv, manifest, rel, type_, src)
import Text.Blaze.Html.Renderer.Utf8  (renderHtml)

import Network.URI (URI, parseURI, parseRelativeReference)

-- implementation of OpenGraph meta tags: http://ogp.me/

property =  H.customAttribute (H.stringTag "property")

ogMeta ts c = H.meta ! property (H.toValue $ fmt ts) ! HA.content (H.toValue c)
  where fmt ts = intercalate ":" ("og" : ts)

ogType :: (ToValue a) => a -> H.Markup
ogType = ogMeta ["type"]
ogTitle :: (ToValue a) => a -> H.Markup
ogTitle = ogMeta ["title"]
ogSiteName :: (ToValue a) => a -> H.Markup
ogSiteName = ogMeta ["site_name"]
ogDescription :: (ToValue a) => a -> H.Markup
ogDescription = ogMeta ["description"]

ogURL :: URI -> H.Markup
ogURL u = ogMeta ["url"] (show u)

type MimeType = String

ogImage :: URI -> URI -> (Int, Int) -> MimeType -> H.Markup
ogImage u su (w,h) mt = do
  ogMeta ["image"] (show u)
  ogMeta ["image","secure_url"] (show su)
  ogMeta ["image","width"] (show w)
  ogMeta ["image","height"] (show h)
  ogMeta ["image","type"] mt
