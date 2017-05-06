module Sitemap where

import Imports hiding (head, link, div, map)
import Types
import Config

import qualified Data.ByteString.Lazy as BL
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Data.Time.Format

createSitemap :: Config -> [PageData] -> BL.ByteString
createSitemap Config{..} pd = renderMarkup $ do
  unsafeLazyByteString "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
  customParent "urlset" ! customAttribute "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9"
           ! customAttribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
           ! customAttribute "xsi:schemaLocation" "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"
    $ forM_ pd $ \PageData{..} -> do
      customParent "url" $ do
        customParent "loc" $ text ("https://anfelor.github.io/blog/" <> pageLocation)
        customParent "lastmod" $ string (formatTime defaultTimeLocale "%F" pageLastMod)
        case pageType of
          FrontPage -> do
            customParent "changefreq" $ text "daily"
            customParent "priority" $ text "0.5"
          Article importance ->
            case importance of
              Ignore -> do
                customParent "changefreq" $ "monthly"
                customParent "priority" $ text "0.0"
              Normal -> do
                customParent "changefreq" $ "weekly"
                customParent "priority" $ text "0.8"
              Promote -> do
                customParent "changefreq" $ "weekly"
                customParent "priority" $ text "1.0"
