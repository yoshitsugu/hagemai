{-# LANGUAGE OverloadedStrings #-}

module IndexPage where

import           Servant.HTML.Blaze
import           Text.Blaze.Html5            hiding (main)
import qualified Text.Blaze.Html5            as H (head, title)
import           Text.Blaze.Html5.Attributes (charset, content, href, httpEquiv,
                                              lang, rel, src, type_)
data IndexPage = IndexPage

instance ToMarkup IndexPage where
  toMarkup IndexPage =
    docTypeHtml ! lang "ja" $ do
      H.head $ do
          meta ! charset "utf-8"
          meta ! content "IE=edge,chrome=1" ! httpEquiv "X-UA-Compatible"
          meta ! httpEquiv "cache-control" ! content "max-age=0"
          meta ! httpEquiv "cache-control" ! content "no-cache"
          meta ! httpEquiv "expires" ! content "0"
          meta ! httpEquiv "expires" ! content "Tue, 01 Jan 1980 1:00:00 GMT"
          meta ! httpEquiv "pragma" ! content "no-cache"
          H.title "Hagemai"
          link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
          link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
      body $ do
          script ! type_ "text/javascript" ! src "/assets/main.js" $ mempty
          script ! type_ "text/javascript" $ "var main = Elm.fullscreen(Elm.Main, {initialPath: window.location.pathname});"
