{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Main where

import           Common
import           Data.Aeson
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import qualified Lucid as L
import           Lucid.Base
import           Network.HTTP.Types hiding (Header)
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           Servant
import qualified System.IO as IO

import           Miso.String

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3002..."
  run 3002 $ logStdout (compress app)
    where
      compress = gzip def { gzipFiles = GzipCompress }

app :: Application
app = serve (Proxy @API) website
  where
    website = serveDirectoryWith (defaultWebAppSettings "static")
      :<|> serverHandlers
      :<|> pure misoManifest
      :<|> pure robotsTxt
      :<|> Tagged handle404

robotsTxt :: Text
robotsTxt =
  T.unlines
  [ "# www.robotstxt.org/"
  , ""
  , "# Allow crawling of all content"
  , "User-agent: *"
  , "Disallow:"
  ]

-- | robots.txt
type RobotsTXT = "robots.txt" :> Get '[PlainText] Text

-- | API type
type API = ("static" :> Raw)
  :<|> ServerRoutes
  :<|> ("manifest.json" :> Get '[JSON] Manifest)
  :<|> RobotsTXT
  :<|> Raw

data Manifest
  = Manifest
  { name :: Text
  , short_name :: Text
  , start_url :: Text
  , display :: Text
  , theme_color :: Text
  , description :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Manifest

misoManifest :: Manifest
misoManifest =
  Manifest { name = "Haskell Miso"
           , short_name = "Miso"
           , start_url = "."
           , display = "standalone"
           , theme_color = "#00d1b2"
           , description = "A tasty Haskell front-end web framework"
           }

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
      renderBS $ toHtml $ Page (haskellMisoComponent go404)

instance L.ToHtml a => L.ToHtml (Page a) where
  toHtmlRaw = L.toHtml
  toHtml (Page x) = do
      L.doctype_
      L.html_ [ L.lang_ "en" ] $ do
        L.head_ $ do
          L.title_ "Miso: A tasty Haskell front-end web framework"
          L.link_ [ L.rel_ "stylesheet"
                  , L.href_ "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.2/gh-fork-ribbon.min.css"
                  ]
          L.link_ [ L.rel_ "manifest"
                  , L.href_ "/manifest.json"
                  ]
          L.link_ [ L.rel_ "icon", L.href_ "https://www.haskell.org/img/favicon.ico", L.type_ "image/x-icon" ]
          L.meta_ [ L.charset_ "utf-8" ]
          L.meta_ [ L.name_ "theme-color", L.content_ "#00d1b2" ]
          L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]
          L.meta_ [ L.name_ "viewport"
                  , L.content_ "width=device-width, initial-scale=1"
                  ]
          L.meta_ [ L.name_ "description"
                  , L.content_ "Miso is a small isomorphic Haskell front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe servant-style routing and an extensible Subscription-based subsystem. Inspired by Elm, Redux and Bobril. Miso is pure by default, but side effects (like XHR) can be introduced into the system via the Effect data type. Miso makes heavy use of the GHC FFI and therefore has minimal dependencies."
                  ]
          L.style_ ".github-fork-ribbon:before { background-color: \"#e59751\" !important; } "
          cssRef animateRef
          cssRef bulmaRef
          cssRef fontAwesomeRef
          jsRef "https://buttons.github.io/buttons.js"
          L.script_ analytics
          jsRef "static/all.js"
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

fontAwesomeRef :: MisoString
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

animateRef :: MisoString
animateRef = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css"

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"

analytics :: MisoString
analytics =
  mconcat
    [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
    , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
    , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
    , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
    , "ga('create', 'UA-102668481-1', 'auto');"
    , "ga('send', 'pageview');"
    ]

-- | Server handlers
serverHandlers :: Server ServerRoutes
serverHandlers = mkPage goExamples
  :<|> mkPage goDocs
  :<|> mkPage goCommunity
  :<|> mkPage goHome
  :<|> mkPage go404
     where
       mkPage :: URI -> Handler (Page HaskellMisoComponent)
       mkPage url = pure $ Page (haskellMisoComponent url)
