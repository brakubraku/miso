{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Window
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Window where

import Control.Monad
import Control.Monad.IO.Class

-- import GHCJS.Marshal
-- import JavaScript.Object
-- import JavaScript.Object.Internal

import Miso.Types (Sub)
import Miso.Event
import Miso.FFI
import Miso.String

import Data.Aeson.Types (parseEither)

-- | Captures window coordinates changes as they occur and writes them to
-- an event sink
windowCoordsSub :: ((Int, Int) -> action) -> Sub action
windowCoordsSub f = \sink -> do
  liftIO . sink . f =<< (,) <$> windowInnerHeight <*> windowInnerWidth
  windowAddEventListener "resize" =<< do
    asyncCallback1 $ \windowEvent -> do
      target <- getProp "target" (JSObject windowEvent)
      Just w <- fromJSVal =<< getProp "innerWidth" (JSObject target)
      Just h <- fromJSVal =<< getProp "innerHeight" (JSObject target)
      liftIO . sink $ f (h, w)

-- | @windowSub eventName decoder toAction@ is a subscription which parallels the
-- attribute handler `on`, providing a subscription to listen to window level events.
windowSub :: MisoString -> Decoder r -> (r -> action) -> Sub action
windowSub  = windowSubWithOptions defaultOptions

-- | @windowSubWithOptions options eventName decoder toAction@ is a subscription which parallels the
-- attribute handler `on`, providing a subscription to listen to window level events.
windowSubWithOptions :: Options -> MisoString -> Decoder r -> (r -> action) -> Sub action
windowSubWithOptions Options{..} eventName Decoder{..} toAction = \sink -> do
  windowAddEventListener eventName =<< do
    asyncCallback1 $ \e -> do
      decodeAtVal <- toJSVal decodeAt
      Just v <- fromJSVal =<< objectToJSON decodeAtVal e
      case parseEither decoder v of
        Left s -> error $ "Parse error on " <> fromMisoString eventName <> ": " <> s
        Right r -> do
          when stopPropagation $ eventStopPropagation e
          when preventDefault $ eventPreventDefault e
          liftIO (sink (toAction r))
