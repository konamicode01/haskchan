{-# LANGUAGE OverloadedStrings #-}

module Phi.HTTP where

import Data.Binary.Builder (fromByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)

import Network.HTTP.Types
import Network.Wai (Response, responseBuilder)

import Lucid (Html, renderText)

fromHtml :: Html () -> Text
fromHtml = toStrict . renderText

respondHtmlWithHeaders' :: ResponseHeaders -> Status -> Html () -> Response
respondHtmlWithHeaders' headers status html =
  responseBuilder status
    (("content-type", "text/html; charset=utf-8") : headers)
    (fromByteString . encodeUtf8 . fromHtml $ html)

respondHtml' :: Status -> Html () -> Response
respondHtml' = respondHtmlWithHeaders' []

respondHtml :: Status -> Html () -> IO (Maybe Response)
respondHtml status html = pure . Just $ respondHtml' status html

respondHtmlWithHeaders :: ResponseHeaders -> Status -> Html () -> IO (Maybe Response)
respondHtmlWithHeaders headers status html = pure . Just $ respondHtmlWithHeaders' headers status html

okHtml :: Html () -> IO (Maybe Response)
okHtml = respondHtml status200
