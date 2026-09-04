{-# LANGUAGE OverloadedStrings #-}

module Phi where

import           Data.ByteString (ByteString, isInfixOf)
import           Data.Text (Text)
import qualified Data.Text as T (drop, length, take, isSuffixOf)
import           Data.Text.Encoding (encodeUtf8)

import           Network.HTTP.Types.Status (ok200, requestEntityTooLarge413, permanentRedirect308)

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.RequestSizeLimit
import           Network.Wai (Application, Middleware, ifRequest, mapResponseHeaders, modifyResponse, responseLBS, responseStatus, requestHeaders, pathInfo)
import           Web.Fn (toWAI)

import           Phi.Context (Context)
import           Phi.Database (createTables)
import           Phi.Routes (routes)

limitRequestSize :: Middleware
limitRequestSize = requestSizeLimitMiddleware settings
  where
    settings =
      setOnLengthExceeded onLengthExceeded . setMaxLengthForRequest maxLengthForRequest $
      defaultRequestSizeLimitSettings
    onLengthExceeded =
      \_maxLength _app _req sendResponse -> sendResponse tooLargeResponse
    tooLargeResponse = responseLBS
      requestEntityTooLarge413
      [("content-type", "text/html; charset=utf-8")]
      "<h1>413 Request Entity Too Large</h1>"
    maxLengthForRequest =
      \req -> pure $ Just $
        case pathInfo req of
          [".phi", "post"]                               -> 16 * 1024^2 + 2048
          [".phi", "auth", "board", "banners", _, "add"] -> 16 * 1024^2 + 2048
          _                                              ->               2048

cacheStaticFiles :: Middleware
cacheStaticFiles =
  ifRequest isForStaticFiles $ \app req sendResponse ->
    app req $ \res ->
      if responseStatus res == ok200
      then sendResponse $
        mapResponseHeaders (addHeaders (pathInfo req)) res
      else sendResponse res
  where
    isForStaticFiles req =
      take 2 (pathInfo req) == [".phi", "static"]

    addHeaders paths headers =
      cacheHeader : mimeHeader paths headers ++ headers

    cacheHeader =
      ("cache-control", "public, max-age=86400, immutable")

    mimeHeader paths headers =
      case lookup "content-type" headers of
        Just _  -> []
        Nothing ->
          case staticMimeType paths of
            Nothing   -> []
            Just mime -> [("content-type", mime)]

staticMimeType :: [Text] -> Maybe ByteString
staticMimeType paths =
  case reverse paths of
    filename : _ ->
      case () of
        _ | ".webp" `T.isSuffixOf` filename -> Just "image/webp"
          | ".jpg" `T.isSuffixOf` filename  -> Just "image/jpeg"
          | ".jpeg" `T.isSuffixOf` filename -> Just "image/jpeg"
          | ".png" `T.isSuffixOf` filename  -> Just "image/png"
          | ".gif" `T.isSuffixOf` filename  -> Just "image/gif"
          | ".svg" `T.isSuffixOf` filename  -> Just "image/svg+xml"
          | ".ico" `T.isSuffixOf` filename  -> Just "image/x-icon"
          | ".css" `T.isSuffixOf` filename  -> Just "text/css; charset=utf-8"
          | ".js" `T.isSuffixOf` filename   -> Just "text/javascript; charset=utf-8"
          | ".mp4" `T.isSuffixOf` filename  -> Just "video/mp4"
          | ".webm" `T.isSuffixOf` filename -> Just "video/webm"
          | ".mp3" `T.isSuffixOf` filename  -> Just "audio/mpeg"
          | ".ogg" `T.isSuffixOf` filename  -> Just "audio/ogg"
          | ".flac" `T.isSuffixOf` filename -> Just "audio/flac"
          | ".txt" `T.isSuffixOf` filename  -> Just "text/plain; charset=utf-8"
          | otherwise -> Nothing
    [] -> Nothing


addSecurityHeaders :: Middleware
addSecurityHeaders = modifyResponse . mapResponseHeaders $ \headers ->
  contentSecurityPolicy
  : referrerPolicy
  : xContentTypeOptions
  : xFrameOptions
  : headers
  where
    contentSecurityPolicy =
      ("content-security-policy", "default-src 'none'; style-src 'self'; script-src 'self'; img-src 'self'; media-src 'self'; form-action 'self';")
    referrerPolicy =
      ("referrer-policy", "same-origin")
    xContentTypeOptions =
      ("x-content-type-options", "nosniff")
    xFrameOptions =
      ("x-frame-options", "DENY")

phi :: Context -> IO Application
phi context = do
  createTables context
  let app = toWAI context routes
  pure $ middlewares app
  where
    middlewares
      = logStdoutDev
      . cacheStaticFiles
      . limitRequestSize
      . addSecurityHeaders
