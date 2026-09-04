{-# LANGUAGE OverloadedStrings #-}

module Phi where

import           Data.ByteString (isInfixOf)
import qualified Data.Text as T (drop, length, take)
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
  ifRequest isForStaticFiles $
    modifyResponse $ \res ->
      if responseStatus res == ok200
      then mapResponseHeaders (\headers -> header : headers) res
      else res
  where
    isForStaticFiles req = take 2 (pathInfo req) == [".phi", "static"]
    header = ("cache-control", "public, max-age=86400, immutable")

addSecurityHeaders :: Middleware
addSecurityHeaders = modifyResponse . mapResponseHeaders $ \headers ->
  contentSecurityPolicy
  : referrerPolicy
  : xContentTypeOptions
  : xFrameOptions
  : headers
  where
    contentSecurityPolicy =
      ("content-security-policy", "default-src 'none'; style-src 'self'; img-src 'self'; media-src 'self'; form-action 'self';")
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
