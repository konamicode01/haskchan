{-# LANGUAGE OverloadedStrings #-}

module Phi.Routes.Meta.Varstatic where

import           Data.Maybe (isJust)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (isInfixOf)
import           Data.Text (Text)
import qualified Data.Text as T (all, unpack)
import           Data.Text.Encoding (encodeUtf8)

import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai (Response, mapResponseHeaders, mapResponseStatus, requestHeaders)
import           Web.Fn hiding (okHtml)

import           Phi.Context (Context(request, static))
import           Phi.Database.Models
import           Phi.Database.Queries
import           Phi.Database.Queries.Types
import           Phi.Routes.Errors

-- Nondeterministically serve static content, e.g. randomly choosing a banner
-- or basing the response off the request's Accept header.
varstaticH :: Context -> IO (Maybe Response)
varstaticH context = route context
  [ path "banner" // segment ==> bannerH
  , path "thumb"  // segment ==> thumbH
  ]

bannerH :: Context -> Text -> IO (Maybe Response)
bannerH context uri_ = do
  mReceipt <- getRandomBanner context uri_
  case mReceipt of
    Nothing      -> errorH conflict409 "Error fetching banner"
    Just receipt ->
      case receipt of
        Left  NoSuchBoard   -> errorH notFound404 "No such board"
        Right Nothing       -> transform $ redirect "/.phi/static/antibanner.png"
        Right (Just banner) -> transform $ redirect $ "/.phi/static/banner/" <> bnBoardUri banner <> "/" <> bnHash banner <> bnExt banner
  where
    cacheControl = ("cache-control", "public, max-age=30, immutable")
    transform getMResponse = do
      mResponse <- getMResponse
      pure $ mapResponseHeaders (cacheControl :) <$> mapResponseStatus (const permanentRedirect308) <$> mResponse

thumbH :: Context -> Text -> IO (Maybe Response)
thumbH context hash_ =
  if accepted context "image/webp"
  then sendNamedFile (mkFilename ".webp") (mkSrc ".webp")
  else do
    mResponse <- sendNamedFile (mkFilename ".jpg") (mkSrc ".jpg")
    if isJust mResponse
    then pure $ mResponse
    else sendNamedFile (mkFilename ".png") (mkSrc ".png")
  where
    mkFilename ext_ = hash_ <> ext_
    mkSrc ext_      = static context <> "/thumb/" <> (T.unpack $ mkFilename ext_)

sendNamedFile :: Text -> FilePath -> IO (Maybe Response)
sendNamedFile filename src
  | T.all inrange filename = do
    mResponse <- sendFile src
    case mResponse of
      Nothing       -> pure Nothing
      Just response -> pure $ Just $ mapResponseHeaders ((cacheControl :) . (contentDisposition :)) response
  | otherwise = sendFile src
  where
    inrange char = char >= 'a' && char <= 'z' || char >= '0' && char <= '9' || char == '.'
    cacheControl       = ("cache-control", "public, max-age=86400, immutable")
    contentDisposition = ("content-disposition", "inline; filename=\"" <> encodeUtf8 filename <> "\"")

-- Check whether a given string is present in the Accept header.
accepted :: Context -> ByteString -> Bool
accepted context mime_ =
  case mAccept of
    Nothing     -> False
    Just accept -> mime_ `BS.isInfixOf` accept
  where
    mAccept    = lookup "Accept" headers
    headers    = requestHeaders waiRequest
    waiRequest = fst fnRequest
    fnRequest  = request context
