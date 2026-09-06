{-# LANGUAGE OverloadedStrings #-}

module Phi.Context where

import qualified Data.ByteString as BSL (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           Network.Wai (requestHeaders)
import           Web.Cookie

import           Crypto.Hash (hashWith, SHA3_256(..))
import           Data.ByteArray (Bytes)
import qualified Data.ByteArray as BA (pack, unpack)
import qualified Data.ByteString as BS (readFile)

import           Data.Pool (createPool, Pool)
import qualified Database.SQLite.Simple as DB (close, Connection, open)

import           System.Directory (createDirectoryIfMissing)

import           Web.Fn (defaultFnRequest, FnRequest, RequestContext(..))

data Context = Context
  { request :: FnRequest
  , db :: Pool DB.Connection
  , secret :: Bytes
  , static :: FilePath
  , captcha :: FilePath
  , font :: FilePath
  }

-- This gives each field a name, much more readable.
data Config = Config
  { databaseFile :: FilePath
  , secretFile :: FilePath
  , staticFolder :: FilePath
  , captchaFolder :: FilePath
  , fontFile :: FilePath
  }

instance RequestContext Context where
  getRequest = request
  setRequest context newRequest = context { request = newRequest }

-- The secret is reduced to 256 bits so it can be used as an AES-256 key.
hashFile :: FilePath -> IO Bytes
hashFile filepath = do
  bytestring <- BS.readFile filepath
  let digest = hashWith SHA3_256 bytestring
  pure $ toBytes digest
  where toBytes = BA.pack . BA.unpack

mkContext :: Config -> IO Context
mkContext config = do
  createDirectoryIfMissing True $ staticFolder config
  createDirectoryIfMissing True $ staticFolder config <> "/file"
  createDirectoryIfMissing True $ staticFolder config <> "/thumb"
  createDirectoryIfMissing True $ staticFolder config <> "/banner"
  createDirectoryIfMissing True $ captchaFolder config
  pool <- createPool (DB.open $ databaseFile config) DB.close 1 16 8
  secret_ <- hashFile (secretFile config)
  pure $ Context
    { request = defaultFnRequest
    , db = pool
    , secret = secret_
    , static = staticFolder config
    , captcha = captchaFolder config
    , font = fontFile config
    }

extractFromCookie :: Context -> BSL.ByteString -> Maybe BSL.ByteString
extractFromCookie context key = mValue
  where
    mValue = mCookie >>= lookup key . parseCookies
    mCookie = lookup "Cookie" $ requestHeaders waiRequest
    waiRequest = fst fnRequest
    fnRequest = request context

getReferrer :: Context -> Maybe BSL.ByteString
getReferrer context = mReferrer
  where
    mReferrer = lookup "Referer" $ requestHeaders waiRequest
    waiRequest = fst fnRequest
    fnRequest = request context

getReferrerAsText :: Context -> Maybe Text
getReferrerAsText = maybe Nothing decode . getReferrer
  where decode = either (const Nothing) Just . decodeUtf8'

requestOrigin :: Context -> Text
requestOrigin context =
  case getHost context of
    Just host
      | T.isSuffixOf ".onion" host -> "tor"
      | T.isSuffixOf ".i2p" host   -> "i2p"
      | otherwise                  -> "clearnet"
    Nothing -> "clearnet"
  where
    getHost ctx =
      case lookup "Host" $ requestHeaders waiRequest of
        Nothing -> Nothing
        Just value ->
          either (const Nothing) Just (decodeUtf8' value)

    waiRequest = fst $ request context
