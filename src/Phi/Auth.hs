{-# LANGUAGE OverloadedStrings #-}

module Phi.Auth where

import           Data.Maybe (isJust)
import           Data.Pool (withResource)
import qualified Database.SQLite.Simple as DB

import           Data.ByteArray (Bytes)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import qualified Data.ByteString as BS (drop, replicate, take)
import qualified Data.Binary as Binary (decode, encode)
import           Data.Text (Text)
import           Data.Text as T (dropWhile, length)
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Web.Cookie

import           Phi.Auth.Crypto
import           Phi.Context (Context(..), extractFromCookie)
import           Phi.Database.Models (User(..))

maxAge :: Integral n => n
maxAge = 604800 -- seven days

encode :: Text -> Int -> ByteString
encode username_ expiry =
  expiryBS <> usernameBS
  where
    expiryBS   = BSL.toStrict . Binary.encode $ expiry
    usernameBS = padding <> encodeUtf8 username_
    padding    = BS.replicate (32 - T.length username_) (fromIntegral 0)

decode :: ByteString -> Maybe (Text, Int)
decode plaintext = do
  username_ <- case decodeUtf8' . BS.drop 8 $ plaintext of
    Right username_ -> Just $ strip username_
    Left _         -> Nothing
  Just (username_, expiry)
  where
    expiry    = Binary.decode . BSL.fromStrict $ expiryBS
    expiryBS  = BS.take 8 $ plaintext
    strip     = T.dropWhile (== '\NUL')

mkAuthToken :: Bytes -> Text -> Int -> IO (Maybe ByteString)
mkAuthToken key username_ expiry = encrypt key $ encode username_ expiry

unAuthToken :: Bytes -> ByteString -> Maybe (Text, Int)
unAuthToken key token = decrypt key token >>= decode

mkAuthCookie :: Context -> User -> IO (Maybe ByteString)
mkAuthCookie context user = do
  expiry <- getCurrentTime >>= pure . addUTCTime (fromInteger maxAge) >>= pure . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
  mToken <- mkAuthToken (secret context) (username user) expiry
  pure $ do
    token <- mToken
    let setCookie = def { setCookieName     = "auth"
                        , setCookieValue    = token
                        , setCookiePath     = Just "/"
                        , setCookieMaxAge   = Just . secondsToDiffTime $ maxAge
                        , setCookieHttpOnly = True
                        , setCookieSameSite = Just sameSiteStrict
                        }
    Just . BSL.toStrict . toLazyByteString . renderSetCookie $ setCookie

unAuthCookie :: Context -> ByteString -> IO (Maybe Text)
unAuthCookie context token =
  case unAuthToken (secret context) token of
    Nothing                  -> pure Nothing
    Just (username_, expiry) -> do
      time <- getCurrentTime >>= pure . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
      if time >= expiry
      then pure Nothing
      else do
        mUser <- withResource (db context) $ \conn ->
          DB.query conn
            "SELECT username FROM user WHERE username = ? LIMIT 1"
            (DB.Only username_) :: IO [DB.Only Text]
        pure $ case mUser of
          []    -> Nothing
          (_:_) -> Just username_

elicitUsername :: Context -> IO (Maybe Text)
elicitUsername context =
  case mToken of
    Nothing    -> pure Nothing
    Just token -> unAuthCookie context token
  where
    mToken     = extractFromCookie context "auth"

isLoggedIn :: Context -> IO Bool
isLoggedIn context = isJust <$> elicitUsername context
