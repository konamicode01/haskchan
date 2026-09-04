{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phi.Auth.Crypto where

import           Data.ByteArray (Bytes)
import qualified Data.ByteArray as BA (empty, drop, pack, take, unpack)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack, unpack)
import           Data.ByteString.Base64 (encodeBase64', decodeBase64)

import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Random.Types

concatenate :: IV AES256 -> AuthTag -> Bytes -> ByteString
concatenate iv tag ciphertext = toBase64 $ (toBytes iv) <> (unAuthTag tag) <> ciphertext
  where
    toBase64 = encodeBase64' . BS.pack . BA.unpack
    toBytes = BA.pack . BA.unpack

deconcatenate :: ByteString -> Maybe (IV AES256, AuthTag, Bytes)
deconcatenate token = fromBase64 >>= pure . split >>= construct
  where
    fromBase64 =
      case decodeBase64 token of
        Left _           -> Nothing
        Right bytestring -> Just . BA.pack . BS.unpack $ bytestring
    split bytes =
      ( BA.take ivLength bytes
      , BA.take tagLength . BA.drop ivLength $ bytes
      , BA.drop (ivLength + tagLength) bytes
      )
    construct (ivBytes, tagBytes, ciphertext) =
      case makeIV ivBytes of
        Nothing -> Nothing
        Just iv -> Just (iv, AuthTag tagBytes, ciphertext)

ivLength :: Int
ivLength = 16

tagLength :: Int
tagLength = 16

-- Authenticated plaintext. Not used.
aad :: Bytes
aad = BA.empty

mkCipher :: Bytes -> Maybe AES256
mkCipher key =
  case cipherInit key of
    CryptoPassed cipher' -> Just cipher'
    CryptoFailed _       -> Nothing

mkRandomIV :: forall m. MonadRandom m => m (Maybe (IV AES256))
mkRandomIV = (getRandomBytes ivLength :: m Bytes) >>= pure . makeIV

mkAEAD :: Bytes -> IV AES256 -> Maybe (AEAD AES256)
mkAEAD key iv = do
  cipher <- mkCipher key
  case aeadInit AEAD_GCM cipher iv of
    CryptoPassed aead -> Just aead
    CryptoFailed _    -> Nothing

encrypt :: MonadRandom m => Bytes -> ByteString -> m (Maybe ByteString)
encrypt key plaintext = do
  mIV <- mkRandomIV
  pure $ case mIV of
    Nothing -> Nothing
    Just iv -> do
      let
        mResult = do
          aead <- mkAEAD key iv
          Just $ aeadSimpleEncrypt aead aad (toBytes plaintext) tagLength
      case mResult of
        Just (tag, ciphertext) -> Just $ concatenate iv tag ciphertext
        _                      -> Nothing
  where toBytes = BA.pack . BS.unpack

decrypt :: Bytes -> ByteString -> Maybe ByteString
decrypt key token = do
  (iv, tag, ciphertext) <- deconcatenate token
  aead <- mkAEAD key iv
  plaintext <- aeadSimpleDecrypt aead aad ciphertext tag
  pure . BS.pack . BA.unpack $ plaintext
