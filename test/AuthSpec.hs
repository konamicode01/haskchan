{-# LANGUAGE OverloadedStrings #-}

module AuthSpec where

import qualified Data.ByteString as BS (length)
import           Data.ByteString.Base64 (isBase64)

import           Test.Hspec hiding (context)
import           Test.Hspec.Fn

import           Phi.Auth
import           Phi.Context (Context(secret))

import           Common (fnTests)

spec :: Spec
spec = fnTests $ do
  describe "mkAuthToken" $ do
    it "should make a 96-length base64 token" $ do
      (Just token) <- eval $ \context -> mkAuthToken (secret context) "abcd" 1234
      isBase64 token `shouldEqual` True
      BS.length token `shouldEqual` 96
    it "shouldn't make the same token twice" $ do
      (Just token1) <- eval $ \context -> mkAuthToken (secret context) "aabb" 1122
      (Just token2) <- eval $ \context -> mkAuthToken (secret context) "aabb" 1122
      (token1 == token2) `shouldEqual` False

  describe "unAuthToken" $ do
    it "should decode tokens properly" $ do
      (Just token) <- eval $ \context -> mkAuthToken (secret context) "abab" 1212
      mAuth <- eval $ \context -> pure $ unAuthToken (secret context) token
      mAuth `shouldEqual` Just ("abab", 1212)
    it "shouldn't decode malformed tokens" $ do
      mAuth <- eval $ \context -> pure $ unAuthToken (secret context) "Malformed/Token"
      mAuth `shouldEqual` Nothing
