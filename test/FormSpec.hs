{-# LANGUAGE OverloadedStrings #-}
module FormSpec where

import           Data.Password.Argon2
import qualified Data.Text as T (replicate)
import           Lucid (renderText)

import           Test.Hspec hiding (context)
import           Test.Hspec.Fn

import           Phi.Database.Models
import           Phi.Database.Queries
import           Phi.Forms

import           Common (fnTests)

spec :: Spec
spec = fnTests $ do
  describe "postForm" $ do
    it "should escape html properly" $ do
      let form = postForm "uri" Nothing "name" "email" "subject" "message <>&\"' <script>alert('owned');</script>"
      let Valid newpost = validate form
      renderText (npMessage newpost) `shouldEqual` "message &lt;&gt;&amp;&quot;&#39; &lt;script&gt;alert(&#39;owned&#39;);&lt;/script&gt;"
    it "should test message formatting" $ do
      True `shouldEqual` False
    it "should test insertion of quotelinks" $ do
      True `shouldEqual` False
    it "should have more tests" $ do
      True `shouldEqual` False

  describe "registerForm" $ do
    it "should be validated if its inputs are valid" $ do
      let form = registerForm "username" "password" "password"
      let Valid newuser = validate form
      pwhash_ <- hashPassword (nuPassword newuser)
      nuUsername newuser `shouldEqual` "username"
      checkPassword "password" pwhash_ `shouldEqual` PasswordCheckSuccess

    it "shouldn't be validated if the username is empty" $ do
      let form = registerForm "" "password" "password"
      let Invalid messages = validate form
      messages `shouldEqual` ["Username was empty"]

    it "shouldn't be validated if the username is too long" $ do
      let form = registerForm (T.replicate 64 "z") "password" "password"
      let Invalid messages = validate form
      messages `shouldEqual` ["Username was longer than 32 characters"]

    it "should be validated even if the username is taken" $ do
      (Just (Right ())) <- eval $ \context -> makeUser context $ NewUser "test" "password"
      let form = registerForm "test" "password" "password"
      let Valid newuser = validate form
      pwhash_ <- hashPassword "password"
      nuUsername newuser `shouldEqual` "test"
      checkPassword (nuPassword newuser) pwhash_ `shouldEqual` PasswordCheckSuccess

    it "shouldn't be validated if the password is empty" $ do
      let form = registerForm "username" "" "password"
      let Invalid messages = validate form
      messages `shouldEqual` ["Password was shorter than 8 characters"]

    it "shouldn't be validated if the password is too long" $ do
      let form = registerForm "username" (T.replicate 1025 "p") "password"
      let Invalid messages = validate form
      messages `shouldEqual` ["Password was longer than 1024 characters"]

    it "shouldn't be validated if the password confirmation is too long" $ do
      let form = registerForm "username" "password" (T.replicate 1025 "p")
      let Invalid messages = validate form
      messages `shouldEqual` ["Password confirmation was longer than 1024 characters"]

    it "shouldn't be validated if the passwords don't match" $ do
      let form = registerForm "username" "password" "pass"
      let Invalid messages = validate form
      messages `shouldEqual` ["Passwords did not match"]

    it "should give multiple error messages when it can" $ do
      let form = registerForm (T.replicate 32 "un") "abababab" "babababa"
      let Invalid messages = validate form
      messages `shouldEqual` ["Username was longer than 32 characters", "Passwords did not match"]

  describe "loginForm" $ do
    it "should validate if all the inputs are valid" $ do
      let form = loginForm "test" "password"
      validate form `shouldEqual` Valid ()

    it "shouldn't be validated if the username is too long" $ do
      let form = loginForm (T.replicate 40 "a") "password"
      validate form `shouldEqual` Invalid ["Username was longer than 32 characters"]

    it "should be validated even if the username is wrong (not its concern)" $ do
      let form = loginForm "nil" "password"
      validate form `shouldEqual` Valid ()

    it "should be validated even if the password is wrong (not its concern)" $ do
      let form = loginForm "test" "pw....pw"
      validate form `shouldEqual` Valid ()

    it "shouldn't be validated if the password is too short" $ do
      let form = loginForm "username" "a"
      validate form `shouldEqual` Invalid ["Password was shorter than 8 characters"]

    it "shouldn't be validated if the password is too long" $ do
      let form = loginForm "username" (T.replicate 2048 "s")
      validate form `shouldEqual` Invalid ["Password was longer than 1024 characters"]

    it "should give multiple error messages when it can" $ do
      let form = loginForm "" (T.replicate 4096 "w")
      validate form `shouldEqual` Invalid ["Username was empty", "Password was longer than 1024 characters"]

  describe "boardSettingsForm" $ do
    it "should validate if all the inputs are valid" $ do
      True `shouldEqual` False
    it "should have more tests" $ do
      True `shouldEqual` False

  describe "globalSettingsForm" $ do
    it "should validate if all the inputs are valid" $ do
      True `shouldEqual` False

  describe "makeBoardForm" $ do
    it "should validate if all the inputs are valid" $ do
      True `shouldEqual` False
    it "should have more tests" $ do
      True `shouldEqual` False

  describe "modStickinessForm" $ do
    it "should validate if all the inputs are valid" $ do
      True `shouldEqual` False
    it "should have more tests" $ do
      True `shouldEqual` False

  describe "modForm" $ do
    it "should validate if all the inputs are valid" $ do
      True `shouldEqual` False
    it "should have more tests" $ do
      True `shouldEqual` False
