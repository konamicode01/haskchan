{-# LANGUAGE OverloadedStrings #-}

module QuerySpec where

import Data.Password.Argon2

import Test.Hspec hiding (context)
import Test.Hspec.Fn

import Phi.Database.Models
import Phi.Database.Queries
import Phi.Database.Queries.Types

import Common (fnTests)

spec :: Spec
spec = fnTests $ do
  describe "getPageDetails" $ do
    it "should do what it's supposed to" $ do
      True `shouldEqual` False

  describe "makeUser" $ do
    it "should make a user" $ do
      let newuser = NewUser "Username" "Password"
      Just (Right ()) <- eval $ \context -> makeUser context newuser
      pure ()
    it "shouldn't make a user if the username is taken ignoring case" $ do
      let newuser = NewUser "username" "pw"
      Just receipt <- eval $ \context -> makeUser context newuser
      receipt `shouldEqual` Left ExtantUser
    it "shouldn't make a user if registration is closed" $ do
      True `shouldEqual` False

  describe "getUserDuringLogin" $ do
    it "should get a user" $ do
      Just (Right user) <- eval $ \context -> getUserDuringLogin context "username" "Password"
      username user `shouldEqual` "Username"
      checkPassword "Password" (pwhash user) `shouldEqual` PasswordCheckSuccess
    it "should ignore case" $ do
      Just (Right user) <- eval $ \context -> getUserDuringLogin context "UsErnAmE" "Password"
      username user `shouldEqual` "Username"
    it "shouldn't get a user that doesn't exist" $ do
      Just (Left NoSuchUser) <- eval $ \context -> getUserDuringLogin context "x" "x"
      pure ()
    it "shouldn't get a user if the provided password is wrong" $ do
      Just (Left WrongPassword) <- eval $ \context -> getUserDuringLogin context "username" "password"
      pure ()
    it "should update the user's last_active datetime" $ do
      Just (Right user1) <- eval $ \context -> getUserDuringLogin context "username" "Password"
      Just (Right user2) <- eval $ \context -> getUserDuringLogin context "username" "Password"
      (lastActive user1 < lastActive user2) `shouldEqual` True

  describe "makeBoard" $ do
    it "should make a new board" $ do
      Just receipt <- eval $ \context -> makeBoard context "username" $ NewBoard "test" "aaaa" "bbbb"
      receipt `shouldEqual` Right ()
      Just [board] <- eval $ getBoards
      uri board                `shouldEqual` "test"
      title board              `shouldEqual` "aaaa"
      description board        `shouldEqual` "bbbb"
      mTheme board             `shouldEqual` Nothing
      anonName board           `shouldEqual` "Anonymous"
      bumpLimit board          `shouldEqual` 256
      replyLimit board         `shouldEqual` 512
      threadLimit board        `shouldEqual` 1024
      indexViewPolicy board    `shouldEqual` IndexViewDisallowed
    it "shouldn't make a new board if the uri is taken" $ do
      Just receipt <- eval $ \context ->
        makeBoard context "username" $ NewBoard "test" "cccc" "dddd"
      receipt `shouldEqual` Left (Right ExtantBoard)
    it "should test when the user doesn't exist" $ do
      True `shouldEqual` False
    it "should test when the user isn't allowed to create boards" $ do
      True `shouldEqual` False

  describe "getBoardAndModNames" $ do
    it "should get a board" $ do
      True `shouldEqual` False
    it "shouldn't get a board that doesn't exist" $ do
      True `shouldEqual` False
    it "should get the names of the board's mods" $ do
      True `shouldEqual` False
    it "should test powerlevels" $ do
      True `shouldEqual` False

  describe "getBoards" $ do
    it "should get all the boards" $ do
      Just [board] <- eval $ getBoards
      uri board `shouldEqual` "test"

  describe "getBoard" $ do
    it "should get a board" $ do
      Just (Right board) <- eval $ \context -> getBoard context "test"
      uri board `shouldEqual` "test"

  describe "getRecent" $ do
    it "should get the most recent posts" $ do
      True `shouldEqual` False
    it "should be able to filter boards" $ do
      True `shouldEqual` False

  describe "makeReply" $ do
    it "should make a reply" $ do
      True `shouldEqual` False
    it "should replace the empty name with the board's default anon name" $ do
      True `shouldEqual` False
    it "should increment the thread's n_replies" $ do
      True `shouldEqual` False
    it "should increment the thread's n_files if the post has a file" $ do
      True `shouldEqual` False
    it "shouldn't increment the thread's n_files if the post doesn't have a file" $ do
      True `shouldEqual` False
    it "should increment the board's total_posts" $ do
      True `shouldEqual` False
    it "should not bump a bumplocked thread" $ do
      True `shouldEqual` False
    it "should not bump the thread if the post's name was sage" $ do
      True `shouldEqual` False
    it "should fail if the thread is locked" $ do
      True `shouldEqual` False
    it "should fail if the thread is locked and bumplocked" $ do
      True `shouldEqual` False
    it "should fail if the thread is locked and cyclic" $ do
      True `shouldEqual` False
    it "should fail if the thread is full" $ do
      True `shouldEqual` False
    it "should test files" $ do
      True `shouldEqual` False
    it "should delete the earliest reply to the thread if the thread is cyclic and the bump limit has been reached" $ do
      True `shouldEqual` False
    it "should not log when a post is deleted by thread cycle" $ do
      True `shouldEqual` False
    it "should test replacement of quotelinks" $ do
      True `shouldEqual` False
    it "should given a message with a quotelink to the same thread insert a row into the quote table" $ do
      True `shouldEqual` False
    it "should given a message with a quotelink to a different thread not insert a row into the quote table" $ do
      True `shouldEqual` False
    it "should test tripcode creation" $ do
      True `shouldEqual` False
    it "should test capcode creation" $ do
      True `shouldEqual` False
    it "should fail for the appropriate values of BoardPermission" $ do
      True `shouldEqual` False

  describe "makeOp" $ do
    it "should make a post and a thread" $ do
      True `shouldEqual` False
    it "should replace the empty name with the board's default anon name" $ do
      True `shouldEqual` False
    it "should increment the board's total_posts" $ do
      True `shouldEqual` False
    it "should test files" $ do
      True `shouldEqual` False
    it "should set n_files to 1 if the post has a file" $ do
      True `shouldEqual` False
    it "should set n_files to 0 if the post doesn't have a file" $ do
      True `shouldEqual` False
    it "should test replacement of quotelinks" $ do
      True `shouldEqual` False
    it "should given a message with a quotelink to a different thread not insert a row into the quote table" $ do
      True `shouldEqual` False
    it "should test tripcode creation" $ do
      True `shouldEqual` False
    it "should test capcode creation" $ do
      True `shouldEqual` False
    it "should fail for the appropriate values of BoardPermission" $ do
      True `shouldEqual` False

  describe "getImplicit" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "getCatalogue" $ do
    it "should have more tests" $ do
      True `shouldEqual` False
    it "should order threads by stickiness, bump datetime, then post no" $ do
      True `shouldEqual` False

  describe "getIndex" $ do
    it "should have more tests" $ do
      True `shouldEqual` False
    it "should order threads by stickiness, bump datetime, then post no" $ do
      True `shouldEqual` False
    it "should give back full threads iff they are stickied" $ do
      True `shouldEqual` False

  describe "getThreadPosts" $ do
    it "should get a thread's posts" $ do
      True `shouldEqual` False
    it "should return FutureThread for threads that don't exist yet" $ do
      True `shouldEqual` False
    it "should return DeletedThread for threads that will never exist" $ do
      True `shouldEqual` False
    it "should order posts by post no" $ do
      True `shouldEqual` False
    it "should test quoted posts" $ do
      True `shouldEqual` False
    it "should have more tests" $ do
      True `shouldEqual` False

  describe "setBoardSettings" $ do
    it "should have tests" $ do
      True `shouldEqual` False
    it "should give back NotAUser if the slated mod doesn't exist" $ do
      True `shouldEqual` False
    it "should give back AlreadyAMod if the slated mod is already a mod" $ do
      True `shouldEqual` False

  describe "getGlobalSettings" $ do
    it "should get the global settings" $ do
      True `shouldEqual` False

  describe "setGlobalSettings" $ do
    it "should set the global settings if the user doing it is an admin" $ do
      True `shouldEqual` False
    it "shouldn't set the global settings if the user trying to do it isn't an admin" $ do
      True `shouldEqual` False

  describe "getUser" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "checkPowerlevelForPost" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "checkPowerlevelForThread" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "checkPowerlevelForBoard" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "moderateThread" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "moderatePost" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "moderatePostFile" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "setThreadStickiness" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "setThreadLockBit" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "unlinkPostFile" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "purgePostFile" $ do
    it "should have tests" $ do
      True `shouldEqual` False

  describe "deletePostOrThread" $ do
    it "should have tests" $ do
      True `shouldEqual` False
