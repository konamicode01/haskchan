{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Thread where

import           Control.Monad (when)

import qualified Data.Text as T (null, take)
import qualified Data.Text.Lazy as TL (toStrict)

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseL)
import           Phi.Layout.Components.Board
import           Phi.Layout.Components.Mod
import           Phi.Layout.Components.Post

threadL :: PageDetails -> Board -> OP -> [Reply] -> Html ()
threadL details board (thread, fpost@(post, _mFile, _quotes)) replies =
  baseL details (title_ $ toHtml $ "/" <> uri board <> "/ - " <> T.take 64 titleText) $ do
    bannerL' board
    h1_ [id_ "pagetitle"] $
      toHtml $ "/" <> uri board <> "/ - " <> title board
    when (not . T.null $ description board) $
      h2_ [id_ "pagesubtitle"] $
        toHtml $ description board
    if lock thread == Full
    then h1_ [id_ "threadclosed"] "This thread is full. You can't reply anymore."
    else
      if permission board == NilThreadsNilReplies
      then
        h2_ [id_ "postform-button"] $
          "[" <> a_ [] "Post a Reply" <> "]"
      else do
        h2_ [id_ "postform-button"] $
          "[" <> a_ [href_ "#postform"] "Post a Reply" <> "]"
        postformL' board (Just thread) (captchaBaseline globalsettings)
    threadBoardnavL' board thread True True
    hr_ []
    form_ [id_ "thread", action_ "/.phi/auth/mod", method_ "post"] $ do
      modformTableL'
      opL' (Right thread) fpost
      mconcat $ (flip map) replies $ \reply -> do
        replyL' (Right thread) reply
        hr_ [class_ "invisible"]
    hr_ []
    threadBoardnavL' board thread False (permission board /= NilThreadsNilReplies && lock thread /= Full)
    modbuttonL'
  where
    globalsettings = pdGlobalSettings details
    titleText
      | T.null $ subject post = nomarkup post
      | otherwise             = subject post
