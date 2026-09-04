{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Index where

import           Control.Monad (when)
import           Data.Maybe (isNothing)

import qualified Data.Text as T (null, pack)

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseL)
import           Phi.Layout.Components.Board
import           Phi.Layout.Components.Mod
import           Phi.Layout.Components.Post

indexL :: PageDetails -> Board -> [Hull] -> Int -> Int -> Html ()
indexL details board hulls nPages page =
  baseL details (title_ $ "/" <> (toHtml $ uri board) <> "/ - index") $ do
    bannerL' board
    h1_ [id_ "pagetitle"] $
      toHtml $ "/" <> uri board <> "/ - " <> title board
    when (not . T.null $ description board) $
      h2_ [id_ "pagesubtitle"] $
        toHtml $ description board
    when (permission board `elem` [AnyThreadsAnyReplies, ModThreadsAnyReplies]) $ do
      h2_ [id_ "postform-button"] $
        "[" <> a_ [href_ "#postform"] "Start a New Thread" <> "]"
      postformL' board Nothing (captchaBaseline globalsettings)
    indexBoardnavL' board nPages page
    hr_ []
    form_ [id_ "index-threads", action_ "/.phi/auth/mod", method_ "post"] $ do
      modformTableL' (pdLoggedIn details)
      mconcat $ (flip map) hulls $ \((thread, fpost), replies) -> do
        indexOpL' (Right thread) fpost
        let nOmittedReplies = nReplies thread - length replies
            nOmittedFiles   = nFiles thread - foldr countFile 0 replies - countFile fpost 0
          in
            when (nOmittedReplies > 0) $
              div_ [class_ "omitted"] $ do
                (toHtml . show $ nOmittedReplies) <> " replies "
                when (nOmittedFiles > 0) $
                  "and " <> (toHtml . show $ nOmittedFiles) <> " files "
                "omitted. "
                a_ [href_ $ "/" <> uri board <> "/thread/" <> (T.pack . show $ tPostNo thread)] "Click here"
                " to see them."
        mconcat $ (flip map) replies $ \reply -> do
          replyL' (Right thread) reply
          hr_ [class_ "invisible"]
        hr_ []
    indexBoardnavL' board nPages page
    modbuttonL' (pdLoggedIn details)
  where
    globalsettings = pdGlobalSettings details
    countFile _fpost@(post, _mFile, _quotes) = if isNothing $ fileHash post then id else (+1)
