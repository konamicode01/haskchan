{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Catalogue where

import           Control.Monad (when)
import qualified Data.Text as T (null, pack)

import           Lucid

import           Phi.Database.Models
import           Phi.Files.Thumbnails (shrink)
import           Phi.Layout.Attributes (loading_)
import           Phi.Layout.Base (baseL)
import           Phi.Layout.Components.Board
import           Phi.Layout.Components.Mod
import           Phi.Layout.Components.Post

catalogueL :: PageDetails -> Board -> [OP] -> Html ()
catalogueL details board ops =
  baseL details (title_ $ "/" <> (toHtml $ uri board) <> "/ - catalogue") $ do
    bannerL' board
    h1_ [id_ "pagetitle"] $
      toHtml $ "/" <> uri board <> "/ - " <> title board
    when (not . T.null $ description board) $
      h3_ [id_ "pagesubtitle"] $
        toHtml $ description board
    when (permission board `elem` [AnyThreadsAnyReplies, ModThreadsAnyReplies]) $ do
      h2_ [id_ "postform-button"] $
        "[" <> a_ [href_ "#postform"] "Start a New Thread" <> "]"
      postformL' board Nothing (captchaBaseline globalsettings)
    catalogueBoardnavL' board
    hr_ []
    form_ [id_ "catalogue-threads", action_ "/.phi/auth/mod", method_ "post"] $ do
      modformTableL'
      mconcat $ (flip map) ops $ \(thread, (post, mFile, _quotes)) -> do
        article_ [class_ "catalogue-thread"] $ do
          header_ [class_ "catalogue-thread-header"] $ do
            let
              (threadUrl, thumbUrl) =
                ( "/" <> uri board <> "/thread/" <> (T.pack . show $ no post)
                , \file -> if hasThumb file then "/.phi/varstatic/thumb/" <> hash file else "/.phi/static/antithumb.png"
                ) in
              case mFile of
                Nothing ->
                  a_ [class_ "catalogue-thread-nofile", href_ threadUrl] "***"
                Just file -> do
                  let mThumbWidthHeight = (,) <$> thumbWidth file <*> thumbHeight file
                      mIconWidthHeight = (\(w, h) -> shrink 150 w h) <$> mThumbWidthHeight
                      attributes =
                        case mIconWidthHeight of
                          Nothing -> []
                          Just (iconWidth, iconHeight) -> [width_ $ T.pack . show $ iconWidth, height_ $ T.pack . show $ iconHeight]
                  a_ [class_ "catalogue-thread-file", href_ threadUrl] $
                    img_ (class_ "catalogue-thread-file-icon" : (src_ $ thumbUrl file) : loading_ "lazy" : attributes)
            div_ [class_ "catalogue-thread-details"] $ do
              a_ [class_ "catalogue-thread-boardlink", href_ $ "/" <> uri board <> "/"] $
                "/" <> toHtml (uri board) <> "/"
              " "
              span_ [class_ "catalogue-thread-reply-count"] $
                "R:" <> (toHtml . show $ nReplies thread)
              when (hasThreadSettings thread) $
                span_ [class_ "catalogue-thread-settings"] $
                  threadSettingsL' thread
            div_ [class_ "catalogue-thread-last-post"] $ do
              "L: "
              time_ [datetime_ $ verboseDatetime (lastActivity thread)] $
                toHtml $ lastActivity thread
            when (not . T.null $ subject post) $
              div_ [class_ "catalogue-thread-subject"] $
                toHtml $ subject post
          blockquote_ [class_ "catalogue-thread-message"] $
            message post
        hr_ [class_ "invisible"]
    modbuttonL'
  where
    globalsettings = pdGlobalSettings details
