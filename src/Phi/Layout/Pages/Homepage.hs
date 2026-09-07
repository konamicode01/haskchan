{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Homepage where

import           Data.Text (Text)
import qualified Data.Text as T (null, pack, replicate, take)
import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseL)
import           Phi.Layout.Attributes (loading_)

homepageL :: PageDetails
          -> [Board]
          -> [Post]
          -> [Post]
          -> [(Text, Int)]
          -> [(Text, Int, Int, Int)]
          -> Html ()
homepageL details boards teaserImagePosts teaserPosts postActivity boardStats =
  baseL details (title_ "Haskchan") $ do

    article_ [id_ "hc-home"] $ do

      -- Intro
      section_ [id_ "hc-intro"] $ do
        img_
          [ id_ "hc-logo"
          , src_ "/.phi/static/haskchan_logo_wide.png"
          , alt_ "Haskchan"
          ]

        h1_ "Haskchan"

        p_ "A lightweight anonymous imageboard written in Haskell."

        div_ [id_ "hc-intro-links"] $ do
          div_ $ do
            "Check out the "
            a_ [href_ "#hc-boards"] "board list"

          div_ $ do
            "View "
            a_ [href_ "/.phi/recent"] "recent posts"

          div_ $ do
            "View "
            a_ [href_ "/recent"] "recent activity"

          div_ $
            "Anonymous discussion across the Haskchan network."

      -- Statistics
      section_ [id_ "hc-stats"] $ do

        div_ [class_ "hc-stat-panel"] $ do
          h2_ "Post Activity"

          table_ [id_ "hc-post-graph"] $ do
            thead_ $
              tr_ $ do
                th_ "Day"
                th_ "Posts"
                th_ "Activity"

            tbody_ $
              mconcat $
                map activityRow (take 14 postActivity)

        div_ [class_ "hc-stat-panel"] $ do
          h2_ "Board Statistics"

          table_ [id_ "hc-board-graph"] $ do
            thead_ $
              tr_ $ do
                th_ "Board"
                th_ "/hr"
                th_ "Today"
                th_ "Total"

            tbody_ $
              mconcat $
                map boardStatsRow boardStats

      -- Boards
      section_ [id_ "hc-boards"] $ do
        h2_ "Boards"

        ul_ [id_ "hc-board-list"] $
          mconcat $
            map boardRow boards

      -- Recent content
      section_ [id_ "hc-recent"] $ do

        div_ [id_ "hc-recent-images"] $ do
          h2_ "Recent Images"

          div_ [id_ "hc-image-grid"] $
            mconcat $
              map recentImage teaserImagePosts

        div_ [id_ "hc-recent-posts"] $ do
          h2_ "Recent Posts"

          ul_ [id_ "hc-post-list"] $
            mconcat $
              map recentPost teaserPosts

      -- Network
      section_ [id_ "hc-network"] $ do
        h2_ "Network"

        div_ [id_ "hc-network-grid"] $ do

          networkLink
            "Clearnet"
            "https://haskchan.me/"
            "/.phi/static/clearnet.png"

          networkLink
            "Tor"
            "http://gbwfhx32yach3i6puu3cl2avq7rl4uqq6bto2723hdrpuflktmioz2yd.onion/"
            "/.phi/static/tor.png"

          networkLink
            "I2P"
            "http://haskchan.i2p/"
            "/.phi/static/i2p.png"

      -- Footer
      footer_ [id_ "hc-footer"] $ do
        hr_ []

        p_ $
          "Haskchan is an open-source anonymous imageboard engine focused on "
          <> "simplicity, self-hosting, and fast discussion."

        p_ "Built with Haskell for independent communities."

        p_ [class_ "hc-footer-links"] $ do
          a_ [href_ "/"] "Home"
          " · "
          a_ [href_ "/.phi/recent"] "Recent"
          " · "
          a_ [href_ "#hc-boards"] "Boards"

  where

    boardRow :: Board -> Html ()
    boardRow board =
      li_ $
        a_
          [ href_ $ "/" <> uri board <> "/"
          , title_ $ title board
          ]
          $ do
            toHtml $ "/" <> uri board <> "/"
            " - "
            toHtml $ title board

    recentImage :: Post -> Html ()
    recentImage post =
      case fileHash post of
        Nothing -> pure ()
        Just filehash ->
          a_ [class_ "hc-image", href_ $ url post] $
            img_
              [ src_ $ "/.phi/varstatic/thumb/" <> filehash
              , alt_ "Recent image"
              , loading_ "lazy"
              ]

    recentPost :: Post -> Html ()
    recentPost post =
      li_ [class_ "hc-post"] $ do
        a_
          [ class_ "hc-post-board"
          , href_ $ "/" <> pBoardUri post <> "/"
          ]
          (toHtml $ "/" <> pBoardUri post <> "/")

        a_
          [ class_ "hc-post-message"
          , href_ $ url post
          ]
          (toHtml $ T.take 224 (nomarkup post))

    activityRow :: (Text, Int) -> Html ()
    activityRow (day_, posts_) =
      tr_ $ do
        td_ $ toHtml day_
        td_ $ toHtml (show posts_)
        td_ [class_ "hc-activity-cell"] $
          span_
            [class_ "hc-activity-bar"]
            (toHtml $ activityBar posts_ (maximumActivity postActivity))

    activityBar :: Int -> Int -> Text
    activityBar posts_ maxPosts
      | posts_ <= 0 = ":0"
      | maxPosts <= 0 = ":0"
      | otherwise =
          ":0"
          <> T.replicate barLength "="
          <> T.pack (show posts_)
      where
        barLength =
          max 1 $
            round
              (fromIntegral posts_ / fromIntegral maxPosts * 28 :: Double)

    maximumActivity :: [(Text, Int)] -> Int
    maximumActivity [] = 0
    maximumActivity xs = maximum (map snd xs)

    boardStatsRow :: (Text, Int, Int, Int) -> Html ()
    boardStatsRow (boardUri, perHour, today, total) =
      tr_ $ do
        td_ $
          a_
            [href_ $ "/" <> boardUri <> "/"]
            (toHtml $ "/" <> boardUri <> "/")
        td_ $ toHtml (show perHour)
        td_ $ toHtml (show today)
        td_ $ toHtml (show total)

    networkLink :: Text -> Text -> Text -> Html ()
    networkLink name address icon =
      a_ [class_ "hc-network-card", href_ address] $ do
        img_
          [ class_ "hc-network-icon"
          , src_ icon
          , alt_ ""
          ]
        div_ $ do
          span_ [class_ "hc-network-name"] $ toHtml name
          span_ [class_ "hc-network-address"] $ toHtml address

    url :: Post -> Text
    url post =
      case pThreadNo post of
        Nothing ->
          "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ no post)

        Just threadNo ->
          "/"
          <> pBoardUri post
          <> "/thread/"
          <> (T.pack . show $ threadNo)
          <> "#post"
          <> (T.pack . show $ no post)
