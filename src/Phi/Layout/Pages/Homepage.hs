{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Homepage where

import           Data.Text (Text)
import qualified Data.Text as T (pack, take)
import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseL)

homepageL :: PageDetails -> [Board] -> [Post] -> [Post] -> Html ()
homepageL details boards teaserImagePosts teaserPosts =
  baseL details (title_ "Homepage") $ do
    article_ [id_ "home", class_ "container"] $ do
      div_ [id_ "home-branding"] $ do
        img_ [id_ "home-logo", src_ "/.phi/static/haskchan_logo_wide.png", alt_ "Haskchan"]
        h1_ [id_ "pagetitle"] "Haskchan"
      header_ [class_ "barheader"] "Boards"
      section_ $
        ul_ [id_ "boards"] $
          mconcat $ (flip map) boards $ \board -> do
            li_ $
              a_ [href_ $ "/" <> uri board <> "/", title_ $ title board] $
                "/" <> (toHtml $ uri board) <> "/ - " <> (toHtml $ title board)
      section_ [id_ "teasers"] $ do
        div_ [id_ "teaser-images"] $ do
          header_ [class_ "barheader"] "Recent images"
          mconcat $ (flip map) teaserImagePosts $ \post ->
            a_ [href_ $ url post] $
              (flip $ maybe "") (fileHash post) $ \filehash ->
                img_ [class_ "teaser-image", src_ $ "/.phi/varstatic/thumb/" <> filehash]
        div_ [id_ "teaser-posts"] $ do
          header_ [class_ "barheader"] "Recent posts"
          ul_ $
            mconcat $ (flip map) teaserPosts $ \post ->
              li_ [class_ "teaser-post"] $ do
                let boardTitle = maybe "" title $ lookup (pBoardUri post) boardsByUri
                a_ [class_ "teaser-post-board", href_ $ "/" <> pBoardUri post <> "/", title_ boardTitle] $
                  toHtml $ "/" <> pBoardUri post <> "/"
                ":" <> toHtmlRaw ("&nbsp;" :: Text)
                a_ [class_ "teaser-post-message", href_ $ url post] $
                  toHtml $ T.take 224 $ nomarkup post
  where
    boardsByUri = [(uri board, board) | board <- boards]
    url post =
      case pThreadNo post of
        Nothing       -> "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ no post)
        Just threadNo -> "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ threadNo) <> "#post" <> (T.pack . show $ no post)
