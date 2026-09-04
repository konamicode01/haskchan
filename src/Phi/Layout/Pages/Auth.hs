{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Auth where

import Control.Monad (when)

import Lucid

import Phi.Database.Models
import Phi.Layout.Base (baseL)

authHomeL :: PageDetails -> User -> [Board] -> [(Board, Bool)] -> [Board] -> Html ()
authHomeL details user ownedBoards modBoards otherBoards = do
  baseL details (title_ "Mod homepage") $ do
    h1_ [id_ "pagetitle"] $ "Welcome, " <> (toHtml $ username user)
    article_ [class_ "container"] $ do
      header_ [class_ "barheader"] "Do something"
      ul_ $ do
        li_ $
          form_ [action_ "/.phi/auth/logout", method_ "post"] $
            input_ [type_ "submit", value_ "Log out"]
        when (userBoardCreation globalsettings || admin user) $
          li_ $ a_ [href_ "/.phi/auth/board/create"] "Make a board"
        when (admin user) $
          li_ $ a_ [href_ "/.phi/auth/settings"] "Change global settings"
      header_ [class_ "barheader"] "Boards you own"
      list $ map (\board -> (board, True)) ownedBoards
      header_ [class_ "barheader"] "Boards you moderate"
      list modBoards
      when (not . null $ otherBoards) $ do
        header_ [class_ "barheader"] "Other boards"
        list $ map (\board -> (board, True)) otherBoards
  where
    globalsettings = pdGlobalSettings details

    list :: [(Board, Bool)] -> Html ()
    list []      = p_ "Nothing here."
    list boards' = do
      ul_ $
        mconcat $ (flip map) boards' $ \(board, showSettings) ->
          li_ $ do
            a_ [href_ $ "/" <> uri board <> "/"] $ "/" <> (toHtml $ uri board) <> "/"
            " - "
            a_ [href_ $ "/" <> uri board <> "/catalogue"] "catalogue"
            " "
            a_ [href_ $ "/" <> uri board <> "/index"] "index"
            when showSettings $ do
              " "
              a_ [href_ $ "/.phi/auth/board/settings/" <> uri board <> "/"] "settings"
              " "
              a_ [href_ $ "/.phi/auth/board/banners/" <> uri board <> "/"] "banners"
