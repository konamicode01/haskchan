{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Recent where

import           Data.Maybe (fromMaybe)
import           Data.Default

import qualified Data.Text as T (pack)

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseL)
import           Phi.Layout.Components.Post
import           Phi.Layout.Components.Mod

recentL :: PageDetails -> Bool -> Bool -> [(Board, Bool)] -> [(Maybe Thread, FPost)] -> Html ()
recentL details openFilterDetails whitelist boardFilter tuples =
  baseL details (title_ "Recent") $ do
    h1_ [id_ "pagetitle"] "Recent posts"
    article_ [class_ "container", id_ "recent"] $ do
      details_ (id_ "recentfilter" : if openFilterDetails then [open_ ""] else []) $ do
        summary_ "Filter boards"
        form_ $ do
          div_ [class_ "vertical-margin"] $ do
            div_ "Select one or more boards:"
            select_ [multiple_ "", name_ "board", Lucid.size_ $ T.pack . show $ min 8 (length boardFilter)] $
              mconcat $ (flip map) boardFilter $ \(board, visible) ->
                option_ ((value_ $ uri board) : if visible == whitelist then [selected_ ""] else []) $
                  toHtml $ "/" <> uri board <> "/ - " <> title board
          div_ $ do
            div_ [class_ "vertical-margin"] $
              if whitelist
              then do
                label_ $ input_ [type_ "radio", name_ "show", value_ "0"]           <> "Blacklist"
                " "
                label_ $ input_ [type_ "radio", name_ "show", value_ "1", checked_] <> "Whitelist"
              else do
                label_ $ input_ [type_ "radio", name_ "show", value_ "0", checked_] <> "Blacklist"
                " "
                label_ $ input_ [type_ "radio", name_ "show", value_ "1"]           <> "Whitelist"
            input_ [type_ "submit", value_ "Filter"]
      form_ [action_ "/.phi/auth/mod", method_ "post"] $ do
        modformTableL'
        nav_ [class_ "boardnav"] $
          ul_ [class_ "flat"] $
            li_ $ "[" <> a_ [href_ ""] "Update" <> "]"
        hr_ []
        mconcat $ (flip map) tuples $ \(mThread, fpost@(post, _mFile, _quotes)) -> do
          let
            eNoThread =
              case mThread of
                Nothing     -> Left $ fromMaybe (no post) (pThreadNo post)
                Just thread -> Right thread
          postL' (def {sidearrows = True, parent = True}) eNoThread fpost
          hr_ [class_ "invisible"]
        hr_ []
        nav_ [class_ "boardnav"] $ do
          ul_ [class_ "flat"] $
            li_ $ "[" <> a_ [href_ ""] "Update" <> "]"
          ul_ [class_ "flat"] $
            li_ $ "[" <> a_ [href_ "#top"] "Top" <> "]"
        modbuttonL'
