{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Log where

import           Prelude hiding (log)

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)

import           Lucid hiding (size_)

import           Phi.Database.Models
import           Phi.Layout.Base (baseL)
import           Phi.Layout.Components.Post (verboseDatetime)

logL :: PageDetails -> [Log] -> Html ()
logL details logs =
  baseL details (title_ "Log") $ do
    h1_ [id_ "pagetitle"] "Log"
    article_ [id_ "log", class_ "container"] $
      table_ [id_ "logtable"] $ do
        thead_ $
          tr_ $ do
            th_ "#"
            th_ "Datetime"
            th_ "User"
            th_ "Subject"
            th_ "Action"
            th_ "Reason"
        tbody_ $ do
          mconcat $ (flip map) logs $ \log ->
            tr_ $ do
              td_ $ toHtml . show $ logId log
              td_ $ time_ [datetime_ $ verboseDatetime $ logDatetime log] $ toHtml $ logDatetime log
              td_ $ toHtml $ logUsername log
              td_ $
                case logMPostNo log of
                  Nothing  -> ">>>/" <> (toHtml $ logBoardUri log) <> "/"
                  Just no_ -> ">>>/" <> (toHtml $ logBoardUri log) <> "/" <> (toHtml $ show no_)
              td_ $
                case logAction log of
                  SetStickinessTo n               -> if n > 0 then "Stickied (at " <> (toHtml . show $ n) <> ")" else "Unstickied"
                  SetCyclicTo bool                -> if bool then "Cycled" else "Uncycled"
                  SetLockTo bool                  -> if bool then "Locked" else "Unlocked"
                  SetBumplockTo bool              -> if bool then "Bumplocked" else "Unbumplocked"
                  DidUnlinkFile hash_ size_ mMime -> "Unlinked " <> abbr_ [title_ $ "sha3=" <> hash_ <> " size=" <> (T.pack . show $ size_) <> " mime=" <> fromMaybe "unk" mMime] "file"
                  DidPurgeFile hash_ size_ mMime  -> "Purged " <> abbr_ [title_ $ "sha3=" <> hash_ <> " size=" <> (T.pack . show $ size_) <> " mime=" <> fromMaybe "unk" mMime] "file"
                  DidDeletePost                   -> "Deleted post"
                  DidDeleteThread                 -> "Deleted thread"
                  DidDeleteBoard                  -> "Deleted board"
              td_ $ toHtml $ logReason log
