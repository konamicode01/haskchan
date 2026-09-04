{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Forms.BoardSettings where

import           Control.Monad (when)

import           Data.Text (Text)
import qualified Data.Text as T (pack)

import           Lucid

import           Phi.Database.Models
import           Phi.Database.Queries.Types
import           Phi.Layout.Base (baseL)

boardSettingsPromptL :: PageDetails -> Powerlevel -> Board -> [(Text, Bool)] -> Html ()
boardSettingsPromptL details powerlevel board modtuples =
  baseL details (title_ $ "/" <> toHtml (uri board) <> "/ - settings") $ do
    h1_ [id_ "pagetitle"] "Board settings"
    h4_ [id_ "pagesubtitle"] $
      case powerlevel of
        Admin        -> "You are an admin. Admins can change settings, add/remove mods, and delete the board."
        BoardOwner   -> "You are the board owner. Board owners can change settings, add/remove mods, and delete the board."
        BoardManager -> "You are a board manager. Board managers can change settings and add/remove mods."
        _            -> "You are not a board manager. You cannot change settings."
    article_ [class_ "container"] $ do
      form_ [method_ "post", action_ $ "/.phi/auth/board/settings/" <> uri board] $ do
        table_ [class_ "formtable"] $ do
          tr_ $ do
            td_ "URI"
            td_ $ input_ [value_ $ uri board, disabled_ ""]

          tr_ $ do
            td_ "Title"
            td_ $ input_ [name_ "title", value_ $ title board, required_ "", maxlength_ "32"]

          tr_ $ do
            td_ "Description"
            td_ $ input_ [name_ "description", value_ $ description board, maxlength_ "128"]

          tr_ $ do
            td_ "Theme"
            td_ $
              select_ [name_ "theme"] $ do
                option_ (value_ ""  : (selectTheme Nothing))         "-- Global theme --"
                option_ (value_ "0" : selectTheme (Just Phichannel)) "Phichannel"
                option_ (value_ "1" : selectTheme (Just Nanochan))   "Nanochan"
                option_ (value_ "2" : selectTheme (Just Yotsuba))    "Yotsuba"
                option_ (value_ "3" : selectTheme (Just Haskchan))   "Haskchan"

          tr_ $ do
            td_ "Anon name"
            td_ $ input_ [name_ "anon-name", value_ $ anonName board]

          tr_ $ do
            td_ "Bump limit"
            td_ $ input_ [name_ "bump-limit", type_ "number", value_ $ T.pack . show $ bumpLimit board]

          tr_ $ do
            td_ "Reply limit"
            td_ $ input_ [name_ "reply-limit", type_ "number", value_ $ T.pack . show $ replyLimit board]

          tr_ $ do
            td_ "Thread limit"
            td_ $ input_ [name_ "thread-limit", type_ "number", value_ $ T.pack . show $ threadLimit board]

          tr_ $ do
            td_ "Post permission"
            td_ $ select_ [name_ "permission"] $ do
              option_ (value_ "0" : selectPermission AnyThreadsAnyReplies) "Anyone can make threads and replies"
              option_ (value_ "1" : selectPermission ModThreadsAnyReplies) "Only mods can make threads"
              option_ (value_ "2" : selectPermission NilThreadsAnyReplies) "No new threads, only replies"
              option_ (value_ "3" : selectPermission NilThreadsNilReplies) "No new threads or replies"

          tr_ $ do
            td_ "Index view"
            td_ $
              select_ [name_ "index-view-policy"] $ do
                option_ (value_ "0" : selectIndexViewPolicy IndexViewDisallowed) "Disabled"
                option_ (value_ "1" : selectIndexViewPolicy IndexViewAllowed)    "Enabled"
                option_ (value_ "2" : selectIndexViewPolicy IndexViewPreferred)  "Preferred"

          tr_ $ do
            td_ "Add moderator"
            td_ $ input_ [name_ "add-mod", maxlength_ "32"]

          if null modtuples
          then input_ [type_ "hidden", name_ "unto-mods", value_ "remove"]
          else
            tr_ $ do
              td_ "Moderators"
              td_ $ do
                select_ [name_ "unto-mods"] $ do
                  option_ [value_ "remove"]  "Remove"
                  option_ [value_ "promote"] "Promote to manager"
                  option_ [value_ "demote"]  "Demote to non-manager"
                hr_ []
                mconcat $ (flip map) modtuples $ \(modname, isManager) ->
                  div_ $
                    label_ $ do
                      input_ [class_ "middle", name_ "select-mod", type_ "checkbox", value_ modname]
                      span_ [class_ "middle"] $ toHtml modname
                      when isManager $
                        abbr_ [class_ "middle", title_ "Manager"] "(*)"

          tr_ $ do
            td_ "Continue"
            td_ $ input_ [type_ "submit", value_ "Submit"]

      when (powerlevel >= BoardOwner) $ do
        header_ [class_ "barheader"] "Delete Board"
        p_ "This permanently deletes the board and all of its posts."

        form_ [method_ "post", action_ $ "/.phi/auth/board/delete/" <> uri board] $ do
          label_ $ do
            "Type DELETE to confirm: "
            input_ [name_ "confirmation", required_ "", autocomplete_ "off"]
          " "
          input_ [type_ "submit", value_ "Delete Board"]

  where
    selectTheme mTheme_
      | mTheme board == mTheme_ = [selected_ ""]
      | otherwise               = []
    selectPermission permission_
      | permission board == permission_ = [selected_ ""]
      | otherwise                       = []
    selectIndexViewPolicy indexViewPolicy_
      | indexViewPolicy board == indexViewPolicy_ = [selected_ ""]
      | otherwise                                 = []
