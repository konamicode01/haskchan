{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Components.Board where

import           Control.Monad (when)

import           Data.Text (Text)
import qualified Data.Text as T (pack)

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Attributes (loading_)

bannerL' :: Board -> Html ()
bannerL' board = img_ [id_ "banner", src_ $ "/.phi/varstatic/banner/" <> uri board <> "/"]

postformL' :: Board -> Maybe Thread -> Bool -> Html ()
postformL' board mThread shouldEnforceCaptcha =
  form_ [id_ "postform", action_ "/.phi/post", method_ "post", enctype_ "multipart/form-data"] $ do
    a_ [class_ "closebutton", href_ "#"] "[X]"
    input_ [type_ "hidden", name_ "board", value_ $ uri board]
    case mThread of
      Nothing     -> pure ()
      Just thread -> input_ [type_ "hidden", name_ "thread", value_ $ T.pack . show $ tPostNo thread]
    table_ [class_ "formtable"] $ do
      tr_ $ do
        td_ $ "Name"
        td_ $ input_ [name_ "name", maxlength_ "32"]
      tr_ $ do
        td_ $ "Email"
        td_ $ input_ [name_ "email", maxlength_ "32"]
      tr_ $ do
        td_ $ "Subject"
        td_ $ do
          input_ [name_ "subject", maxlength_ "64"]
          input_ [type_ "submit", value_ "Post"]
      tr_ $ do
        td_ $ "Message"
        td_ $ textarea_ [name_ "message", cols_ "32", rows_ "4", required_ "", maxlength_ "4096"] ""
      tr_ $ do
        td_ $ abbr_ [title_ "max 8 MiB"] "File"
        td_ $ input_ [name_ "file", type_ "file", accept_ "image/*, .webm, .mp4, audio/ogg, .flac, .mp3, text/plain"]
      when shouldEnforceCaptcha $
        tr_ $ do
          td_ $ "Captcha"
          td_ $ do
            img_ [id_ "captcha", src_ "/.phi/captcha.jpg", loading_ "lazy", title_ "Click to refresh CAPTCHA"]
            input_ [name_ "captcha", required_ ""]

threadBoardnavL' :: Board -> Thread -> Bool -> Bool -> Html ()
threadBoardnavL' board thread isAtTop showReply =
  nav_ (class_ "boardnav" : (if isAtTop then [] else [id_ "boardnav-bottom"])) $ do
    ul_ [class_ "flat"] $ do
      case indexViewPolicy board of
        IndexViewDisallowed -> do
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/"]          "Catalogue"    <> "]"
        IndexViewAllowed -> do
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/"]          "Catalogue" <> "]"
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/index"]     "Index"     <> "]"
        IndexViewPreferred -> do
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/"]          "Index"     <> "]"
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/catalogue"] "Catalogue" <> "]"
      if isAtTop
      then li_ $ "[" <> a_ [href_ "#bottom"] "Bottom" <> "]"
      else li_ $ "[" <> a_ [href_ "#top"]    "Top"    <> "]"
      li_ $ "[" <> a_ [href_ ""] "Update" <> "]"
    when (not isAtTop) $ do
      span_ [id_ "boardnav-postbutton"] $ do
        "[" <> a_ (if showReply then [href_ "#postform"] else []) "Post a Reply" <> "]"
      div_ [id_ "boardnav-stats"] $ do
        (toHtml . show $ nFiles thread)   <> toHtmlRaw ("&nbsp;" :: Text) <> "files, "
        (toHtml . show $ nReplies thread) <> toHtmlRaw ("&nbsp;" :: Text) <> "replies"

catalogueBoardnavL' :: Board -> Html ()
catalogueBoardnavL' board =
  nav_ [class_ "boardnav"] $ do
    ul_ [class_ "flat"] $ do
      li_ $ "[" <> a_ [href_ ""] "Update" <> "]"
      case indexViewPolicy board of
        IndexViewDisallowed -> pure ()
        IndexViewAllowed -> do
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/index"]     "Index"     <> "]"
        IndexViewPreferred -> do
          li_ $ "[" <> a_ [href_ $ "/" <> uri board <> "/"]          "Index"     <> "]"

indexBoardnavL' :: Board -> Int -> Int -> Html ()
indexBoardnavL' board nPages page =
  nav_ [class_ "boardnav"] $ do
    if indexViewPolicy board == IndexViewPreferred
    then "[" <> a_ [href_ $ "/" <> uri board <> "/catalogue"] "Catalogue" <> "]"
    else "[" <> a_ [href_ $ "/" <> uri board <> "/"]          "Catalogue" <> "]"
    " Page: "
    form_ [id_ "boardnav-indexform"] $
      ul_ [class_ "flat"] $ do
        when (page > 0) $
          li_ $ input_ [id_ "boardnav-previous", type_ "submit", formaction_ $ url (page - 1), value_ "Previous"]
        mconcat $ (flip map) [0..nPages-1] $ \p ->
          li_ (if p == page then [id_ "boardnav-thispage"] else []) $
            a_ [href_ $ url p] $ "[" <> (toHtml . show $ p + 1) <> "]"
        when (nPages > 1 && page < nPages - 1) $
          li_ $ input_ [id_ "boardnav-next", type_ "submit", formaction_ $ url (page + 1), value_ "Next"]
  where
    url p
      | indexViewPolicy board == IndexViewPreferred =
        "/" <> uri board <> "/"       <> (T.pack . show $ p + 1)
      | otherwise =
        "/" <> uri board <> "/index/" <> (T.pack . show $ p + 1)
