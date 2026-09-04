{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Forms.Banners where

import Data.Text (Text)

import Lucid

import Phi.Database.Models
import Phi.Database.Queries.Types
import Phi.Layout.Base (baseL)

changeBannersPromptL :: PageDetails -> Powerlevel -> Text -> [Banner] -> Html ()
changeBannersPromptL details powerlevel uri_ banners =
  baseL details (title_ "Banners") $ do
    h1_ [id_ "pagetitle"] $ "Banners for /" <> toHtml uri_ <> "/"
    h4_ [id_ "pagesubtitle"] $
      case powerlevel of
        Admin        -> "You are an admin. Admins can change banners."
        BoardOwner   -> "You are the board owner. Board owners can change banners."
        BoardManager -> "You are a board manager. Board managers can change banners."
        _            -> "You are not a board manager. You cannot change banners."
    article_ [class_ "container"] $ do
      header_ [class_ "barheader"] "Add a banner"
      p_ "Must be PNG or JPEG smaller than 512 KiB. Should be 3:1 aspect ratio or wider and at least 300x100."
      form_ [action_ "add", method_ "post", enctype_ "multipart/form-data"] $ do
        table_ [class_ "formtable"] $ do
          tr_ $ do
            td_ "New banner"
            td_ $
              input_ [name_ "banner", type_ "file", accept_ "image/jpeg, image/png", required_ ""]
          tr_ $ do
            td_ "Continue"
            td_ $ input_ [type_ "submit", value_ "Upload"]
      header_ [class_ "barheader"] "Delete banners"
      form_ [action_ "delete", method_ "post"] $ do
        table_ [class_ "formtable"] $ do
          tr_ $ do
            td_ "Banners"
            td_ $
              if null banners
              then p_ "Nothing here."
              else
                mconcat $ (flip map) banners $ \banner ->
                  label_ [class_ "luminous"] $ do
                    input_ [class_ "luminous-checkbox invisible", name_ "banner", type_ "checkbox", value_ $ bnHash banner]
                    div_ [class_ "luminous-check"] ""
                    img_ [class_ "banner", src_ $ "/.phi/static/banner/" <> uri_ <> "/" <> bnHash banner <> bnExt banner]
          tr_ $ do
            td_ "Continue"
            td_ $ input_ [type_ "submit", value_ "Delete"]
