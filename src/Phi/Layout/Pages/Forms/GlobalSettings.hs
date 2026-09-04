{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Forms.GlobalSettings where

import Lucid

import Phi.Database.Models
import Phi.Layout.Base (baseL)

globalSettingsPromptL :: PageDetails -> Html ()
globalSettingsPromptL details =
  baseL details (title_ "Global settings") $ do
    h1_ [id_ "pagetitle"] "Global settings"
    article_ [class_ "container"] $
      form_ [method_ "post"] $
        table_ [class_ "formtable"] $ do
          tr_ $ do
            td_ "Global theme"
            td_ $
              select_ [name_ "global-theme"] $ do
                option_ (value_ "0" : selectGlobalTheme Phichannel) "Phichannel"
                option_ (value_ "1" : selectGlobalTheme Nanochan)   "Nanochan"
                option_ (value_ "2" : selectGlobalTheme Yotsuba)    "Yotsuba"
          tr_ $ do
            td_ "Open registration"
            td_ $ input_ (name_ "open-registration" : type_ "checkbox" : check openRegistration)
          tr_ $ do
            td_ "User board creation"
            td_ $ input_ (name_ "user-board-creation" : type_ "checkbox" : check userBoardCreation)
          tr_ $ do
            td_ "Require captcha globally"
            td_ $ input_ (name_ "captcha-baseline" : type_ "checkbox" : check captchaBaseline)
          tr_ $ do
            td_ "Continue"
            td_ $ input_ [type_ "submit", value_ "Submit"]
  where
    globalsettings = pdGlobalSettings details
    selectGlobalTheme theme_
      | theme_ == globalTheme globalsettings = [selected_ ""]
      | otherwise                            = []
    check accessor
      | accessor globalsettings = [checked_]
      | otherwise               = []
