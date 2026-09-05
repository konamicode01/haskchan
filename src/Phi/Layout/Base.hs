{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Base where

import Control.Monad (when)
import Lucid

import Phi.Database.Models

baseL :: PageDetails -> Html () -> Html () -> Html ()
baseL = baseL' True

baseWithoutThemeSelectL :: PageDetails -> Html () -> Html () -> Html ()
baseWithoutThemeSelectL = baseL' False

baseL' :: Bool -> PageDetails -> Html () -> Html () -> Html ()
baseL' includeThemeSelect details htmlHead htmlBody =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      meta_ [ httpEquiv_ "content-security-policy"
            , content_ "default-src 'none'; style-src 'self'; script-src 'self'; img-src 'self'; media-src 'self'; form-action 'self';"
            ]
      meta_ [name_ "referrer", content_ "same-origin"]
      link_ [rel_ "icon", type_ "image/x-icon", href_ "/.phi/static/favicon.ico"]
      link_ [rel_ "stylesheet", href_ "/.phi/static/style.css"]
      script_ [src_ "/.phi/static/captcha.js", defer_ ""] (mempty :: Html ())
      script_ [src_ "/.phi/static/quote.js", defer_ ""] (mempty :: Html ())
      when (pageTheme /= Phichannel) $
        link_ [rel_ "stylesheet", href_ $ themeUrl pageTheme]
      htmlHead
    body_ [class_ $ if pdLoggedIn details then "logged-in" else "logged-out"] $ do
      div_ [id_ "top"] ""
      nav_ [id_ "topnav"] $ do
        ul_ [id_ "topnav-links", class_ "flat"] $ do
          li_ $ a_ [href_ "/"] "home"
          li_ $ a_ [href_ "/.phi/auth/"] "mod"
          li_ $ a_ [href_ "/.phi/log"] "log"
          li_ $ a_ [href_ "/.phi/recent"] "recent"
        ul_ [id_ "topnav-boards", class_ "flat"] $
          mconcat $ (flip map) topnav $ \board ->
            li_ $
              a_ [href_ $ "/" <> uri board <> "/", title_ $ title board] $
                toHtml $ uri board
        ul_ [id_ "topnav-right", class_ "flat"] $ do
          li_ $ a_ [href_ "#settings-menu"] "settings"

        when includeThemeSelect $
          a_ [id_ "topnav-theme", href_ "#theme"] "theme"

      div_ [id_ "settings-menu"] $ do
        div_ [id_ "settings-menu-content"] $ do
          div_ [class_ "settings-title"] "Settings"
          div_ [class_ "settings-section"] $ do
            span_ [class_ "settings-label"] "Theme"
            a_ [href_ "/.phi/settings?theme=haskchan"] "Haskchan"
            a_ [href_ "/.phi/settings?theme=yotsuba"] "Yotsuba"
            a_ [href_ "/.phi/settings?theme=nanochan"] "Nanochan"
            a_ [href_ "/.phi/settings?theme=phichannel"] "Phichannel"
          div_ [class_ "settings-section"] $ do
            span_ [class_ "settings-label"] "Account"
            a_ [href_ "/.phi/auth/"] "Account / Mod"
          a_ [href_ "#"] "close"
      main_
        htmlBody
      when includeThemeSelect $
        div_ [id_ "theme"] $ do
          span_ [id_ "theme-arrow"] ""
          form_ [action_ "/.phi/settings"] $ do
            select_ [name_ "theme"] $ do
              option_ (optionAttributes $ Nothing)         "-- Default --"
              option_ (optionAttributes $ Just Phichannel) "Phichannel"
              option_ (optionAttributes $ Just Nanochan)   "Nanochan"
              option_ (optionAttributes $ Just Yotsuba)    "Yotsuba"
              option_ (optionAttributes $ Just Haskchan)   "Haskchan"
            input_ [type_ "submit", value_ "Theme"]
      footer_ [id_ "site-footer"] $ do
        "This code is ran by "
        a_ [href_ "https://github.com/konamicode01/haskchan"] "Haskchan"
      div_ [id_ "bottom"] ""
  where
    topnav = pdTopnav details
    pageTheme = pdTheme details
    cookiesettings = pdCookieSettings details

    optionAttributes mTheme_ =
      if mTheme_ == cookieTheme cookiesettings
      then [value_ $ getName mTheme_, selected_ ""]
      else [value_ $ getName mTheme_]

    getName Nothing           = ""
    getName (Just Phichannel) = "phichannel"
    getName (Just Nanochan)   = "nanochan"
    getName (Just Yotsuba)    = "yotsuba"
    getName (Just Haskchan)   = "haskchan"
