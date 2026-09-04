{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Forms.Login where

import Control.Monad (when)

import Lucid

import Phi.Database.Models
import Phi.Layout.Base (baseL)
import Phi.Layout.Attributes (minlength_)

loginPromptL :: PageDetails -> Html ()
loginPromptL details =
  baseL details (title_ "Login") $ do
    h1_ [id_ "pagetitle"] "Login"
    article_ [class_ "container"] $ do
      div_ $ do
        "The moderation interface requires you to be logged in."
        when (openRegistration globalsettings) $
          " Alternatively you can " <> a_ [href_ "/.phi/register"] "register" <> "."
      br_ []
      form_ [method_ "post"] $ do
        table_ [class_ "formtable"] $ do
          tr_ $ do
            td_ "Username"
            td_ $ input_ [name_ "username", required_ "", maxlength_ "32", pattern_ "[a-zA-Z0-9]*", title_ "alphanumeric characters"]
          tr_ $ do
            td_ "Password"
            td_ $ input_ [name_ "password", type_ "password", required_ "", minlength_ "8", maxlength_ "1024"]
          tr_ $ do
            td_ "Captcha"
            td_ $ do
              img_ [id_ "captcha", src_ "/.phi/captcha.jpg"]
              input_ [name_ "captcha", required_ "", maxlength_ "32"]
          tr_ $ do
            td_ "Continue"
            td_ $ input_ [type_ "submit", value_ "Login"]
  where
    globalsettings = pdGlobalSettings details
