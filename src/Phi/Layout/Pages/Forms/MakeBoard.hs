{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Forms.MakeBoard where

import Lucid

import Phi.Database.Models
import Phi.Layout.Base (baseL)

makeBoardPromptL :: PageDetails -> Html ()
makeBoardPromptL details =
  baseL details (title_ "Make a board") $ do
    h1_ [id_ "pagetitle"] "Make a board"
    article_ [class_ "container"] $
      form_ [method_ "post"] $
        table_ [class_ "formtable"] $ do
          tr_ $ do
            td_ "URI"
            td_ $ input_ [name_ "uri", required_ "", maxlength_ "32", pattern_ "[a-z0-9]*", title_ "lowercase alphanumeric characters"]
          tr_ $ do
            td_ "Title"
            td_ $ input_ [name_ "title", required_ "", maxlength_ "32"]
          tr_ $ do
            td_ "Description"
            td_ $ input_ [name_ "description", maxlength_ "128"]
          tr_ $ do
            td_ "Captcha"
            td_ $ do
              img_ [id_ "captcha", src_ "/.phi/captcha.jpg", title_ "Click to refresh CAPTCHA"]
              input_ [name_ "captcha", required_ "", maxlength_ "32"]
          tr_ $ do
            td_ "Continue"
            td_ $ input_ [type_ "submit", value_ "Make board"]
