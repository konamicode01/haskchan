{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Components.Mod where

import Control.Monad (when)
import Lucid

modbuttonL' :: Bool -> Html ()
modbuttonL' loggedIn =
  when loggedIn $
    div_ [id_ "modbutton"] $ a_ [href_ "#modform"] "[Mod]"

modformTableL' :: Bool -> Html ()
modformTableL' loggedIn =
  when loggedIn $ do
  div_ [id_ "modform"] $ do
    a_ [class_ "closebutton", href_ "##"] "[X]"
    table_ [class_ "formtable"] $ do
      tr_ $ do
        td_ $ "Action"
        td_ $ do
          input_ [id_ "modform-sticky", type_ "radio", name_ "action", value_ "sticky", required_ ""]
          label_ [for_ "modform-sticky"] "Sticky"
          br_ []
          input_ [id_ "modform-cycle", class_ "boolean", type_ "radio", name_ "action", value_ "cycle"]
          label_ [for_ "modform-cycle"] "Cycle"
          br_ []
          input_ [id_ "modform-lock", class_ "boolean", type_ "radio", name_ "action", value_ "lock"]
          label_ [for_ "modform-lock"] "Lock"
          br_ []
          input_ [id_ "modform-bumplock", class_ "boolean", type_ "radio", name_ "action", value_ "bumplock"]
          label_ [for_ "modform-bumplock"] "Bumplock"
          hr_ []
          input_ [id_ "modform-unlink", type_ "radio", name_ "action", value_ "unlink"]
          label_ [for_ "modform-unlink"] "Unlink file"
          br_ []
          input_ [id_ "modform-purge", type_ "radio", name_ "action", value_ "purge"]
          label_ [for_ "modform-purge"] "Purge file"
          br_ []
          input_ [id_ "modform-delete", type_ "radio", name_ "action", value_ "delete"]
          label_ [for_ "modform-delete"] "Delete"
          hr_ [class_ "invisible"]
          label_ [id_ "modform-stickiness"] $ do
            abbr_ [title_ "0 = unsticky"] "Stickiness" <> ": "
            input_ [type_ "number", name_ "stickiness", min_ "0"]
          div_ [id_ "modform-boolean"] $ do
            label_ $ input_ [type_ "radio", name_ "boolean", value_ "1"] <> "Enable"
            " "
            label_ $ input_ [type_ "radio", name_ "boolean", value_ "0"] <> "Disable"
      tr_ $ do
        td_ $ "Reason"
        td_ $ input_ [name_ "reason", maxlength_ "32"]
      tr_ $ do
        td_ $ "Continue"
        td_ $ input_ [type_ "submit", value_ "Submit"]
