{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Attributes where

import Data.Text (Text)
import Lucid.Base (Attribute, makeAttribute)

loading_ :: Text -> Attribute
loading_ = makeAttribute "loading"

minlength_ :: Text -> Attribute
minlength_ = makeAttribute "minlength"

poster_ :: Text -> Attribute
poster_ = makeAttribute "poster"
