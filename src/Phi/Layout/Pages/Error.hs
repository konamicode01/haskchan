{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Error where

import           Control.Monad (when)
import           Data.Default

import           Data.Text (Text)
import qualified Data.Text as T (pack)
import           Data.Text.Encoding (decodeUtf8)

import           Network.HTTP.Types (Status(..))

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseWithoutThemeSelectL)

errorL :: Status -> Maybe Board -> [Text] -> Html ()
errorL status mBoard explanations =
  baseWithoutThemeSelectL (def :: PageDetails) (title_ "Error") $ do
    h1_ [id_ "pagetitle"] $ toHtml $ stCode <> " " <> stMessage
    case explanations of
      []            -> pure ()
      [explanation] -> p_ [class_ "center"] $ toHtml explanation
      _             ->
        ul_ $
          mconcat $ (flip map) explanations $ \explanation ->
            li_ $ toHtml explanation
    case mBoard of
      Nothing    -> pure ()
      Just board -> do
        br_ []
        p_ [class_ "center"] $ do
          "Return:"
          " [" <> a_ [href_ $ "/" <> uri board <> (if indexViewPolicy board == IndexViewPreferred then "/catalogue" else "/")] "Catalogue" <> "]"
          when (indexViewPolicy board /= IndexViewDisallowed) $
            " [" <> a_ [href_ $ "/" <> uri board <> (if indexViewPolicy board == IndexViewPreferred then "/" else "/index")] "Index" <> "]"
  where
    stCode = T.pack . show . statusCode $ status
    stMessage = decodeUtf8 . statusMessage $ status
