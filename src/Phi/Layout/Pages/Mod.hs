{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Pages.Mod where

import           Control.Monad (when)
import           Data.Default

import           Data.Text (Text)
import qualified Data.Text as T (pack)

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Base (baseWithoutThemeSelectL)

modCompleteL :: Int -> Int -> Maybe Text -> Html ()
modCompleteL nRequestedActions nPerformedActions mReturnUrl =
  baseWithoutThemeSelectL (def :: PageDetails) (title_ "Moderation complete") $ do
    h1_ [id_ "pagetitle"] $
      if nPerformedActions == 0 then "Nothing done"
      else if nPerformedActions /= nRequestedActions then "Partial success"
      else "Success"
    p_ $ do
      div_ [class_ "center"] $ toHtml $ "Actions requested: " <> (T.pack . show $ nRequestedActions)
      div_ [class_ "center"] $ toHtml $ "Actions performed: " <> (T.pack . show $ nPerformedActions)
    when (nPerformedActions /= nRequestedActions) $
      p_ [class_ "center"] $
        if nIgnoredActions == 1 && nPerformedActions /= 0
        then "1 requested action would have had no effect and was ignored."
        else toHtml $ ignoredActions <> " requested actions would have had no effect and were ignored."
    case mReturnUrl of
      Nothing        -> pure ()
      Just returnUrl -> do
        br_ []
        div_ [class_ "center"] $ "[" <> a_ [href_ returnUrl] "Return" <> "]"
  where
    nIgnoredActions = nRequestedActions - nPerformedActions
    ignoredActions
      | nPerformedActions == 0 = "All"
      | otherwise = T.pack . show $ nIgnoredActions
