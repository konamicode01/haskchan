{-# LANGUAGE OverloadedStrings #-}

module Phi.Routes.Errors where

import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai (Response)

import Phi.Database.Models (Board)
import Phi.HTTP (respondHtml, respondHtml')
import Phi.Layout.Pages.Error (errorL)

errorH :: Status -> Text -> IO (Maybe Response)
errorH status explanation =
  respondHtml status $ errorL status Nothing [explanation]

errorSimpleH :: Status -> IO (Maybe Response)
errorSimpleH status =
  respondHtml status $ errorL status Nothing []

errorListH :: Status -> [Text] -> IO (Maybe Response)
errorListH status explanations =
  respondHtml status $ errorL status Nothing explanations

errorWithBoardLinksH :: Board -> Status -> Text -> IO (Maybe Response)
errorWithBoardLinksH board status explanation =
  respondHtml status $ errorL status (Just board) [explanation]

errorNoMatchingRoutesH :: IO Response
errorNoMatchingRoutesH =
  pure . respondHtml' status $ errorL status Nothing []
  where status = methodNotAllowed405
