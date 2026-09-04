{-# LANGUAGE OverloadedStrings #-}

module Phi.Routes where

import           Data.Text (Text)

import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status hiding (mkStatus)
import           Network.Wai (Response)
import           Web.Fn hiding (okHtml)

import           Phi.Captcha (shouldEnforceCaptchaForBoard)
import           Phi.Context (Context)
import           Phi.Database.Models (PageDetails(..))
import           Phi.Database.Queries
import           Phi.Database.Queries.Types
import           Phi.Forms (implicitBoardPage, indexPage, cataloguePage, threadPage, validate, Validation(..))
import           Phi.HTTP (okHtml)
import           Phi.Layout.Pages.Homepage
import           Phi.Layout.Pages.Catalogue
import           Phi.Layout.Pages.Index
import           Phi.Layout.Pages.Thread
import           Phi.Routes.Errors
import           Phi.Routes.Meta (metaRoutes)
import           Phi.Routes.Auth (authRoutes)

routes :: Context -> IO Response
routes context = (flip fallthrough) errorNoMatchingRoutesH $ route context
  [ method GET // end ==> homepageH
  , method GET // segment // end ==> implicitBoardH
  , method GET // segment // path "catalogue" // end ==> catalogueH
  , method GET // segment // path "index" // segment // end ==> indexH
  , method GET // segment // segment // end ==> indexH
  , method GET // segment // path "index" // end ==> indexFrontPageH
  , method GET // segment // path "thread" // segment // end ==> threadH
  , path ".phi" !=> metaRoutes
  , path ".phi" // path "auth" !=> authRoutes
  , method GET  ==> \_context -> errorSimpleH notFound404
  , method POST ==> \_context -> errorSimpleH badRequest400
  ]

homepageH :: Context -> IO (Maybe Response)
homepageH context = do
  mDetails <- getPageDetails context Nothing
  case mDetails of
    Nothing      -> errorH internalServerError500 "Error preparing page"
    Just details -> do
      mTeaserImagePosts <- getRecentHavingFiles context
      case mTeaserImagePosts of
        Nothing -> errorH internalServerError500 "Error fetching images"
        Just teaserImagePosts -> do
          mTeaserPosts <- (map (fst3 . snd) . fst <$>) <$> getRecent context 16 (False, [])
          case mTeaserPosts of
            Nothing -> errorH internalServerError500 "Error fetching posts"
            Just teaserPosts -> do
              okHtml $ homepageL details (pdTopnav details) teaserImagePosts teaserPosts
  where
    fst3 (a, b, c) = a

implicitBoardH :: Context -> Text -> IO (Maybe Response)
implicitBoardH context uri_ =
  case validation of
    Invalid messages -> errorListH badRequest400 messages
    Aborted          -> pure Nothing
    Valid ()         -> do
      mReceipt <- getImplicit context uri_
      case mReceipt of
        Nothing -> errorH conflict409 "Error fetching board"
        Just receipt ->
          case receipt of
            Left NoSuchBoard -> errorH notFound404 "No such board"
            Right implicit   -> do
              let
                board =
                  case implicit of
                    Left  (board, _, _) -> board
                    Right (board, _)    -> board
              mDetails <- getPageDetails context $ Just board
              case mDetails of
                Nothing      -> errorH internalServerError500 "Error preparing page"
                Just details ->
                  case implicit of
                    Left  (_, hulls, nPages) -> okHtml $ indexL details board hulls nPages 0
                    Right (_, ops)           -> okHtml $ catalogueL details board ops
  where
    validation = validate $ implicitBoardPage uri_

catalogueH :: Context -> Text -> IO (Maybe Response)
catalogueH context uri_ =
  case validation of
    Invalid messages -> errorListH badRequest400 messages
    Aborted          -> pure Nothing
    Valid ()         -> do
      mReceipt <- getCatalogue context uri_
      case mReceipt of
        Nothing -> errorH conflict409 "Error fetching catalogue"
        Just receipt ->
          case receipt of
            Left NoSuchBoard   -> errorH notFound404 "No such board"
            Right (board, ops) -> do
              mDetails <- getPageDetails context $ Just board
              case mDetails of
                Nothing      -> errorH internalServerError500 "Error preparing page"
                Just details -> okHtml $ catalogueL details board ops
  where
    validation = validate $ cataloguePage uri_

indexH :: Context -> Text -> Int -> IO (Maybe Response)
indexH context uri_ pageInc =
  case validation of
    Invalid messages -> errorListH badRequest400 messages
    Aborted          -> pure Nothing
    Valid ()         -> do
      mReceipt <- getIndex context uri_ page
      case mReceipt of
        Nothing      -> errorH conflict409 "Error fetching index"
        Just receipt ->
          case receipt of
            Left NoSuchBoard                     -> errorH notFound404 "No such board"
            Right (Left ViewDisabled)            -> errorH forbidden403 "Index view is disabled on this board"
            Right (Right (board, hulls, nPages)) ->
              if null hulls && page /= 0
              then errorH notFound404 "No such index page"
              else do
              mDetails <- getPageDetails context $ Just board
              case mDetails of
                Nothing      -> errorH internalServerError500 "Error preparing page"
                Just details -> okHtml $ indexL details board hulls nPages page
  where
    validation = validate $ indexPage uri_ pageInc
    page = pageInc - 1

indexFrontPageH :: Context -> Text -> IO (Maybe Response)
indexFrontPageH context uri_ = indexH context uri_ 1

threadH :: Context -> Text -> Int -> IO (Maybe Response)
threadH context uri_ threadNo =
  case validation of
    Invalid messages -> errorListH badRequest400 messages
    Aborted          -> pure Nothing
    Valid ()         -> do
      mReceipt <- getThreadPosts context uri_ threadNo
      case mReceipt of
        Nothing      -> errorH conflict409 "Error fetching thread"
        Just receipt -> do
          case receipt of
            Left (Left NoSuchBoard)    -> errorH notFound404 "No such board"
            Left (Right threadFate)    -> do
              meBoard <- getBoard context uri_
              case meBoard of
                Just (Right board) -> errorWithBoardLinksH board (mkStatus threadFate) "No such thread"
                _                  -> errorH (mkStatus threadFate) "No such thread"
            Right (board, op, replies) -> do
              mDetails <- getPageDetails context $ Just board
              case mDetails of
                Nothing -> errorH internalServerError500 "Error preparing page"
                Just details ->
                  okHtml $ threadL details board op replies
  where
    validation = validate $ threadPage uri_ threadNo
    mkStatus FutureThread  = notFound404
    mkStatus DeletedThread = gone410
