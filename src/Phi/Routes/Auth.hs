{-# LANGUAGE OverloadedStrings #-}

module Phi.Routes.Auth where

import           Data.Text (Text, intercalate)
import qualified Data.Text as T (pack)

import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai (Response)
import           Web.Fn hiding (okHtml, File, file)
import qualified Web.Fn as Fn (File, file)

import           Phi.Auth (elicitUsername, isLoggedIn)
import           Phi.Captcha (enforceCaptcha)
import           Phi.Context (Context, getReferrerAsText)
import           Phi.Database.Models (BoardPermission, IndexViewPolicy, Theme, BlankMaybe(..), SuperMaybe(..), PageDetails(..), User(admin), GlobalSettings(userBoardCreation, openRegistration))
import           Phi.Database.Queries (getPageDetails, addBanner, deleteBanners, getBanners, getBoardAndModTuples, getBoards, getGlobalSettings, getUser, getUserAndBoards, makeBoard, setBoardSettings, setGlobalSettings, moderate)
import           Phi.Database.Queries.Types
import           Phi.Files (readableFilesize)
import           Phi.Forms (boardSettingsPage, boardSettingsForm, globalSettingsForm, makeBoardForm, modForm, validate, Validation(..))
import           Phi.HTTP (okHtml, respondHtmlWithHeaders)
import           Phi.Layout.Pages.Auth
import           Phi.Layout.Pages.Mod
import           Phi.Layout.Pages.Forms.Banners
import           Phi.Layout.Pages.Forms.BoardSettings
import           Phi.Layout.Pages.Forms.GlobalSettings
import           Phi.Layout.Pages.Forms.MakeBoard
import           Phi.Routes.Errors

authRoutes :: Context -> IO (Maybe Response)
authRoutes context = route context
  [ method POST // path "mod"
                // param "action"
                // param "stickiness"
                // param "boolean"
                // param "reason"
                // param "uri-no"
                // end !=> modH
  , method GET // end ==> authHomeH
  , method POST // path "logout"
                // end ==> logoutH
  , method GET // path "board"
               // path "create"
               // end ==> makeBoardPromptH
  , method POST // path "board"
                // path "create"
                // param "uri"
                // param "title"
                // param "description"
                // param "captcha"
                // end !=> makeBoardH
  , method GET // path "board"
               // path "settings"
               // segment
               // end ==> boardSettingsPromptH
  , method POST // path "board"
                // path "settings"
                // segment
                // param "title"
                // param "description"
                // param "theme"
                // param "anon-name"
                // param "bump-limit"
                // param "reply-limit"
                // param "thread-limit"
                // param "permission"
                // param "index-view-policy"
                // param "add-mod"
                // param "unto-mods"
                // param "select-mod"
                // end !=> boardSettingsH
  , method GET // path "board"
               // path "banners"
               // segment
               // end ==> changeBannersPromptH
  , method POST // path "board"
                // path "banners"
                // segment
                // path "add"
                // Fn.file "banner"
                // end !=> addBannerH
  , method POST // path "board"
                // path "banners"
                // segment
                // path "delete"
                // param "banner"
                // end !=> deleteBannersH
  , method GET // path "settings"
               // end ==> globalSettingsPromptH
  , method POST // path "settings"
                // param "global-theme"
                // param "open-registration"
                // param "user-board-creation"
                // param "captcha-baseline"
                // end !=> globalSettingsH
  ]

authHomeH :: Context -> IO (Maybe Response)
authHomeH context = do
  mUsername <- elicitUsername context
  case mUsername of
    Nothing        -> redirect "/.phi/login"
    Just username_ -> do
      mReceipt <- getUserAndBoards context username_
      case mReceipt of
        Nothing      -> errorH conflict409 "Error fetching user"
        Just receipt ->
          case receipt of
            Left UserNotFound -> errorH forbidden403 "No user exists with the username in your cookie"
            Left Forbidden    -> errorH forbidden403 "You are not allowed to see this page"
            Right (user, ownedBoards, modBoards, otherBoards) -> do
              mDetails <- getPageDetails context Nothing
              case mDetails of
                Nothing      -> errorH internalServerError500 "Error preparing page"
                Just details -> okHtml $ authHomeL details user ownedBoards modBoards otherBoards

logoutH :: Context -> IO (Maybe Response)
logoutH context = do
  bool <- isLoggedIn context
  if bool
  then respondHtmlWithHeaders headers seeOther303 ""
  else errorH forbidden403 "You are not logged in"
  where
    headers =
      [ ("set-cookie", "auth=; path=/; expires=Thu, Jan 01 1970 00:00:00 GMT")
      , ("location", "/.phi/login")
      ]

makeBoardPromptH :: Context -> IO (Maybe Response)
makeBoardPromptH context = do
  mUsername <- elicitUsername context
  case mUsername of
    Nothing -> errorH forbidden403 "You are not logged in"
    Just username_ -> do
      mReceipt <- getUser context username_
      case mReceipt of
        Nothing      -> errorH internalServerError500 "Error fetching user"
        Just receipt ->
          case receipt of
            Nothing   -> errorH forbidden403 "No user exists with the username in your cookie"
            Just user -> do
              mDetails <- getPageDetails context Nothing
              case mDetails of
                Nothing -> errorH internalServerError500 "Error preparing page"
                Just details ->
                  if not (openRegistration $ pdGlobalSettings details) && not (admin user)
                  then errorH forbidden403 "You cannot make a board"
                  else okHtml $ makeBoardPromptL details

makeBoardH :: Context -> Text -> Text -> Text -> Text -> IO (Maybe Response)
makeBoardH context uri_ title_ description_ work = do
  enforceCaptcha context work (errorH badRequest400 "Wrong or expired captcha") $
    case validation of
      Aborted          -> pure Nothing
      Invalid messages -> errorListH badRequest400 messages
      Valid newboard   -> do
        mUsername <- elicitUsername context
        case mUsername of
          Nothing        -> errorH forbidden403 "You are not logged in"
          Just username_ -> do
            mReceipt <- makeBoard context username_ newboard
            case mReceipt of
              Nothing      -> errorH conflict409 "Error making board"
              Just receipt -> do
                case receipt of
                  Left (Left UserNotFound) -> errorH forbidden403 "No user exists with the username in your cookie"
                  Left (Left Forbidden)    -> errorH forbidden403 "You cannot make a board"
                  Left (Right ExtantBoard) -> errorH forbidden403 "URI is taken"
                  Right ()                 -> redirect $ "/" <> uri_ <> "/"
  where
    validation = validate $ makeBoardForm uri_ title_ description_

boardSettingsPromptH :: Context -> Text -> IO (Maybe Response)
boardSettingsPromptH context uri_ = do
  case validation of
    Aborted          -> pure Nothing
    Invalid messages -> errorListH badRequest400 messages
    Valid ()         -> do
      mUsername <- elicitUsername context
      case mUsername of
        Nothing        -> errorH forbidden403 "You are not logged in"
        Just username_ -> do
          mReceipt <- getBoardAndModTuples context username_ uri_
          case mReceipt of
            Nothing      -> errorH conflict409 "Error fetching board settings"
            Just receipt ->
              case receipt of
                Left (Left UserNotFound)             -> errorH forbidden403 "No user exists with the username in your cookie"
                Left (Left Forbidden)                -> errorH forbidden403 "You have no authority here"
                Left (Right NoSuchBoard)             -> errorH forbidden403 "No such board"
                Right (powerlevel, board, modtuples) -> do
                  mDetails <- getPageDetails context Nothing
                  case mDetails of
                    Nothing      -> errorH internalServerError500 "Error preparing page"
                    Just details -> okHtml $ boardSettingsPromptL details powerlevel board modtuples
  where
    validation = validate $ boardSettingsPage uri_

boardSettingsH :: Context -> Text -> Text -> Text -> BlankMaybe Theme -> Text -> Int -> Int -> Int -> BoardPermission -> IndexViewPolicy -> Text -> Text -> [Text] -> IO (Maybe Response)
boardSettingsH context uri_ title_ description_ (BlankMaybe mTheme_) anonName_ bumpLimit_ replyLimit_ threadLimit_ permission_ indexViewPolicy_ addMod untoMods selectMods =
  case validation of
    Aborted             -> pure Nothing
    Invalid messages    -> errorListH badRequest400 messages
    Valid boardsettings -> do
      mUsername <- elicitUsername context
      case mUsername of
        Nothing        -> errorH forbidden403 "You are not logged in"
        Just username_ -> do
          mReceipt <- setBoardSettings context username_ uri_ boardsettings
          case mReceipt of
            Nothing      -> errorH conflict409 "Error setting board settings"
            Just receipt ->
              case receipt of
                Left (Left UserNotFound)         -> errorH forbidden403 "No user exists with the username in your cookie"
                Left (Left Forbidden)            -> errorH forbidden403 "You have no authority here"
                Left (Right (Left NoSuchBoard))  -> errorH badRequest400 "No such board"
                Left (Right (Right NotAUser))    -> errorH badRequest400 "New moderator doesn't exist"
                Left (Right (Right AlreadyAMod)) -> errorH badRequest400 "New moderator is already a mod"
                Right ()                         -> redirect $ "/.phi/auth/"
  where
    validation = validate $
      boardSettingsForm uri_ title_ description_ mTheme_ anonName_ bumpLimit_ replyLimit_ threadLimit_ permission_ indexViewPolicy_ addMod untoMods selectMods

changeBannersPromptH :: Context -> Text -> IO (Maybe Response)
changeBannersPromptH context uri_ = do
  mUsername <- elicitUsername context
  case mUsername of
    Nothing -> errorH forbidden403 "You are not logged in"
    Just username_ -> do
      mReceipt <- getUser context username_
      case mReceipt of
        Nothing      -> errorH internalServerError500 "Error fetching user"
        Just receipt ->
          case receipt of
            Nothing   -> errorH forbidden403 "No user exists with the username in your cookie"
            Just user -> do
              mReceipt' <- getBanners context username_ uri_
              case mReceipt' of
                Nothing       -> errorH conflict409 "Error fetching banners"
                Just receipt' -> do
                  case receipt' of
                    Left (Left NoSuchBoard)     -> errorH notFound404 "No such board"
                    Left (Right UserNotFound)   -> errorH forbidden403 "No user exists with the username in your cookie"
                    Left (Right Forbidden)      -> errorH forbidden403 "You have no authority here"
                    Right (powerlevel, banners) -> do
                      mDetails <- getPageDetails context Nothing
                      case mDetails of
                        Nothing      -> errorH internalServerError500 "Error preparing page"
                        Just details -> okHtml $ changeBannersPromptL details powerlevel uri_ banners

addBannerH :: Context -> Text -> Fn.File -> IO (Maybe Response)
addBannerH context uri_ fnFile = do
  mUsername <- elicitUsername context
  case mUsername of
    Nothing        -> errorH forbidden403 "You are not logged in"
    Just username_ -> do
      mReceipt <- addBanner context username_ uri_ fnFile
      case mReceipt of
        Nothing      -> errorH conflict409 "Error adding banner"
        Just receipt ->
          case receipt of
            Left (Left NoSuchBoard)           -> errorH notFound404 "No such board"
            Left (Right (Left UserNotFound))  -> errorH forbidden403 "No user exists with the username in your cookie"
            Left (Right (Left Forbidden))     -> errorH forbidden403 "You have no authority here"
            Left (Right (Right fileRejected)) -> fileRejectedH fileRejected
            Right ()                          -> redirect $ "/.phi/auth/board/banners/" <> uri_ <> "/"
  where
    fileRejectedH (FileTooLarge maxsize) =
      errorH requestEntityTooLarge413 $ "File exceeded " <> readableFilesize maxsize
    fileRejectedH (FileBadMime mimes) =
      errorH badRequest400 $ "Filetype not allowed (must be one of: " <> intercalate ", " mimes <> ")"
    fileRejectedH FileMissing =
      errorH badRequest400 $ "A file is required"

deleteBannersH :: Context -> Text -> [Text] -> IO (Maybe Response)
deleteBannersH context uri_ hashes = do
  mUsername <- elicitUsername context
  case mUsername of
    Nothing        -> errorH forbidden403 "You are not logged in"
    Just username_ -> do
      mReceipt <- deleteBanners context username_ uri_ hashes
      case mReceipt of
        Nothing      -> errorH conflict409 "Error deleting banners"
        Just receipt ->
          case receipt of
            Left (Left NoSuchBoard)           -> errorH notFound404 "No such board"
            Left (Right (Left UserNotFound))  -> errorH forbidden403 "No user exists with the username in your cookie"
            Left (Right (Left Forbidden))     -> errorH forbidden403 "You have no authority here"
            Left (Right (Right NoBanners))    -> errorH badRequest400 "No banners to delete"
            Left (Right (Right NoSuchBanner)) -> errorH badRequest400 "Not all selected banners exist"
            Right ()                          -> redirect $ "/.phi/auth/board/banners/" <> uri_ <> "/"

globalSettingsPromptH :: Context -> IO (Maybe Response)
globalSettingsPromptH context = do
  mUsername <- elicitUsername context
  case mUsername of
    Nothing -> errorH forbidden403 "You are not logged in"
    Just username_ -> do
      mReceipt <- getUser context username_
      case mReceipt of
        Nothing      -> errorH internalServerError500 "Error fetching user"
        Just receipt ->
          case receipt of
            Nothing   -> errorH forbidden403 "No user exists with the username in your cookie"
            Just user -> do
              if not (admin user)
              then errorH forbidden403 "You have no authority here"
              else do
                mDetails <- getPageDetails context Nothing
                case mDetails of
                  Nothing      -> errorH internalServerError500 "Error fetching existing boards"
                  Just details -> okHtml $ globalSettingsPromptL details

globalSettingsH :: Context -> Theme -> Bool -> Bool -> Bool -> IO (Maybe Response)
globalSettingsH context globalTheme_ openRegistration_ userBoardCreation_ captchaBaseline_ = do
  case validation of
    Aborted              -> pure Nothing
    Invalid messages     -> errorListH badRequest400 messages
    Valid globalsettings -> do
      mUsername <- elicitUsername context
      case mUsername of
        Nothing        -> errorH forbidden403 "You are not logged in"
        Just username_ -> do
          mReceipt <- setGlobalSettings context username_ globalsettings
          case mReceipt of
            Nothing      -> errorH conflict409 "Error setting global settings"
            Just receipt ->
              case receipt of
                Left UserNotFound -> errorH forbidden403 "No user exists with the username in your cookie"
                Left Forbidden    -> errorH forbidden403 "You have no authority here"
                Right ()          -> redirect "/.phi/auth/settings"
  where
    validation = validate $
      globalSettingsForm globalTheme_ openRegistration_ userBoardCreation_ captchaBaseline_

modH :: Context -> Text -> SuperMaybe Int -> SuperMaybe Bool -> Text -> [Text] -> IO (Maybe Response)
modH context modActionName (SuperMaybe mStickiness) (SuperMaybe mBoolean) reason postStrings =
  case validation of
    Aborted                       -> pure Nothing
    Invalid messages              -> errorListH badRequest400 messages
    Valid (modAction, postTuples) -> do
      mUsername <- elicitUsername context
      case mUsername of
        Nothing        -> errorH forbidden403 "You are not logged in"
        Just username_ -> do
          mReceipt <- moderate context username_ modAction postTuples
          case mReceipt of
            Nothing      -> errorH conflict409 "Error moderating"
            Just receipt ->
              case receipt of
                Left (Left UserNotFound)                 -> errorH forbidden403 "No user exists with the username in your cookie"
                Left (Left Forbidden)                    -> errorH forbidden403 "You have no authority here"
                Left (Right (Left NoSuchBoard))          -> errorH badRequest400 "At least one selected posts purports to belong to a board that does not actually exist"
                Left (Right (Right (Left noSuchThread))) -> errorH badRequest400 "At least one selected post is not an existing thread OP"
                Left (Right (Right (Right NoSuchPost)))  -> errorH badRequest400 "At least one selected post does not exist"
                Right nPerformedActions                  -> okHtml $ modCompleteL (length postStrings) nPerformedActions (getReferrerAsText context)
  where
    validation = validate $
      modForm modActionName mStickiness mBoolean reason postStrings
