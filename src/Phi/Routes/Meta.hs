{-# LANGUAGE OverloadedStrings #-}

module Phi.Routes.Meta where

import           Data.Maybe (fromMaybe, isNothing)
import           Data.Text (Text, intercalate)
import qualified Data.Text as T (pack)
import           Data.Text.Encoding (encodeUtf8)

import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai (Response, mapResponseHeaders)
import           Web.Fn hiding (okHtml)
import qualified Web.Fn as Fn (File(..), file)

import           Phi.Auth (mkAuthCookie, elicitUsername, isLoggedIn)
import           Phi.Captcha (getCaptcha, makeAndSaveNewCaptcha, enforceCaptcha, enforceCaptchaForBoard)
import           Phi.Context (Context(captcha, static), getReferrer)
import           Phi.Database.Models (Board(uri), Post(no), PageDetails(..), GlobalSettings(openRegistration), SuperMaybe(..))
import           Phi.Database.Queries
import           Phi.Database.Queries.Types
import           Phi.Files (readableFilesize)
import           Phi.Forms (postForm, registerForm, loginForm, validate, Validation(..))
import           Phi.HTTP (okHtml, respondHtmlWithHeaders)
import           Phi.Layout.Pages.Log
import           Phi.Layout.Pages.Recent
import           Phi.Layout.Pages.Forms.Login
import           Phi.Layout.Pages.Forms.Register
import           Phi.Routes.Errors
import           Phi.Routes.Meta.Varstatic (varstaticH)

metaRoutes :: Context -> IO (Maybe Response)
metaRoutes context = route context
  [ method GET // path "static" ==> staticH
  , method GET // path "varstatic" ==> varstaticH
  , method POST // path "post"
                // param "board"
                // param "thread"
                // param "name"
                // param "email"
                // param "subject"
                // param "message"
                // Fn.file "file"
                // param "captcha"
                // end !=> makePostH
  , method GET // path "register"
               // end ==> registerPromptH
  , method POST // path "register"
                // param "username"
                // param "password"
                // param "password-again"
                // param "captcha"
                // end ==> registerH
  , method GET // path "login"
               // end ==> loginPromptH
  , method POST // path "login"
                // param "username"
                // param "password"
                // param "captcha"
                // end ==> loginH
  , method GET // path "captcha.jpg"
               // end ==> captchaH
  , method GET // path "captcha-refresh.jpg"
               // end ==> captchaRefreshH
  , method GET // path "log"
               // end ==> logH
  , method GET // path "recent"
               // param "show"
               // param "board"
               // end ==> recentH
  ,                path "settings"
                // param "theme"
                // end !=> settingsH
  ]

instance Show Fn.File where
  show fnFile = "Fn.File { fileName = " <> show (Fn.fileName fnFile) <> ", fileContentType = " <> show (Fn.fileContentType fnFile) <> ", filePath = " <> show (Fn.filePath fnFile) <> " }"

staticH :: Context -> IO (Maybe Response)
staticH context = staticServe (T.pack $ static context) context

captchaH :: Context -> IO (Maybe Response)
captchaH context = do
  mFilename <- getCaptcha context
  case mFilename of
    Nothing       -> errorH internalServerError500 "Error fetching captcha"
    Just filename -> addCaptchaHeaders <$> sendFile (captcha context <> "/" <> filename)

captchaRefreshH :: Context -> IO (Maybe Response)
captchaRefreshH context = do
  mFilename <- makeAndSaveNewCaptcha context
  case mFilename of
    Nothing       -> errorH internalServerError500 "Error generating captcha"
    Just filename -> addCaptchaHeaders <$> sendFile (captcha context <> "/" <> filename)

addCaptchaHeaders :: Maybe Response -> Maybe Response
addCaptchaHeaders =
  fmap $ mapResponseHeaders $ \headers ->
    [ ("cache-control", "no-store, no-cache, must-revalidate, private")
    , ("pragma", "no-cache")
    , ("expires", "0")
    ] ++ headers

settingsH :: Context -> Text -> IO (Maybe Response)
settingsH context themeName =
  respondHtmlWithHeaders headers seeOther303 ""
  where
    returnUrl = fromMaybe "/" $ getReferrer context
    headers =
      if themeName `elem` ["phichannel", "nanochan", "yotsuba", "haskchan"]
      then [ ("set-cookie", "theme=" <> encodeUtf8 themeName <> "; path=/")
           , ("location", returnUrl <> "#bottom")
           ]
      else [ ("set-cookie", "theme=; path=/; expires=Thu, Jan 01 1970 00:00:00 GMT")
           , ("location", returnUrl <> "#bottom")
           ]

logH :: Context -> IO (Maybe Response)
logH context = do
  mLogs <- getLogs context
  case mLogs of
    Nothing   -> errorH conflict409 "Error fetching logs"
    Just logs -> do
      mDetails <- getPageDetails context Nothing
      case mDetails of
        Nothing      -> errorH internalServerError500 "Error preparing page"
        Just details -> okHtml $ logL details logs

recentH :: Context -> SuperMaybe Bool -> [Text] -> IO (Maybe Response)
recentH context (SuperMaybe mWhitelist) uris = do
  mReceipt <- getRecent context 128 uriFilter
  case mReceipt of
    Nothing                    -> errorH conflict409 "Error fetching recent posts"
    Just (tuples, boardFilter) -> do
      mDetails <- getPageDetails context Nothing
      case mDetails of
        Nothing      -> errorH internalServerError500 "Error preparing page"
        Just details -> okHtml $ recentL details openFilterDetails whitelist boardFilter tuples
  where
    uriFilter :: (Bool, [Text])
    uriFilter@(whitelist, _) =
      case (mWhitelist, uris) of
        (Nothing, [])       -> (False, [])
        (Nothing, _)        -> (True, uris)
        (Just whitelist, _) -> (whitelist, uris)

    openFilterDetails :: Bool
    openFilterDetails = not $ null uris && isNothing mWhitelist

makePostH :: Context -> Text -> Maybe Int -> Text -> Text -> Text -> Text -> Fn.File -> Maybe Text -> IO (Maybe Response)
makePostH context uri_ mThreadNo name_ email_ subject_ message_ fnFile mWork =
  enforceCaptchaForBoard context uri_ mWork captchaFail $
    case validation of
      Aborted          -> pure Nothing
      Invalid messages -> errorListH badRequest400 messages
      Valid newpost -> do
        mUsername <- elicitUsername context
        case mThreadNo of
          Nothing -> do
            mReceipt <- makeOp context mUsername uri_ newpost fnFile
            case mReceipt of
              Nothing      -> errorH conflict409 "Error making thread"
              Just receipt ->
                case receipt of
                  Left (Left NoSuchBoard)            -> errorH badRequest400 "No such board"
                  Left (Right (Left PermissionFail)) -> errorH forbidden403 "You cannot start new threads on this board"
                  Left (Right (Right fileRejected))  -> fileRejectedH fileRejected
                  Right post                         -> redirect $ "/" <> uri_ <> "/thread/" <> (T.pack . show $ no post)
          Just threadNo -> do
            mReceipt <- makeReply context mUsername uri_ threadNo newpost fnFile
            case mReceipt of
              Nothing      -> errorH conflict409 "Error making reply"
              Just receipt ->
                case receipt of
                  Left (Left NoSuchBoard)                           -> errorH badRequest400 "No such board"
                  Left (Right (Left DeletedThread))                 -> errorH gone410 "No such thread"
                  Left (Right (Left FutureThread))                  -> errorH notFound404 "No such thread"
                  Left (Right (Right (Left (Left PermissionFail)))) -> errorH forbidden403 "You cannot post on this board"
                  Left (Right (Right (Left (Right LockedThread))))  -> errorH badRequest400 "This thread is locked"
                  Left (Right (Right (Left (Right FullThread))))    -> errorH badRequest400 "This thread is full"
                  Left (Right (Right (Right fileRejected)))         -> fileRejectedH fileRejected
                  Right post                                        -> redirect $ "/" <> uri_ <> "/thread/" <> (T.pack . show $ threadNo) <> "#post" <> (T.pack . show $ no post)
  where
    validation = validate $
      postForm uri_ mThreadNo name_ email_ subject_ message_

    captchaFail False =
      errorH internalServerError500 "Error fetching global settings"
    captchaFail True =
      case mWork of
        Nothing -> errorH badRequest400 "Missing captcha"
        Just _  -> errorH badRequest400 "Wrong or expired captcha"

    fileRejectedH (FileTooLarge maxsize) =
      errorH requestEntityTooLarge413 $ "File exceeded " <> readableFilesize maxsize
    fileRejectedH (FileBadMime mimes) =
      errorH badRequest400 $ "Filetype not allowed (must be one of: " <> intercalate ", " mimes <> ")"
    fileRejectedH FileMissing =
      errorH badRequest400 $ "A file is required"

loginPromptH :: Context -> IO (Maybe Response)
loginPromptH context = do
  bool <- isLoggedIn context
  if bool
  then redirect "/.phi/auth/"
  else do
    mDetails <- getPageDetails context Nothing
    case mDetails of
      Nothing      -> errorH internalServerError500 "Error preparing page"
      Just details -> okHtml $ loginPromptL details

registerPromptH :: Context -> IO (Maybe Response)
registerPromptH context = do
  mDetails <- getPageDetails context Nothing
  case mDetails of
    Nothing -> errorH internalServerError500 "Error preparing page"
    Just details ->
      if not (openRegistration $ pdGlobalSettings details)
      then errorH forbidden403 "Registration is closed"
      else okHtml $ registerPromptL details

loginH :: Context -> Text -> Text -> Text -> IO (Maybe Response)
loginH context username_ password work =
  enforceCaptcha context work (errorH badRequest400 "Wrong or expired captcha") $
    case validation of
      Aborted          -> pure Nothing
      Invalid messages -> errorListH badRequest400 messages
      Valid ()         -> do
        mReceipt <- getUserDuringLogin context username_ password
        case mReceipt of
          Nothing      -> errorH conflict409 "Error fetching user"
          Just receipt -> do
            case receipt of
              Left NoSuchUser    -> errorH forbidden403 "Wrong username or password"
              Left WrongPassword -> errorH forbidden403 "Wrong username or password"
              Right user         -> do
                mSetCookie <- mkAuthCookie context user
                case mSetCookie of
                  Nothing        -> errorH internalServerError500 "Error making cookie"
                  Just setCookie -> do
                    let headers = [("location", "/.phi/auth/"), ("set-cookie", setCookie)]
                    respondHtmlWithHeaders headers status303 ""
  where
    validation = validate $ loginForm username_ password

registerH :: Context -> Text -> Text -> Text -> Text -> IO (Maybe Response)
registerH context username password passwordAgain work =
  enforceCaptcha context work (errorH badRequest400 "Wrong or expired captcha") $
    case validation of
      Aborted          -> pure Nothing
      Invalid messages -> errorListH badRequest400 messages
      Valid newuser -> do
        mReceipt <- makeUser context newuser
        case mReceipt of
          Nothing      -> errorH conflict409 "Error making user"
          Just receipt ->
            case receipt of
              Left ExtantUser         -> errorH badRequest400 "Username is taken"
              Left ClosedRegistration -> errorH forbidden403 "Registration is closed"
              Right ()                -> redirect "/.phi/login"
  where
    validation = validate $ registerForm username password passwordAgain
