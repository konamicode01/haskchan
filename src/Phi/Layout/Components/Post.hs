{-# LANGUAGE OverloadedStrings #-}

module Phi.Layout.Components.Post where

import           Prelude hiding ((++))

import           Control.Monad (when)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Default

import           Blaze.ByteString.Builder.Char8 (fromChar)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import           Data.ByteString.Builder (toLazyByteString)

import           Data.Time (timeToTimeOfDay)
import           Data.Time.Clock (UTCTime(UTCTime))
import           Database.SQLite.Simple.Time.Implementation (dayToBuilder, timeOfDayToBuilder)

import           Data.Text (Text, isPrefixOf)
import qualified Data.Text as T (null, pack)
import           Data.Text.Encoding (decodeUtf8)

import           Lucid

import           Phi.Database.Models
import           Phi.Layout.Attributes (loading_, poster_)
import           Phi.Files (readableFilesize)

formatDatetime :: Bool -> UTCTime -> Text
formatDatetime verbose (UTCTime day time) =
  decodeUtf8 . BSL.toStrict . toLazyByteString $ builder
  where
    builder
      | verbose   = dayToBuilder day ++ fromChar 'T' ++ timeOfDayToBuilder (timeToTimeOfDay time) ++ fromChar 'Z'
      | otherwise = dayToBuilder day ++ fromChar ' ' ++ timeOfDayToBuilder (timeToTimeOfDay time)
    (++) = mappend

verboseDatetime :: UTCTime -> Text
verboseDatetime = formatDatetime True

terseDatetime :: UTCTime -> Text
terseDatetime = formatDatetime False

instance ToHtml UTCTime where
  toHtmlRaw = toHtmlRaw . terseDatetime
  toHtml = toHtmlRaw

data PostDecoration = PostDecoration
  { reply_ :: Bool
  , sidearrows :: Bool
  , parent :: Bool
  } deriving (Eq, Show)

instance Default PostDecoration where
  def = PostDecoration
    { reply_ = True
    , sidearrows = True
    , parent = False
    }

hasThreadSettings :: Thread -> Bool
hasThreadSettings thread = stickiness thread > 0 || lock thread /= Free

threadSettingsL' :: Thread -> Html ()
threadSettingsL' thread = do
  when (stickiness thread > 0) $
    abbr_ [title_ "Sticky"] "(S)"
  case lock thread of
    Free             -> pure ()
    Bumplocked       -> abbr_ [title_ "Bumplocked"] "(B)"
    Cyclic           -> abbr_ [title_ "Cyclic"] "(C)"
    Locked           -> abbr_ [title_ "Locked"] "(L)"
    LockedBumplocked -> abbr_ [title_ "Locked"] "(L)" <> abbr_ [title_ "Bumplocked"] "(B)"
    LockedCyclic     -> abbr_ [title_ "Locked"] "(L)" <> abbr_ [title_ "Cyclic"] "(C)"
    Full             -> abbr_ [title_ "Full"] "(F)"

opL' :: Either Int Thread -> FPost -> Html ()
opL' = postL' $ def {reply_ = False}

indexOpL' :: Either Int Thread -> FPost -> Html ()
indexOpL' = postL' def

replyL' :: Either Int Thread -> FPost -> Html ()
replyL' = postL' def

postL' :: PostDecoration -> Either Int Thread -> FPost -> Html ()
postL' (PostDecoration showReply showSidearrows showParent) eNoThread (post, mFile, quotes) = do
  div_ [class_ $ if isOp then "post-container op" else "post-container"] $ do
    when (not isOp && showSidearrows) $
      span_ [class_ "sidearrows"] ">>"
    if isOp
    then
      article_
        [ id_ $ "post" <> (T.pack . show $ no post)
        , class_ "post op"
        , data_ "origin" (pOrigin post)
        ] $ do
        case mFile of
          Nothing   -> pure ()
          Just file -> postFileHeader file <> postFile file
        postHeader
        blockquote_ [class_ "post-message"] $
          message post
    else
      article_
        [ id_ $ "post" <> (T.pack . show $ no post)
        , class_ "post"
        , data_ "origin" (pOrigin post)
        ] $ do
        postHeader
        case mFile of
          Nothing   -> pure ()
          Just file -> postFileHeader file <> postFile file
        blockquote_ [class_ "post-message"] $
          message post
  where
    isOp :: Bool
    isOp = isNothing $ pThreadNo post

    url file = "/.phi/static/file/" <> hash file <> ext file
    thumbUrl file =
      if hasThumb file
      then "/.phi/varstatic/thumb/" <> hash file
      else "/.phi/static/antithumb.png"

    threadNo :: Int
    threadNo =
      case eNoThread of
        Left  no_    -> no_
        Right thread -> tPostNo thread

    postHeader :: Html ()
    postHeader = do
      header_ [class_ "post-header"] $ do
        input_ [id_ $ "mod-" <> pBoardUri post <> "-" <> (T.pack . show $ no post), class_ "mod-checkbox", type_ "checkbox", name_ "uri-no", value_ $ pBoardUri post <> "-" <> (T.pack . show $ no post), accesskey_ "x"]
        when showParent $ do
          span_ [class_ "post-parent"] $ do
            if isOp
            then
              a_ [href_ $ "/" <> pBoardUri post <> "/"] $
                "/" <> (toHtml $ pBoardUri post) <> "/"
            else
              a_ [href_ $ "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ threadNo)] $
                "/" <> (toHtml $ pBoardUri post) <> "/" <> (toHtml $ show threadNo)
          toHtmlRaw (" &rarr; " :: Text)
        when (not . T.null $ subject post) $ do
          span_ [class_ "post-subject"] $
            toHtml $ subject post
          " "
        if T.null $ email post
        then
          span_ [class_ $ if isNothing (capcode post) then "post-name" else "post-name adjacent-capcode"] $
            toHtml $ name post
        else
          a_ [class_ $ if isNothing (capcode post) then "post-name" else "post-name adjacent-capcode", href_ $ "mailto:" <> email post] $
            toHtml $ name post
        case capcode post of
          Nothing       -> pure ()
          Just capcode_ -> do
            " "
            span_ [class_ "post-capcode"] $ do
              span_ "## "
              span_ $ toHtml capcode_
        case tripcode post of
          Nothing        -> pure ()
          Just tripcode_ -> do
            span_ [class_ "post-tripcode"] $ do
              span_ [class_ "post-tripcode-mark"] " #|"
              span_ $ toHtml tripcode_
        " "
        time_ [class_ "post-datetime", datetime_ $ verboseDatetime (datetime post)] $
          toHtml $ datetime post
        " "
        a_ [class_ "post-no-literal", href_ $ "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ threadNo) <> "#post" <> (T.pack . show $ no post)] $
          "No."
        a_ [class_ "post-no", href_ $ "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ threadNo) <> "#postform"] $
          toHtml . T.pack . show $ no post
        case eNoThread of
          Left  _no    -> pure ()
          Right thread ->
            when (isOp && hasThreadSettings thread) $ do
              " "
              span_ [class_ "thread-settings"] $
                threadSettingsL' thread
        " "
        a_ [class_ "post-modbutton", href_ "#modform"] "[M]"
        when (isOp && showReply) $ do
          " "
          span_ [class_ "thread-reply"] $
            "[" <> a_ [href_ $ "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ threadNo)] "Reply" <> "]"
        when (not . null $ quotes) $ do
          small_ [class_ "quotes"] $
            mconcat $ (flip map) quotes $ \quote -> do
              " "
              a_ [href_ $ "/" <> pBoardUri post <> "/thread/" <> (T.pack . show $ threadNo) <> "#post" <> (T.pack . show $ qChildNo quote)] $
                ">>" <> (toHtml . show $ qChildNo quote)

    postFileHeader :: File -> Html ()
    postFileHeader file = do
      header_ [class_ "post-file-header"] $ do
        "File:" <> toHtmlRaw ("&nbsp;" :: Text) <> a_ [href_ $ url file] (toHtml $ hash file <> ext file)
        " (" <> toHtml (readableFilesize $ size file) <> ", " <> toHtml (fromMaybe "unk" $ mime file) <> ")"

    postFile :: File -> Html ()
    postFile file
      | fromMaybe False $ ("audio/" `isPrefixOf`) <$> mime file =
        audio_ [class_ "post-file", src_ $ url file, controls_ "", loop_ "", preload_ "none"] ""
      | fromMaybe False $ ("video/" `isPrefixOf`) <$> mime file =
        video_ [class_ "post-file", src_ $ url file, poster_ $ thumbUrl file, controls_ "", loop_ "", preload_ "none"] ""
      | otherwise = do
        a_ [href_ $ url file, target_ "blank"] $
          let
            imgAttributes =
              case (thumbWidth file, thumbHeight file) of
                (Just w,  Just h)  -> [width_  (T.pack . show $ w), height_ (T.pack . show $ h)]
                (Just w,  Nothing) -> [width_  (T.pack . show $ w)]
                (Nothing, Just h)  -> [height_ (T.pack . show $ h)]
                (Nothing, Nothing) -> []
            in
            img_ $ class_ "post-file" : src_ (thumbUrl file) : loading_ "lazy" : imgAttributes
