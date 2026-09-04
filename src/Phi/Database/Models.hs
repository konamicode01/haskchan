{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Phi.Database.Models where

import Lucid (Html, renderText, toHtmlRaw)
import Data.Default
import Data.Password.Argon2
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import Web.Fn (FromParam(..), ParamError(..))

import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite3 (SQLData(SQLInteger, SQLText))

-- Database tables

data Board = Board
  { uri :: Text
  , title :: Text
  , description :: Text
  , mTheme :: Maybe Theme
  , anonName :: Text
  , bumpLimit :: Int
  , replyLimit :: Int
  , threadLimit :: Int
  , permission :: BoardPermission
  , indexViewPolicy :: IndexViewPolicy
  , totalPosts :: Int
  , created :: UTCTime
  , ownerName :: Text
  } deriving (Eq, Show)

data Banner = Banner
  { bnBoardUri :: Text
  , bnHash :: FileHash
  , bnExt :: Text
  } deriving (Eq, Show)

data Thread = Thread
  { tBoardUri :: Text
  , tPostNo :: Int
  , lastActivity :: UTCTime
  , bumped :: UTCTime
  , nReplies :: Int
  , nFiles :: Int
  , stickiness :: Int
  , lock :: Lock
  } deriving (Eq, Show)

data Post = Post
  { pBoardUri :: Text
  , no :: Int
  , pThreadNo :: Maybe Int
  , sage :: Bool
  , name :: Text
  , tripcode :: Maybe Text
  , capcode :: Maybe Text
  , email :: Text
  , subject :: Text
  , datetime :: UTCTime
  , nomarkup :: Text
  , message :: Html ()
  , fileHash :: Maybe Text
  } deriving (Eq, Show)

data File = File
  { hash :: FileHash
  , ext :: Text
  , size :: Int
  , hasThumb :: Bool
  , thumbWidth :: Maybe Int
  , thumbHeight :: Maybe Int
  , mime :: Maybe Text
  } deriving (Eq, Show)

data Quote = Quote
  { qBoardUri :: Text
  , qParentNo :: Int
  , qChildNo :: Int
  } deriving (Eq, Show)

data User = User
  { username :: Text
  , pwhash :: PasswordHash Argon2
  , admin :: Bool
  , lastActive :: UTCTime
  } deriving (Eq, Show)

data Log = Log
  { logId :: Int
  , logDatetime :: UTCTime
  , logUsername :: Text
  , logBoardUri :: Text
  , logMPostNo :: Maybe Int
  , logAction :: LogAction
  , logReason :: Text
  } deriving (Eq, Show)

data GlobalSettings = GlobalSettings
  { globalTheme :: Theme
  , openRegistration :: Bool
  , userBoardCreation :: Bool
  , captchaBaseline :: Bool
  } deriving (Eq, Show)

instance Default GlobalSettings where
  def = GlobalSettings
    { globalTheme = Haskchan
    , openRegistration = True
    , userBoardCreation = False
    , captchaBaseline = True
    }

data CookieSettings = CookieSettings
  { cookieTheme :: Maybe Theme
  } deriving (Eq, Show)

instance Default CookieSettings where
  def = CookieSettings
    { cookieTheme = Nothing
    }

type FPost = (Post, Maybe File, [Quote])
type Reply = (Post, Maybe File, [Quote])
type OP = (Thread, FPost)
type Hull = (OP, [Reply])

data PageDetails = PageDetails
  { pdTopnav :: [Board]
  , pdTheme :: Theme
  , pdCookieSettings :: CookieSettings
  , pdGlobalSettings :: GlobalSettings
  } deriving (Eq, Show)

-- This is used on error pages where we don't do any more database queries
-- after one has failed already (having caused the error). Using a default
-- PageSettings like this will ignore the global theme and not show any boards
-- in the topnav which is not ideal.
-- TODO: something other than this
instance Default PageDetails where
  def = PageDetails
    { pdTopnav = []
    , pdTheme = Phichannel
    , pdCookieSettings = def
    , pdGlobalSettings = def
    }

type FileHash = Text

-- Datatypes constructed just before database insertions happen

data NewBoard = NewBoard
  { nbUri :: Text
  , nbTitle :: Text
  , nbDescription :: Text
  } deriving (Eq, Show)

data BoardSettings = BoardSettings
  { bsTitle :: Text
  , bsDescription :: Text
  , bsMTheme :: Maybe Theme
  , bsAnonName :: Text
  , bsBumpLimit :: Int
  , bsReplyLimit :: Int
  , bsThreadLimit :: Int
  , bsPermission :: BoardPermission
  , bsIndexViewPolicy :: IndexViewPolicy
  , bsAddMod :: Maybe Text
  , bsUntoMods :: Maybe Bool
  , bsSelectMods :: [Text]
  } deriving (Eq, Show)

data NewPost = NewPost
  { npName :: Text
  , npHashtext :: Text
  , npEmail :: Text
  , npSubject :: Text
  , npNomarkup :: Text
  , npMessage :: Html ()
  , npQuotes :: ([Int], [Text], [(Text, Int)])
  } deriving (Eq, Show)

data NewUser = NewUser
  { nuUsername :: Text
  , nuPassword :: Password
  } deriving Show

-- Database columns

data Theme
  = Phichannel
  | Nanochan
  | Yotsuba
  | Haskchan
  deriving (Eq, Show)

themeUrl :: Theme -> Text
themeUrl Phichannel = "/.phi/static/style.css"
themeUrl Nanochan   = "/.phi/static/nanochan.css"
themeUrl Yotsuba    = "/.phi/static/yotsuba.css"
themeUrl Haskchan   = "/.phi/static/haskchan.css"

data BoardPermission
  = AnyThreadsAnyReplies
  | ModThreadsAnyReplies
  | NilThreadsAnyReplies
  | NilThreadsNilReplies
  deriving (Eq, Show)

data Lock
  = Free
  | Bumplocked
  | Cyclic
  | Locked
  | LockedBumplocked
  | LockedCyclic
  | Full
  deriving (Eq, Show)

data IndexViewPolicy
  = IndexViewDisallowed
  | IndexViewAllowed
  | IndexViewPreferred
  deriving (Eq, Show)

data ModAction
  = Sticky Text Int
  | Cycle Text Bool
  | Lock Text Bool
  | Bumplock Text Bool
  | UnlinkFile Text
  | PurgeFile Text
  | Delete Text
  deriving (Eq, Show)

data LogAction
  = SetStickinessTo Int
  | SetCyclicTo Bool
  | SetLockTo Bool
  | SetBumplockTo Bool
  | DidUnlinkFile FileHash Int (Maybe Text)
  | DidPurgeFile FileHash Int (Maybe Text)
  | DidDeletePost
  | DidDeleteThread
  | DidDeleteBoard
  deriving (Eq, Show)

-- Typeclass instances for fn

instance FromParam BoardPermission where
  fromParam ["0"] = Right AnyThreadsAnyReplies
  fromParam ["1"] = Right ModThreadsAnyReplies
  fromParam ["2"] = Right NilThreadsAnyReplies
  fromParam ["3"] = Right NilThreadsNilReplies
  fromParam [_]   = Left ParamUnparsable
  fromParam []    = Left ParamMissing
  fromParam _     = Left ParamTooMany

instance FromParam IndexViewPolicy where
  fromParam ["0"] = Right IndexViewDisallowed
  fromParam ["1"] = Right IndexViewAllowed
  fromParam ["2"] = Right IndexViewPreferred
  fromParam [_]   = Left ParamUnparsable
  fromParam []    = Left ParamMissing
  fromParam _     = Left ParamTooMany

instance FromParam Theme where
  fromParam ["0"] = Right Phichannel
  fromParam ["1"] = Right Nanochan
  fromParam ["2"] = Right Yotsuba
  fromParam ["3"] = Right Haskchan
  fromParam [_]   = Left ParamUnparsable
  fromParam []    = Left ParamMissing
  fromParam _     = Left ParamTooMany

-- The empty string means Nothing and absence is an error.
newtype BlankMaybe a = BlankMaybe (Maybe a)
instance FromParam a => FromParam (BlankMaybe a) where
  fromParam [""] = Right $ BlankMaybe Nothing
  fromParam ps   = BlankMaybe . Just <$> fromParam ps -- :: Either ParamError a

-- The empty string and absence both mean Nothing.
newtype SuperMaybe a = SuperMaybe (Maybe a)
instance FromParam a => FromParam (SuperMaybe a) where
  fromParam [""] = Right $ SuperMaybe Nothing
  fromParam []   = Right $ SuperMaybe Nothing
  fromParam ps   = SuperMaybe . Just <$> fromParam ps -- :: Either ParamError a

instance FromParam Bool where
  fromParam ["on"]  = Right True
  fromParam ["1"]   = Right True
  fromParam ["y"]   = Right True
  fromParam ["yes"] = Right True
  fromParam ["off"] = Right False
  fromParam ["0"]   = Right False
  fromParam ["n"]   = Right False
  fromParam ["no"]  = Right False
  fromParam []      = Right False
  fromParam [_]     = Left ParamUnparsable
  fromParam _       = Left ParamTooMany

-- Typeclass instances for SQLite

instance Eq (Html ()) where
  html1 == html2 = renderText html1 == renderText html2

takeInt :: Field -> Ok Int
takeInt (Field (SQLInteger i) _) = Ok . fromIntegral $ i
takeInt f                        = returnError ConversionFailed f "need an int"

instance ToField Theme where
  toField Phichannel = toField (0 :: Int)
  toField Nanochan   = toField (1 :: Int)
  toField Yotsuba    = toField (2 :: Int)
  toField Haskchan   = toField (3 :: Int)

instance FromField Theme where
  fromField f =
    case takeInt f of
      Ok 0 -> Ok Phichannel
      Ok 1 -> Ok Nanochan
      Ok 2 -> Ok Yotsuba
      Ok 3 -> Ok Haskchan
      _    -> returnError ConversionFailed f "encountered disallowed value (allowed values: 0, 1, 2, 3)"

instance ToField BoardPermission where
  toField AnyThreadsAnyReplies = toField (0 :: Int)
  toField ModThreadsAnyReplies = toField (1 :: Int)
  toField NilThreadsAnyReplies = toField (2 :: Int)
  toField NilThreadsNilReplies = toField (3 :: Int)

instance FromField BoardPermission where
  fromField f =
    case takeInt f of
      Ok 0 -> Ok AnyThreadsAnyReplies
      Ok 1 -> Ok ModThreadsAnyReplies
      Ok 2 -> Ok NilThreadsAnyReplies
      Ok 3 -> Ok NilThreadsNilReplies
      _    -> returnError ConversionFailed f "encountered disallowed value (allowed values: 0, 1, 2, 3, 3)"

instance ToField IndexViewPolicy where
  toField IndexViewDisallowed = toField (0 :: Int)
  toField IndexViewAllowed    = toField (1 :: Int)
  toField IndexViewPreferred  = toField (2 :: Int)

instance FromField IndexViewPolicy where
  fromField f =
    case takeInt f of
      Ok 0 -> Ok IndexViewDisallowed
      Ok 1 -> Ok IndexViewAllowed
      Ok 2 -> Ok IndexViewPreferred
      _    -> returnError ConversionFailed f "encountered disallowed value (allowed values: 0, 1, 2, 3)"

instance ToField Lock where
  toField Free             = toField (0 :: Int)
  toField Bumplocked       = toField (1 :: Int)
  toField Cyclic           = toField (2 :: Int)
  toField Locked           = toField (4 :: Int)
  toField LockedBumplocked = toField (5 :: Int)
  toField LockedCyclic     = toField (6 :: Int)
  toField Full             = toField (8 :: Int)

instance FromField Lock where
  fromField f =
    case takeInt f of
      Ok 0 -> Ok Free
      Ok 1 -> Ok Bumplocked
      Ok 2 -> Ok Cyclic
      Ok 4 -> Ok Locked
      Ok 5 -> Ok LockedBumplocked
      Ok 6 -> Ok LockedCyclic
      Ok 8 -> Ok Full
      _    -> returnError ConversionFailed f "encountered disallowed value (allowed values: 0, 1, 2, 3, 4, 5, 6, 8)"

instance ToField (Html ()) where
  toField = toField . renderText

instance FromField (Html ()) where
  fromField (Field (SQLText txt) _) = Ok . toHtmlRaw $ txt
  fromField f                       = returnError ConversionFailed f "need a text"

instance ToField (PasswordHash Argon2) where
  toField = toField . unPasswordHash

instance FromField (PasswordHash Argon2) where
  fromField (Field (SQLText txt) _) = Ok . PasswordHash $ txt
  fromField f                       = returnError ConversionFailed f "need a text"

instance ToRow Board where
  toRow (Board uri_ title_ description_ mTheme_ anonName_ bumpLimit_ replyLimit_ threadLimit_ permission_ indexViewPolicy_ totalPosts_ created_ ownerName_) =
    [ toField uri_
    , toField title_
    , toField description_
    , toField mTheme_
    , toField anonName_
    , toField bumpLimit_
    , toField replyLimit_
    , toField threadLimit_
    , toField permission_
    , toField indexViewPolicy_
    , toField totalPosts_
    , toField created_
    , toField ownerName_
    ]

instance FromRow Board where
  fromRow = Board <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Banner where
  toRow (Banner bnBoardUri_ bnHash_ bnExt_) =
    [ toField bnBoardUri_
    , toField bnHash_
    , toField bnExt_
    ]

instance FromRow Banner where
  fromRow = Banner <$> field <*> field <*> field

instance ToRow Thread where
  toRow (Thread tBoardUri_ tPostNo_ lastActivity_ bumped_ nReplies_ nFiles_ stickiness_ lock_) =
    [ toField tBoardUri_
    , toField tPostNo_
    , toField lastActivity_
    , toField bumped_
    , toField nReplies_
    , toField nFiles_
    , toField stickiness_
    , toField lock_
    ]

instance FromRow Thread where
  fromRow = Thread <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Post where
  toRow (Post pBoardUri_ no_ pThreadNo_ sage_ name_ tripcode_ capcode_ email_ subject_ datetime_ nomarkup_ message_ fileHash_) =
    [ toField pBoardUri_
    , toField no_
    , toField pThreadNo_
    , toField sage_
    , toField name_
    , toField tripcode_
    , toField capcode_
    , toField email_
    , toField subject_
    , toField datetime_
    , toField nomarkup_
    , toField message_
    , toField fileHash_
    ]

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow File where
  toRow (File hash_ size_ ext_ hasThumb_ thumbWidth_ thumbHeight_ mime_) =
    [ toField hash_
    , toField size_
    , toField ext_
    , toField hasThumb_
    , toField thumbWidth_
    , toField thumbHeight_
    , toField mime_
    ]

instance FromRow File where
  fromRow = File <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Quote where
  toRow (Quote qBoardUri_ qParentNo_ qChildNo_) =
    [ toField qBoardUri_
    , toField qParentNo_
    , toField qChildNo_
    ]

instance FromRow Quote where
  fromRow = Quote <$> field <*> field <*> field

instance ToRow User where
  toRow (User username_ pwhash_ admin_ lastActive_) =
    [ toField username_
    , toField pwhash_
    , toField admin_
    , toField lastActive_
    ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToRow GlobalSettings where
  toRow (GlobalSettings globalTheme_ openRegistraion_ userBoardCreation_ captchaBaseline_) =
    [ toField globalTheme_
    , toField openRegistraion_
    , toField userBoardCreation_
    , toField captchaBaseline_
    ]

instance FromRow GlobalSettings where
  fromRow = GlobalSettings <$> field <*> field <*> field <*> field

instance ToRow Log where
  toRow = logToRow

logToRow :: Log -> [SQLData]
logToRow (Log logId_ logDatetime_ logUsername_ logBoardUri_ logMPostNo_ logAction_ logReason_) =
  [ toField logId_
  , toField logDatetime_
  , toField logUsername_
  , toField logBoardUri_
  , toField logMPostNo_
  , toField mFileHash
  , toField mFileSize
  , toField mFileMime
  , toField action
  , toField mValue
  , toField logReason_
  ]
  where
    action :: Int
    mValue :: Maybe Int
    mFileHash :: Maybe Text
    mFileSize :: Maybe Int
    mFileMime :: Maybe Text
    (action, mValue, mFileHash, mFileSize, mFileMime) =
      case logAction_ of
        SetStickinessTo n               -> (0, Just n,            Nothing,    Nothing,    Nothing)
        SetCyclicTo bool                -> (1, Just $ toInt bool, Nothing,    Nothing,    Nothing)
        SetLockTo bool                  -> (2, Just $ toInt bool, Nothing,    Nothing,    Nothing)
        SetBumplockTo bool              -> (3, Just $ toInt bool, Nothing,    Nothing,    Nothing)
        DidUnlinkFile hash_ size_ mMime -> (4, Nothing,           Just hash_, Just size_, mMime)
        DidPurgeFile hash_ size_ mMime  -> (5, Nothing,           Just hash_, Just size_, mMime)
        DidDeletePost                   -> (6, Nothing,           Nothing,    Nothing,    Nothing)
        DidDeleteThread                 -> (7, Nothing,           Nothing,    Nothing,    Nothing)
        DidDeleteBoard                  -> (8, Nothing,           Nothing,    Nothing,    Nothing)

    toInt :: Bool -> Int
    toInt True = 1
    toInt False = 0

instance FromRow Log where
  fromRow = toLog <$> tupleFromRow

type LogTuple = (Int, UTCTime, Text, Text, Maybe Int, Maybe FileHash, Maybe Int, Maybe Text, Int, Maybe Int, Text)

tupleFromRow :: RowParser LogTuple
tupleFromRow =
  (\f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 -> (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)) <$>
    field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

toLog :: LogTuple -> Log
toLog (logId_, logDatetime_, logUsername_, logBoardUri_, logMPostNo_, mFileHash, mFileSize, mFileMime, action, mValue, logReason_) =
  Log logId_ logDatetime_ logUsername_ logBoardUri_ logMPostNo_ logAction_ logReason_
  where
    logAction_ :: LogAction
    logAction_ =
      case (action, mValue, mFileHash, mFileSize, mFileMime) of
        (0, Just n,  Nothing,    Nothing,    Nothing) -> SetStickinessTo n
        (1, Just 0,  Nothing,    Nothing,    Nothing) -> SetCyclicTo False
        (1, Just 1,  Nothing,    Nothing,    Nothing) -> SetCyclicTo True
        (2, Just 0,  Nothing,    Nothing,    Nothing) -> SetLockTo False
        (2, Just 1,  Nothing,    Nothing,    Nothing) -> SetLockTo True
        (3, Just 0,  Nothing,    Nothing,    Nothing) -> SetBumplockTo False
        (3, Just 1,  Nothing,    Nothing,    Nothing) -> SetBumplockTo True
        (4, Nothing, Just hash_, Just size_, _      ) -> DidUnlinkFile hash_ size_ mFileMime
        (5, Nothing, Just hash_, Just size_, _      ) -> DidPurgeFile hash_ size_ mFileMime
        (6, Nothing, Nothing,    Nothing,    Nothing) -> DidDeletePost
        (7, Nothing, Nothing,    Nothing,    Nothing) -> DidDeleteThread
        (8, Nothing, Nothing,    Nothing,    Nothing) -> DidDeleteBoard
        _ -> error "[Phi.Database.Models:toLog] could not parse row in table log"
