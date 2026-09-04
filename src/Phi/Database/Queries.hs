{-# LANGUAGE OverloadedStrings #-}

module Phi.Database.Queries where

import           Crypto.Hash (hashInitWith, hashUpdates, hashFinalize, SHA3_256(..))
import qualified Data.ByteArray as BA (pack, unpack)
import qualified Data.ByteString as BS (take, pack, unpack)
import           Data.ByteString.Base64 (encodeBase64)
import qualified Data.Text as T (isPrefixOf, null)
import           Data.Text.Encoding (encodeUtf8)

import           Data.Maybe (catMaybes, fromJust, isJust, isNothing, maybeToList)
import           Data.List (nub)

import           Data.Text (Text)
import           Data.Password.Argon2 (checkPassword, mkPassword, PasswordCheck(..))

import           Data.Pool (withResource, Pool)
import qualified Database.SQLite.Simple as DB

import qualified Web.Fn as Fn (File(..))

import           Phi.Context (Context(..))
import           Phi.Database.Models
import           Phi.Database.Queries.Now
import           Phi.Database.Queries.Types
import           Phi.Files (prepareBanner, prepareForPost)

type Receipt failure success = IO (Maybe (Either failure success))
type ReceiptAssured success = IO (Maybe success)

-- Constants

nodelete :: [Either Banner File]
nodelete = []

nofiles :: [File]
nofiles = []

nobanners :: [Banner]
nobanners = []

threadsPerPage :: Int
threadsPerPage = 8

repliesPerHull :: Int
repliesPerHull = 4

-- Helper functions

tryTransaction :: Context -> (DB.Connection -> IO a) -> IO (Maybe a)
tryTransaction context mkAction =
  withResource (db context) $ \conn ->
    tryTransactionNow context conn $ do
      result <- mkAction conn
      pure (result, nodelete)

tryTransactionWithFileDeletion :: Context -> (DB.Connection -> IO (a, [File])) -> IO (Maybe a)
tryTransactionWithFileDeletion context mkAction =
  withResource (db context) $ \conn ->
    tryTransactionNow context conn $ do
      (a, files) <- mkAction conn
      pure (a, map Right files)

tryTransactionWithBannerDeletion :: Context -> (DB.Connection -> IO (a, [Banner])) -> IO (Maybe a)
tryTransactionWithBannerDeletion context mkAction =
  withResource (db context) $ \conn ->
    tryTransactionNow context conn $ do
      (a, banners) <- mkAction conn
      pure (a, map Left banners)

makeHull :: DB.Connection -> OP -> IO (OP, [Reply])
makeHull conn op@(thread, (_post, _mFile, _quotes)) = do
  replies <- getRepliesNow conn thread (Just repliesPerHull)
  pure (op, replies)

ceilingDiv :: Integral a => a -> a -> a
ceilingDiv n m =
  let (q, r) = divMod n m
  in if r == 0 then q else q + 1

mkTripcode :: Context -> Text -> Text
mkTripcode context hashtext =
  encodeBase64 . BS.take 6 . BS.pack . BA.unpack $ digest
  where
    initialHashContext = hashInitWith SHA3_256
    hashContext = hashUpdates initialHashContext [secret context, BA.pack . BS.unpack . encodeUtf8 $ hashtext]
    digest = hashFinalize hashContext

makeCodes :: Context -> DB.Connection -> Maybe Text -> Board -> NewPost -> IO (Maybe Text, Maybe Text)
makeCodes context conn mUsername board newpost
  | T.null hashtext                    = pure (Nothing, Nothing)
  | not ("##" `T.isPrefixOf` hashtext) = pure (Just tripcode_, Nothing)
  | otherwise = do
    powerlevel <-
      case mUsername of
        Nothing        -> pure Commoner
        Just username_ -> do
          mUser <- getUserNow conn username_
          case mUser of
            Nothing   -> pure Commoner
            Just user -> getPowerlevelNow conn board user
    case powerlevel of
      Commoner     -> pure (Just tripcode_, Nothing)
      BoardMod     -> pure (Nothing, Just "Mod")
      BoardManager -> pure (Nothing, Just "Mod")
      BoardOwner   -> pure (Nothing, Just "Board Owner")
      Admin        -> pure (Nothing, Just "Admin")
  where
    hashtext = npHashtext newpost
    tripcode_ = mkTripcode context hashtext

-- Actual database queries

getPageDetails :: Context -> Maybe Board -> ReceiptAssured PageDetails
getPageDetails context mBoard =
  tryTransaction context $ \conn ->
    getPageDetailsNow context conn mBoard

getBoards :: Context -> ReceiptAssured [Board]
getBoards context =
  tryTransaction context getBoardsNow

getBoard :: Context -> Text -> Receipt NoSuchBoard Board
getBoard context uri_ =
  tryTransaction context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing    -> pure $ Left NoSuchBoard
      Just board -> pure $ Right board

getRecent :: Context -> Int -> (Bool, [Text]) -> ReceiptAssured ([(Maybe Thread, FPost)], [(Board, Bool)])
getRecent context limit uriFilter@(whitelist, uris) =
  tryTransaction context $ \conn -> do
    posts <- getRecentPostsNow conn limit uriFilter
    boards <- getBoardsNow conn
    pure (posts, mkBoardFilter conn boards)
    where
      mkBoardFilter :: DB.Connection -> [Board] -> [(Board, Bool)]
      mkBoardFilter conn boards = do
        board <- boards
        let bool = if uri board `elem` uris then whitelist else not whitelist
        pure (board, bool)

getRecentHavingFiles :: Context -> ReceiptAssured [Post]
getRecentHavingFiles context =
  tryTransaction context getRecentPostsHavingFilesNow

getRandomBanner :: Context -> Text -> Receipt NoSuchBoard (Maybe Banner)
getRandomBanner context uri_ =
  tryTransaction context $ \conn -> do
    mBanner <- getRandomBannerNow conn uri_
    case mBanner of
      Just banner -> pure $ Right (Just banner)
      Nothing -> do
        mBoard <- getBoardNow conn uri_
        case mBoard of
          Nothing     -> pure $ Left NoSuchBoard
          Just _board -> pure $ Right Nothing

getImplicit :: Context -> Text -> Receipt NoSuchBoard (Either (Board, [Hull], Int) (Board, [OP]))
getImplicit context uri_ = do
  tryTransaction context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing -> pure $ Left NoSuchBoard
      Just board
        | indexViewPolicy board == IndexViewPreferred -> do
          (ops, nThreads) <- getNonfullOpsNow conn board threadsPerPage 0
          hulls <- mapM (makeHull conn) ops
          let nPages = ceilingDiv nThreads threadsPerPage
          pure $ Right $ Left (board, hulls, nPages)
        | otherwise -> do
          ops <- getAllOpsNow conn board
          pure $ Right $ Right (board, ops)

getCatalogue :: Context -> Text -> Receipt NoSuchBoard (Board, [OP])
getCatalogue context uri_ = do
  tryTransaction context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing    -> pure $ Left NoSuchBoard
      Just board -> do
        ops <- getAllOpsNow conn board
        pure $ Right (board, ops)

getIndex :: Context -> Text -> Int -> Receipt NoSuchBoard (Either ViewDisabled (Board, [Hull], Int))
getIndex context uri_ page = do
  tryTransaction context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing    -> pure $ Left NoSuchBoard
      Just board
        | indexViewPolicy board == IndexViewDisallowed ->
          pure $ Right $ Left ViewDisabled
        | otherwise -> do
          (ops, nThreads) <- getNonfullOpsNow conn board threadsPerPage (threadsPerPage * page)
          hulls <- mapM (makeHull conn) ops
          let nPages = ceilingDiv nThreads threadsPerPage
          pure $ Right $ Right (board, hulls, nPages)

getThreadPosts :: Context -> Text -> Int -> Receipt (Either NoSuchBoard NoSuchThread) (Board, OP, [Reply])
getThreadPosts context uri_ no_ = do
  tryTransaction context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing    -> pure $ Left (Left NoSuchBoard)
      Just board -> do
        eOp <- getOpNow conn board no_
        case eOp of
          Left threadFate -> pure $ Left (Right threadFate)
          Right op@(thread, _fpost) -> do
            replies <- getRepliesNow conn thread Nothing
            pure $ Right (board, op, replies)

getUserDuringLogin :: Context -> Text -> Text -> Receipt LoginFail User
getUserDuringLogin context username_ password = do
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left NoSuchUser
      Just user ->
        case checkPassword (mkPassword password) (pwhash user) of
          PasswordCheckFail    -> pure $ Left WrongPassword
          PasswordCheckSuccess -> do
            updateUserLastActiveNow conn user
            pure $ Right user

getUser :: Context -> Text -> ReceiptAssured (Maybe User)
getUser context username_ =
  tryTransaction context $ \conn -> do
    getUserNow conn username_

getGlobalSettings :: Context -> ReceiptAssured GlobalSettings
getGlobalSettings context =
  tryTransaction context getGlobalSettingsNow

getLogs :: Context -> ReceiptAssured [Log]
getLogs context =
  tryTransaction context getLogsNow

makeOp :: Context -> Maybe Text -> Text -> NewPost -> Fn.File -> Receipt (Either NoSuchBoard (Either PermissionFail FileRejected)) Post
makeOp context mUsername uri_ newpost fnFile =
  tryTransaction context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing    -> pure $ Left $ Left NoSuchBoard
      Just board ->
        case permission board of
          AnyThreadsAnyReplies -> continue conn board
          ModThreadsAnyReplies -> do
            case mUsername of
              Nothing       -> pure $ Left $ Right $ Left PermissionFail
              Just username -> do
                mUser <- getUserNow conn username
                case mUser of
                  Nothing   -> pure $ Left $ Right $ Left PermissionFail
                  Just user -> do
                    powerlevel <- getPowerlevelNow conn board user
                    if powerlevel < BoardMod
                    then pure $ Left $ Right $ Left PermissionFail
                    else continue conn board
          NilThreadsAnyReplies -> pure $ Left $ Right $ Left PermissionFail
          NilThreadsNilReplies -> pure $ Left $ Right $ Left PermissionFail
  where
    continue conn board = do
      -- Evaluate tripcode and capcode.
      (tripcode_, capcode_) <- makeCodes context conn mUsername board newpost

      -- Deal with the uploaded file if it exists.
      eFile <- prepareForPost context fnFile
      let
        emFile =
          case eFile of
            Left FileMissing  -> Right Nothing
            Left fileRejected -> Left fileRejected
            Right file        -> Right $ Just file
      case emFile of
        Left fileRejected -> pure $ Left $ Right $ Right fileRejected
        Right mFile -> do
          post <- insertThreadPostNow conn board newpost tripcode_ capcode_ mFile
          insertThreadNow conn board post
          pure $ Right post

makeReply :: Context -> Maybe Text -> Text -> Int -> NewPost -> Fn.File -> Receipt (Either NoSuchBoard (Either NoSuchThread (Either (Either PermissionFail ReplyFail) FileRejected))) Post
makeReply context mUsername uri_ no_ newpost fnFile =
  tryTransactionWithFileDeletion context $ \conn -> do
    mBoard <- getBoardNow conn uri_
    case mBoard of
      Nothing    -> pure (Left (Left NoSuchBoard), nofiles)
      Just board ->
        case permission board of
          AnyThreadsAnyReplies -> continue conn board
          ModThreadsAnyReplies -> continue conn board
          NilThreadsAnyReplies -> continue conn board
          NilThreadsNilReplies -> pure (Left $ Right $ Right $ Left $ Left PermissionFail, nofiles)
  where
    continue conn board = do
      eThread <- getThreadNow conn board no_
      case eThread of
        Left threadFate -> pure (Left $ Right $ Left threadFate, nofiles)
        Right thread    -> do
          case lock thread of
            Locked           -> pure (Left $ Right $ Right $ Left $ Right LockedThread, nofiles)
            LockedCyclic     -> pure (Left $ Right $ Right $ Left $ Right LockedThread, nofiles)
            LockedBumplocked -> pure (Left $ Right $ Right $ Left $ Right LockedThread, nofiles)
            Full             -> pure (Left $ Right $ Right $ Left $ Right FullThread, nofiles)
            _ -> do
              -- Evaluate tripcode and capcode.
              (tripcode_, capcode_) <- makeCodes context conn mUsername board newpost

              -- Deal with the uploaded file if it exists.
              eFile <- prepareForPost context fnFile
              let
                emFile =
                  case eFile of
                    Left FileMissing  -> Right Nothing
                    Left fileRejected -> Left fileRejected
                    Right file        -> Right $ Just file
              case emFile of
                Left fileRejected -> pure (Left $ Right $ Right $ Right fileRejected, nofiles)
                Right mFile -> do
                  (post, doomedFiles) <- insertThreadReplyNow conn board thread newpost tripcode_ capcode_ mFile
                  pure (Right post, doomedFiles)

makeUser :: Context -> NewUser -> Receipt RegisterFail ()
makeUser context newuser =
  tryTransaction context $ \conn -> do
    globalsettings <- getGlobalSettingsNow conn
    if not . openRegistration $ globalsettings
    then pure $ Left ClosedRegistration
    else do
      mUser <- getUserNow conn (nuUsername newuser)
      case mUser of
        Just _user -> pure $ Left ExtantUser
        Nothing    -> do
          insertUserNow conn newuser
          pure $ Right ()

-- These queries check permissions

getUserAndBoards :: Context -> Text -> Receipt NoAuthority (User, [Board], [(Board, Bool)], [Board])
getUserAndBoards context username_ =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left UserNotFound
      Just user -> do
        (ownedBoards, modBoard, otherBoards) <- getUserBoardsNow conn user
        pure $ Right (user, ownedBoards, modBoard, otherBoards)

getBoardAndModTuples :: Context -> Text -> Text -> Receipt (Either NoAuthority NoSuchBoard) (Powerlevel, Board, [(Text, Bool)])
getBoardAndModTuples context username_ uri_ = do
  tryTransaction context $ \conn -> do
   mUser <- getUserNow conn username_
   case mUser of
     Nothing   -> pure $ Left (Left UserNotFound)
     Just user -> do
       mBoard <- getBoardNow conn uri_
       case mBoard of
         Nothing    -> pure $ Left (Right NoSuchBoard)
         Just board -> do
           powerlevel <- getPowerlevelNow conn board user
           if powerlevel < BoardManager
           then pure $ Left (Left Forbidden)
           else do
             modtuples <- getBoardModTuplesNow conn board
             pure $ Right (powerlevel, board, modtuples)

getBanners :: Context -> Text -> Text -> Receipt (Either NoSuchBoard NoAuthority) (Powerlevel, [Banner])
getBanners context username_ uri_ =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left $ Right UserNotFound
      Just user -> do
        mBoard <- getBoardNow conn uri_
        case mBoard of
          Nothing    -> pure $ Left $ Left NoSuchBoard
          Just board -> do
            powerlevel <- getPowerlevelNow conn board user
            if powerlevel < BoardManager
            then pure $ Left $ Right Forbidden
            else do
              banners <- getBannersNow conn uri_
              pure $ Right (powerlevel, banners)

addBanner :: Context -> Text -> Text -> Fn.File -> Receipt (Either NoSuchBoard (Either NoAuthority FileRejected)) ()
addBanner context username_ uri_ fnFile =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left $ Right $ Left UserNotFound
      Just user -> do
        mBoard <- getBoardNow conn uri_
        case mBoard of
          Nothing    -> pure $ Left $ Left NoSuchBoard
          Just board -> do
            powerlevel <- getPowerlevelNow conn board user
            if powerlevel < BoardManager
            then pure $ Left $ Right $ Left $ Forbidden
            else do
              eBanner <- prepareBanner context uri_ fnFile
              case eBanner of
                Left fileRejected -> pure $ Left $ Right $ Right fileRejected
                Right banner      -> do
                  addBannerNow conn uri_ banner
                  pure $ Right ()

deleteBanners :: Context -> Text -> Text -> [Text] -> Receipt (Either NoSuchBoard (Either NoAuthority DeleteBannersFail)) ()
deleteBanners context username_ uri_ hashes =
  tryTransactionWithBannerDeletion context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure (Left $ Right $ Left UserNotFound, nobanners)
      Just user -> do
        mBoard <- getBoardNow conn uri_
        case mBoard of
          Nothing    -> pure (Left $ Left NoSuchBoard, nobanners)
          Just board -> do
            powerlevel <- getPowerlevelNow conn board user
            if powerlevel < BoardManager
            then pure (Left $ Right $ Left Forbidden, nobanners)
            else do
              if null hashes
              then pure (Left $ Right $ Right NoBanners, nobanners)
              else do
                banners <- getTheseBannersNow conn uri_ hashes
                if length banners /= length hashes
                then pure (Left $ Right $ Right NoSuchBanner, nobanners)
                else do
                  deleteBannersNow conn uri_ hashes
                  pure (Right (), banners)

makeBoard :: Context -> Text -> NewBoard -> Receipt (Either NoAuthority ExtantBoard) ()
makeBoard context username_ newboard =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left (Left UserNotFound)
      Just user -> do
        mBoard <- getBoardNow conn (nbUri newboard)
        case mBoard of
          Just _board -> pure $ Left (Right ExtantBoard)
          Nothing     -> do
            insertBoardNow conn user newboard
            pure $ Right ()

deleteBoard :: Context -> Text -> Text -> Text -> Receipt (Either NoSuchBoard (Either NoAuthority DeleteBoardFail)) ()
deleteBoard context username_ uri_ confirmation =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing -> pure $ Left $ Right $ Left UserNotFound
      Just user -> do
        mBoard <- getBoardNow conn uri_
        case mBoard of
          Nothing -> pure $ Left $ Left NoSuchBoard
          Just board -> do
            powerlevel <- getPowerlevelNow conn board user
            if powerlevel < BoardOwner
            then pure $ Left $ Right $ Left Forbidden
            else
              if confirmation /= "DELETE"
              then pure $ Left $ Right $ Right InvalidConfirmation
              else do
                -- Remove all quote references first.
                DB.executeNamed conn
                  "DELETE FROM quote WHERE board_uri = :board_uri"
                  [":board_uri" DB.:= uri_]

                -- Delete replies before threads because replies reference threads.
                DB.executeNamed conn
                  "DELETE FROM post WHERE board_uri = :board_uri AND thread_no IS NOT NULL"
                  [":board_uri" DB.:= uri_]

                -- Delete threads before OP posts because threads reference their OP posts.
                DB.executeNamed conn
                  "DELETE FROM thread WHERE board_uri = :board_uri"
                  [":board_uri" DB.:= uri_]

                -- Delete the OP posts.
                DB.executeNamed conn
                  "DELETE FROM post WHERE board_uri = :board_uri"
                  [":board_uri" DB.:= uri_]

                -- Remove banners and moderators.
                DB.executeNamed conn
                  "DELETE FROM banner WHERE board_uri = :board_uri"
                  [":board_uri" DB.:= uri_]

                DB.executeNamed conn
                  "DELETE FROM board_mod WHERE board_uri = :board_uri"
                  [":board_uri" DB.:= uri_]

                -- Finally remove the board itself.
                DB.executeNamed conn
                  "DELETE FROM board WHERE uri = :uri"
                  [":uri" DB.:= uri_]

                pure $ Right ()

setBoardSettings :: Context -> Text -> Text -> BoardSettings -> Receipt (Either NoAuthority (Either NoSuchBoard AddModFail)) ()
setBoardSettings context username_ uri_ boardsettings =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left (Left UserNotFound)
      Just user -> do
        mBoard <- getBoardNow conn uri_
        case mBoard of
          Nothing    -> pure $ Left (Right (Left NoSuchBoard))
          Just board -> do
            powerlevel <- getPowerlevelNow conn board user
            if powerlevel < BoardManager
            then pure $ Left (Left Forbidden)
            else do
              case bsAddMod boardsettings of
                Nothing -> do
                  setBoardSettingsNow conn board boardsettings
                  pure $ Right ()
                Just modname -> do
                  -- Check there actually is a user whose name is the name of
                  -- the mod to be added.
                  -- This doesn't check that the mods to be removed are actually
                  -- mods of the board or that there are actually users with the
                  -- given names.
                  mUser <- getUserNow conn modname
                  case mUser of
                    Nothing   -> pure $ Left (Right (Right NotAUser))
                    Just user -> do
                      isMod <- isJust <$> checkBoardModNow conn board user
                      if isMod
                      then pure $ Left (Right (Right AlreadyAMod))
                      else do
                        setBoardSettingsNow conn board boardsettings
                        pure $ Right ()

setGlobalSettings :: Context -> Text -> GlobalSettings -> Receipt NoAuthority ()
setGlobalSettings context username_ globalsettings =
  tryTransaction context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure $ Left UserNotFound
      Just user -> do
        if not (admin user)
        then pure $ Left Forbidden
        else do
          setGlobalSettingsNow conn globalsettings
          pure $ Right ()

moderate :: Context -> Text -> ModAction -> [(Text, Int)] -> Receipt (Either NoAuthority (Either NoSuchBoard (Either NoSuchThread NoSuchPost))) PerformedActions
moderate context username_ modAction postTuples =
  tryTransactionWithFileDeletion context $ \conn -> do
    mUser <- getUserNow conn username_
    case mUser of
      Nothing   -> pure (Left $ Left UserNotFound, nofiles)
      Just user -> do
        -- Check all given boards exist.
        let uris = nub $ map fst postTuples
        boardsByUri <- catMaybes <$> sequence [((\board -> (uri_, board)) <$>) <$> getBoardNow conn uri_ | uri_ <- uris] :: IO [(Text, Board)]
        if length boardsByUri /= length uris
        then pure (Left $ Right $ Left NoSuchBoard, nofiles)
        else do
          -- Check powerlevels for each board.
          let boards = map snd boardsByUri :: [Board]
          powerlevels <- sequence [getPowerlevelNow conn board user | board <- boards]
          if any (< requisite) powerlevels
          then pure (Left $ Left Forbidden, nofiles)
          else
            if actingOnThreads
            then do
              -- Check threads exist
              threadsExist <- sequence
                [ checkThreadExists conn board no_
                | (uri_, no_) <- postTuples
                , let board = fromJust $ lookup uri_ boardsByUri
                ]
              if any not threadsExist
              then pure (Left $ Right $ Right $ Left FutureThread, nofiles) -- just say FutureThread for now, not necessarilly correct
              else continue conn user
            else do
              -- Check posts exist
              postsExist <- sequence
                [ checkPostExists conn board no_
                | (uri_, no_) <- postTuples
                , let board = fromJust $ lookup uri_ boardsByUri
                ]
              if any not postsExist
              then pure (Left $ Right $ Right $ Right NoSuchPost, nofiles)
              else continue conn user
  where
    continue conn user = do
      case modAction of
        Sticky     reason stickiness_ -> withnofiles <$> modSticky  conn user reason postTuples stickiness_
        Cycle      reason bool        -> withnofiles <$> modLockBit conn user reason postTuples Cyclic     bool
        Lock       reason bool        -> withnofiles <$> modLockBit conn user reason postTuples Locked     bool
        Bumplock   reason bool        -> withnofiles <$> modLockBit conn user reason postTuples Bumplocked bool
        UnlinkFile reason             -> withfiles   <$> modUnlink  conn user reason postTuples
        PurgeFile  reason             -> withfiles   <$> modPurge   conn user reason postTuples
        Delete     reason             -> withfiles   <$> modDelete  conn user reason postTuples

    withnofiles n        = (Right n, [])
    withfiles (n, files) = (Right n, files)

    requisite :: Powerlevel
    requisite =
      case modAction of
        Sticky _ _   -> BoardMod
        Cycle _ _    -> BoardMod
        Lock _ _     -> BoardMod
        Bumplock _ _ -> BoardMod
        UnlinkFile _ -> BoardMod
        PurgeFile _  -> Admin
        Delete _     -> BoardMod

    actingOnThreads =
      case modAction of
        Sticky _ _   -> True
        Cycle _ _    -> True
        Lock _ _     -> True
        Bumplock _ _ -> True
        UnlinkFile _ -> False
        PurgeFile _  -> False
        Delete _     -> False
