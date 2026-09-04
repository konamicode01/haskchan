{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phi.Database.Queries.Now where

import           Prelude hiding (log)

import           Control.Monad (filterM, forM, forM_, when)
import           Data.List (nub, nubBy)
import           Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)

import qualified Data.Text.Lazy as TL (toStrict)
import           Data.Char (ord)
import           Lucid (Html, renderText, toHtmlRaw)

import           Data.Text (Text)
import qualified Data.Text as T (all, null, pack, unpack, replace, toCaseFold)
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Password.Argon2 (hashPassword)

import           Control.Exception.Safe (bracketOnError_, SomeException, try)
import qualified Database.SQLite.Simple as DB
import           Database.SQLite.Simple (NamedParam((:=)), Only(Only), fromOnly)

import           Phi.Context (Context, extractFromCookie)
import           Phi.Files (deleteBanner, deleteFile)
import           Phi.Database.Models
import           Phi.Database.Queries.Types

roundDownUTCTime :: UTCTime -> UTCTime
roundDownUTCTime =
  posixSecondsToUTCTime . fromIntegral . floor . utcTimeToPOSIXSeconds

tryTransactionNow :: forall a. Context -> DB.Connection -> IO (a, [Either Banner File]) -> IO (Maybe a)
tryTransactionNow context conn action = do
  result <- try transaction :: IO (Either SomeException (a, [Either Banner File]))
  case result of
    Right (value, doomeds) -> do
      -- Physically delete files/banners that were removed from the database.
      forM_ doomeds $ \doomed ->
        case doomed of
          Left banner -> deleteBanner context banner
          Right file  -> deleteFile context file
      pure $ Just value
    Left exception -> do
      print exception
      pure Nothing
  where
    transaction =
      bracketOnError_ begin rollback $ do
        result <- action
        commit
        pure result :: IO (a, [Either Banner File])
    begin    = DB.execute_ conn "BEGIN TRANSACTION"
    commit   = DB.execute_ conn "COMMIT TRANSACTION"
    rollback = DB.execute_ conn "ROLLBACK TRANSACTION"

lowalnum :: Char -> Bool
lowalnum char = ord char >= 48 && ord char < 58 || ord char >= 97 && ord char < 123

mkSqlListFromInts :: [Int] -> Text
mkSqlListFromInts ns = "(" <> csv ns <> ")"
  where
    csv [] = ""
    csv [n] = T.pack . show $ n
    csv (n:ns') = (T.pack . show $ n) <> ", " <> csv ns'

mkSqlListFromTexts :: [Text] -> Text
mkSqlListFromTexts texts = "(" <> csv filtered <> ")"
  where
    filtered = filter (T.all lowalnum) texts
    csv [] = ""
    csv [text] = "'" <> text <> "'"
    csv (text:texts') = "'" <> text <> "', " <> csv texts'

replaceQuotelinks :: Board -> [(Maybe Int, Int)] -> [Text] -> [(Text, Maybe Int, Int)] -> Text -> Text
replaceQuotelinks board postquotes boardquotes foreignquotes message_ =
  foldr ($) message_ (map replaceForeignquote foreignquotes ++ map replaceBoardquote boardquotes ++ map replacePostquote postquotes)
  where
    replacePostquote :: (Maybe Int, Int) -> Text -> Text
    replacePostquote postquote@(_mThreadNo, no_) message' =
      T.replace
        ("<a class=\"quote\">&gt;&gt;" <> (T.pack . show $ no_) <> "</a>")
        ("<a class=\"quote\" href=\"" <> postUrl postquote <> "\">&gt;&gt;" <> (T.pack . show $ no_) <> "</a>")
        message'

    replaceBoardquote :: Text -> Text -> Text
    replaceBoardquote uri_ message' =
      T.replace
        ("<a class=\"boardquote\">&gt;&gt;&gt;/" <> uri_ <> "/</a>")
        ("<a class=\"boardquote\" href=\"" <> boardUrl uri_ <> "\">&gt;&gt;&gt;/" <> uri_ <> "/</a>")
        message'

    replaceForeignquote :: (Text, Maybe Int, Int) -> Text -> Text
    replaceForeignquote foreignquote@(uri_, _mThreadNo, no_) message' =
      T.replace
        ("<a class=\"foreignquote\">&gt;&gt;&gt;/" <> uri_ <> "/" <> (T.pack . show $ no_) <> "</a>")
        ("<a class=\"foreignquote\" href=\"" <> foreignPostUrl foreignquote <> "\">&gt;&gt;&gt;/" <> uri_ <> "/" <> (T.pack  . show $ no_) <> "</a>")
        message'

    postUrl :: (Maybe Int, Int) -> Text
    postUrl (mThreadNo, no_) =
      case mThreadNo of
        Nothing       -> "/" <> uri board <> "/thread/" <> (T.pack . show $ no_)      <> "#post" <> (T.pack . show $ no_)
        Just threadNo -> "/" <> uri board <> "/thread/" <> (T.pack . show $ threadNo) <> "#post" <> (T.pack . show $ no_)

    boardUrl :: Text -> Text
    boardUrl uri_ =
      "/" <> uri_ <> "/"

    foreignPostUrl :: (Text , Maybe Int, Int) -> Text
    foreignPostUrl (uri_, mThreadNo, no_) =
      case mThreadNo of
        Nothing       -> "/" <> uri_ <> "/thread/" <> (T.pack . show $ no_)      <> "#post" <> (T.pack . show $ no_)
        Just threadNo -> "/" <> uri_ <> "/thread/" <> (T.pack . show $ threadNo) <> "#post" <> (T.pack . show $ no_)

findAndReplaceQuotelinks :: DB.Connection -> Board -> NewPost -> IO ([Int], Html ())
findAndReplaceQuotelinks conn board newpost = do
  -- Find genuine quoted posts.
  realPostquotes <-
    DB.query conn
      (DB.Query $ "SELECT thread_no, no FROM post WHERE board_uri = ? AND no IN " <> mkSqlListFromInts postquotes)
      [uri board]
      :: IO [(Maybe Int, Int)]

  -- Find genuine quoted boards.
  realBoardquotes <- (fmap fromOnly) <$>
    DB.query_ conn
      (DB.Query $ "SELECT uri FROM board WHERE uri IN " <> mkSqlListFromTexts boardquotes)
      :: IO [Text]

  -- Find genuine quoted foreign-board posts.
  realForeignquotes <-
    DB.query_ conn
      foreignquotesQuery
      :: IO [(Text, Maybe Int, Int)]

  pure (map snd realPostquotes, toHtmlRaw $ replaceQuotelinks board realPostquotes realBoardquotes realForeignquotes (TL.toStrict . renderText $ npMessage newpost))

  where
    (postquotes, boardquotes, foreignquotes) = npQuotes newpost

    foreignquotesQuery :: DB.Query
    foreignquotesQuery = DB.Query $
      "SELECT board_uri, thread_no, no FROM post WHERE " <> mkSqlSum foreignquotes

    mkSqlSum :: [(Text, Int)] -> Text
    mkSqlSum tuples
      | T.null $ unsafeMkSqlSum (filtered tuples) = "FALSE"
      | otherwise = unsafeMkSqlSum (filtered tuples)

    filtered :: [(Text, Int)] -> [(Text, Int)]
    filtered tuples = [(text, n) | (text, n) <- tuples, T.all lowalnum text]

    unsafeMkSqlSum :: [(Text, Int)] -> Text
    unsafeMkSqlSum [] = ""
    unsafeMkSqlSum [(uri_, no_)]        = "(board_uri, no) = ('" <> uri_ <> "', " <> (T.pack . show $ no_) <> ")"
    unsafeMkSqlSum ((uri_, no_):tuples) = "(board_uri, no) = ('" <> uri_ <> "', " <> (T.pack . show $ no_) <> ") OR " <> unsafeMkSqlSum tuples

getInsertedPostNow :: DB.Connection -> IO Post
getInsertedPostNow conn = do
  rowid <- DB.lastInsertRowId conn
  posts <- DB.query conn "SELECT * FROM post WHERE ROWID = ?" [rowid]
  pure $ case posts of
    [post] -> post
    _      -> error "[Phi.Database.Queries.Now:getInsertedPostNow] database is corrupt: couldn't select last inserted row in table post"

getPageDetailsNow :: Context -> DB.Connection -> Maybe Board -> IO PageDetails
getPageDetailsNow context conn mBoard = do
  -- Topnav
  topnav <- getBoardsNow conn

  -- Cookie settings
  let
    cookieTheme_ = case extractFromCookie context "theme" of
      Just "phichannel" -> Just Phichannel
      Just "nanochan"   -> Just Nanochan
      Just "yotsuba"    -> Just Yotsuba
      Just "haskchan"   -> Just Haskchan
      _                 -> Nothing
    cookiesettings = CookieSettings { cookieTheme = cookieTheme_ }

  -- Global settings
  globalsettings <- getGlobalSettingsNow conn

  -- Page theme
  pageTheme <-
    case cookieTheme_ of
      Just theme -> pure theme
      Nothing    ->
        case mBoard of
          Nothing    -> pure $ globalTheme globalsettings
          Just board ->
            case mTheme board of
              Just theme -> pure theme
              Nothing    -> pure $ globalTheme globalsettings
  -- TODO: let boards enable the captcha for themselves regardless of the
  --       global setting (and then include that here)

  pure $ PageDetails
    { pdTopnav = topnav
    , pdTheme = pageTheme
    , pdCookieSettings = cookiesettings
    , pdGlobalSettings = globalsettings
    }

getBoardsNow :: DB.Connection -> IO [Board]
getBoardsNow conn =
  DB.query_ conn
    "SELECT * FROM board ORDER BY total_posts DESC"

getBoardNow :: DB.Connection -> Text -> IO (Maybe Board)
getBoardNow conn uri_ = do
  boards <- DB.query conn
    "SELECT * FROM board WHERE uri = ?"
    [uri_]
  pure $ case boards of
      []      -> Nothing
      [board] -> Just board
      _       -> error "[Phi.Database.Queries.Now:getBoardNow] database is corrupt: non-unique uri in table board"

getBoardOrErrorNow :: String -> DB.Connection -> Text -> IO Board
getBoardOrErrorNow caller conn uri_ = do
  mBoard <- getBoardNow conn uri_
  case mBoard of
    Just board -> pure board
    Nothing -> error $ "[Phi.Database.Queries.Now:" <> caller <> "->getBoardOrErrorNow] database is corrupt: board with uri " <> T.unpack uri_ <> "was not found"

getRecentPostsNow :: DB.Connection -> Int -> (Bool, [Text]) -> IO [(Maybe Thread, FPost)]
getRecentPostsNow conn limit (whitelist, uris) = do
  posts <-
    case (whitelist, uris) of
      (True,  []) -> pure []
      (False, []) -> DB.query_ conn $ DB.Query $ "SELECT * FROM post ORDER BY datetime DESC, no DESC LIMIT " <> (T.pack . show $ limit)
      (True,  _ ) -> DB.query_ conn $ DB.Query $ "SELECT * FROM post WHERE board_uri IN " <> mkSqlListFromTexts uris <> "ORDER BY datetime DESC, no DESC LIMIT " <> (T.pack . show $ limit)
      (False, _ ) -> DB.query_ conn $ DB.Query $ "SELECT * FROM post WHERE board_uri NOT IN " <> mkSqlListFromTexts uris <> "ORDER BY datetime DESC, no DESC LIMIT " <> (T.pack . show $ limit)
  let
    threadActionKeyValue :: Post -> ((Text, Int), IO Thread)
    threadActionKeyValue post = ((pBoardUri post, no post), getThreadFromPostNow conn post)

    -- For all posts that constitute threads...
    threadPosts :: [Post]
    threadPosts = filter (\post -> isNothing $ pThreadNo post) posts

    -- ...create a mapping from a post's (uri, no) pair to an IO action returning its thread...
    threadActions :: [((Text, Int), IO Thread)]
    threadActions = map threadActionKeyValue threadPosts

    -- ...without duplicate keys...
    threadActionMap :: [((Text, Int), IO Thread)]
    threadActionMap = nubBy (\(key1, _) (key2, _) -> key1 == key2) threadActions

    -- ...and transform that mapping into a mapping from a post's (uri, no) pair to its actual thread.
    makeThreadMap :: IO [((Text, Int), Thread)]
    makeThreadMap = forM threadActionMap $ \(key, getThread) -> getThread >>= \thread -> pure (key, thread)

  -- (This saves redundant calls to getThreadFromPostNow, which makes a database query.)
  threadMap <- makeThreadMap

  forM posts $ \post -> do
    mFile <- getPostFileNow conn post
    quotes <- getQuotesNow conn post
    mThread <-
      case pThreadNo post of
        Just _no -> pure Nothing
        Nothing  -> pure $ lookup (pBoardUri post, no post) threadMap
    pure (mThread, (post, mFile, quotes))

  where
    threadNo post = fromMaybe (no post) (pThreadNo post)

getRecentPostsHavingFilesNow :: DB.Connection -> IO [Post]
getRecentPostsHavingFilesNow conn = do
  filetuples <- take 8 <$>
    nubBy (\t1@(filehash1, _, _) t2@(filehash2, _, _) -> filehash1 == filehash2) <$>
     DB.query_ conn
      "SELECT file_hash, thumb_width, thumb_height FROM post JOIN file \
      \ ON file_hash = hash AND thumb_width NOTNULL AND thumb_height NOTNULL \
      \ ORDER BY datetime DESC, no DESC"
    :: IO [(FileHash, Int, Int)]
  reverse <$> getPostsWithinHeight 400 filetuples
  where
    margin = 6
    minHeight = 42
    maxHeight = 112
    maxWidth = 112
    getPostsWithinHeight = aux []

    shrinkHeight :: Int -> Int -> Int
    shrinkHeight width height
      | width <= maxWidth = height
      | otherwise = round $ fromIntegral (height * maxWidth) / fromIntegral width

    aux :: [Post] -> Int -> [(FileHash, Int, Int)] -> IO [Post]
    aux posts height [] = pure posts
    aux posts height (filetuple@(filehash, thumbWidth, thumbHeight) : filetuples')
      | height <= 0 = pure posts
      | otherwise = do
        [post] <- DB.query conn
          "SELECT * FROM post WHERE file_hash = ? ORDER BY datetime DESC, no DESC LIMIT 1"
          [filehash]
        let effectiveThumbHeight = constrain minHeight maxHeight (shrinkHeight thumbWidth thumbHeight) + margin
            height' = height - effectiveThumbHeight
        if abs height' > abs height
        then pure posts
        else do
          let posts' = post : posts
          aux posts' height' filetuples'

    constrain :: Ord a => a -> a -> a -> a
    constrain min_ max_ value
      | value < min_ = min_
      | value > max_ = max_
      | otherwise    = value

getRandomBannerNow :: DB.Connection -> Text -> IO (Maybe Banner)
getRandomBannerNow conn uri_ = do
  banners <- DB.query conn
    "SELECT * FROM banner WHERE board_uri = ? ORDER BY RANDOM() LIMIT 1"
    [uri_]
  case banners of
    []       -> pure Nothing
    [banner] -> pure $ Just banner
    _        -> error "[Phi.Database.Queries.Now:getRandomBannerNow] database is corrupt: query selecting at most a single row returned multiple rows"

getBannersNow :: DB.Connection -> Text -> IO [Banner]
getBannersNow conn uri_ =
  DB.query conn
    "SELECT * FROM banner WHERE board_uri = ?"
    [uri_]

getTheseBannersNow :: DB.Connection -> Text -> [Text] -> IO [Banner]
getTheseBannersNow conn uri_ hashes = do
  DB.query conn
    (DB.Query $ "SELECT * FROM banner WHERE board_uri = ? AND hash IN " <> mkSqlListFromTexts hashes)
    [uri_]

addBannerNow :: DB.Connection -> Text -> Banner -> IO ()
addBannerNow conn uri_ banner =
  DB.executeNamed conn
    "INSERT INTO banner (board_uri, hash, ext)               \
    \ VALUES (:board_uri, :hash, :ext)                       \
    \ ON CONFLICT (board_uri, hash) DO UPDATE                \
    \ SET (board_uri, hash, ext) = (:board_uri, :hash, :ext) "
    [ ":board_uri" := uri_
    , ":hash"      := bnHash banner
    , ":ext"       := bnExt banner
    ]

deleteBannersNow :: DB.Connection -> Text -> [Text] -> IO ()
deleteBannersNow conn uri_ hashes =
  DB.execute conn
    (DB.Query $ "DELETE FROM banner WHERE board_uri = ? AND hash IN " <> mkSqlListFromTexts hashes)
    [uri_]

checkPostExists :: DB.Connection -> Board -> Int -> IO Bool
checkPostExists conn board no_ = do
  [Only n] <- DB.queryNamed conn
    "SELECT COUNT(*) FROM post WHERE (board_uri, no) = (:board_uri, :no)"
    [ ":board_uri" := uri board
    , ":no"        := no_
    ]
    :: IO [Only Int]
  pure $ n >= 1

checkThreadExists :: DB.Connection -> Board -> Int -> IO Bool
checkThreadExists conn board no_ = do
  [Only n] <- DB.queryNamed conn
    "SELECT COUNT(*) FROM thread WHERE (board_uri, post_no) = (:board_uri, :post_no)"
    [ ":board_uri" := uri board
    , ":post_no"   := no_
    ]
    :: IO [Only Int]
  pure $ n >= 1

getThreadNow :: DB.Connection -> Board -> Int -> IO (Either NoSuchThread Thread)
getThreadNow conn board no_ = do
  threads <- DB.queryNamed conn
    "SELECT * FROM thread WHERE (board_uri, post_no) = (:board_uri, :post_no)"
    [ ":board_uri" := uri board
    , ":post_no"   := no_
    ]
  pure $ case threads of
    []       -> Left  threadFate
    [thread] -> Right thread
    _        -> error "[Phi.Database.Queries.Now:getThreadNow] database is corrupt: non-unique combination of board_uri and post_no in table thread"
   where
     threadFate
       | no_ <= totalPosts board = DeletedThread
       | otherwise               = FutureThread

getThreadNow' :: DB.Connection -> Text -> Int -> IO (Maybe Thread)
getThreadNow' conn uri_ no_ = do
  threads <- DB.queryNamed conn
    "SELECT * FROM thread WHERE (board_uri, post_no) = (:board_uri, :post_no)"
    [ ":board_uri" := uri_
    , ":post_no"   := no_
    ]
  pure $ case threads of
    []       -> Nothing
    [thread] -> Just thread
    _        -> error "[Phi.Database.Queries.Now:getThreadNow'] database is corrupt: non-unique combination of board_uri and post_no in table thread"

getThreadFromPostNow :: DB.Connection -> Post -> IO Thread
getThreadFromPostNow conn post = do
  mThread <-
    case pThreadNo post of
      Nothing       -> getThreadNow' conn (pBoardUri post) (no post)
      Just threadNo -> getThreadNow' conn (pBoardUri post) threadNo
  case mThread of
    Nothing     -> error "[Phi.Database.Queries.Now:getThreadFromPostNow] database is corrupt: no corresponding row in table thread for row in table post"
    Just thread -> pure thread

getFPostFromThreadNow :: DB.Connection -> Thread -> IO FPost
getFPostFromThreadNow conn thread = do
  mPost <- getPostNow conn (tBoardUri thread) (tPostNo thread)
  case mPost of
    Nothing   -> error "[Phi.Database.Queries.Now:getPostFromThreadNow] database is corrupt: no corresponding row in table post for foreign key post_no in table thread"
    Just post -> do
      mFile <- getPostFileNow conn post
      quotes <- getQuotesNow conn post
      pure (post, mFile, quotes)

getPostNow :: DB.Connection -> Text -> Int -> IO (Maybe Post)
getPostNow conn uri_ no_ = do
  posts <- DB.queryNamed conn
    "SELECT * FROM post WHERE (board_uri, no) = (:board_uri, :no)"
     [ ":board_uri" := uri_
     , ":no"        := no_
     ]
  pure $ case posts of
    []     -> Nothing
    [post] -> Just post
    _      -> error "[Phi.Database.Queries.Now:getPostNow] database is corrupt: non-unique combination of board_uri and no in table post"

getPostOrErrorNow :: String -> DB.Connection -> Board -> Int -> IO Post
getPostOrErrorNow caller conn board no_ = do
  mPost <- getPostNow conn (uri board) no_
  case mPost of
    Just post -> pure post
    Nothing   -> error $ "[Phi.Database.Queries.Now:" <> caller <> "->getPostOrErrorNow] database is corrupt: post with board_uri " <> (T.unpack $ uri board) <> " and no " <> show no_ <> " was not found"

getPostOrErrorNow' :: String -> DB.Connection -> Text -> Int -> IO Post
getPostOrErrorNow' caller conn uri_ no_ = do
  mPost <- getPostNow conn uri_ no_
  case mPost of
    Just post -> pure post
    Nothing   -> error $ "[Phi.Database.Queries.Now:" <> caller <> "->getPostOrErrorNow'] database is corrupt: post with board_uri " <> T.unpack uri_ <> " and no " <> show no_ <> " was not found"

getThreadOrErrorNow' :: String -> DB.Connection -> Text -> Int -> IO Thread
getThreadOrErrorNow' caller conn uri_ no_ = do
  mThread <- getThreadNow' conn uri_ no_
  case mThread of
    Just thread -> pure thread
    Nothing     -> error $ "[Phi.Database.Queries.Now:" <> caller <> "->getThreadOrErrorNow'] database is corrupt: thread with board_uri " <> T.unpack uri_ <> " and no " <> show no_ <> " was not found"

getNonfullOpsNow :: DB.Connection -> Board -> Int -> Int -> IO ([OP], Int)
getNonfullOpsNow conn board limit offset = do
  threads <- DB.queryNamed conn
    query
    [ ":board_uri" := uri board
    , ":lock"      := Full
    ]
  ops <- mapM makeOp threads
  ns <- DB.query conn
    "SELECT COUNT(*) FROM thread WHERE board_uri = ?"
    [uri board]
    :: IO [Only Int]
  case ns of
    [Only nThreads] -> pure (ops, nThreads)
    _               -> error "[Phi.Database.Queries.Now:getNonfullOpsNow] database is corrupt: query counting the rows of table thread didn't return exactly 1 row"
  where
    query =
      DB.Query $
        "SELECT * FROM thread WHERE board_uri = :board_uri AND (lock != :lock OR stickiness > 0)"
        <> " ORDER BY stickiness DESC, bumped DESC, post_no DESC"
        <> " LIMIT " <> (T.pack . show $ limit)
        <> " OFFSET " <> (T.pack . show $ offset)
    makeOp thread = do
      mPost <- getPostNow conn (uri board) (tPostNo thread)
      case mPost of
        Just post -> do
          mFile <- getPostFileNow conn post
          quotes <- getQuotesNow conn post
          pure (thread, (post, mFile, quotes))
        Nothing   -> error "[Phi.Database.Queries.Now:getOpsNow] database is corrupt: no corresponding row in table post for foreign key (board_uri, post_no) in table thread"

getAllOpsNow :: DB.Connection -> Board -> IO [OP]
getAllOpsNow conn board = do
  threads <- DB.query conn
    "SELECT * FROM thread WHERE board_uri = ? ORDER BY stickiness DESC, bumped DESC, post_no DESC"
    [uri board]
  mapM makeOp threads
  where
    makeOp thread = do
      mPost <- getPostNow conn (uri board) (tPostNo thread)
      case mPost of
        Just post -> do
          mFile <- getPostFileNow conn post
          quotes <- getQuotesNow conn post
          pure (thread, (post, mFile, quotes))
        Nothing   -> error "[Phi.Database.Queries.Now:getAllOpsNow] database is corrupt: no corresponding row in table post for foreign key (board_uri, post_no) in table thread"

getOpNow :: DB.Connection -> Board -> Int -> IO (Either NoSuchThread OP)
getOpNow conn board no_ = do
  eThread <- getThreadNow conn board no_
  case eThread of
    Left threadFate -> pure $ Left threadFate
    Right thread    -> do
      mPost <- getPostNow conn (uri board) no_
      case mPost of
        Just post -> do
          mFile <- getPostFileNow conn post
          quotes <- getQuotesNow conn post
          pure $ Right (thread, (post, mFile, quotes))
        Nothing   -> error "[Phi.Database.Queries.Now:getOpNow] database is corrupt: no corresponding row in table post for foreign key (board_uri, post_no) in table thread"

getRepliesNow :: DB.Connection -> Thread -> Maybe Int -> IO [Reply]
getRepliesNow conn thread mLimit = do
  posts <- DB.queryNamed conn
    query
    [ ":board_uri" := tBoardUri thread
    , ":thread_no" := tPostNo   thread
    ]
  replies <- mapM makeReply posts
  pure $ sort replies
  where
    (sort, query) =
      case mLimit of
        Nothing    -> (id,      DB.Query baseQuery)
        Just limit -> (reverse, DB.Query $ baseQuery <> " DESC LIMIT " <> (T.pack . show $ limit))
    baseQuery =
      "SELECT * FROM post WHERE (board_uri, thread_no) = (:board_uri, :thread_no) ORDER BY no"
    makeReply post = do
      mFile <- getPostFileNow conn post
      quotes <- getQuotesNow conn post
      pure (post, mFile, quotes)

getPostFileNow :: DB.Connection -> Post -> IO (Maybe File)
getPostFileNow conn post =
  case fileHash post of
    Nothing    -> pure Nothing
    Just hash_ -> do
      mFile <- getFileNow conn hash_
      case mFile of
        Just file -> pure $ Just file
        Nothing   -> error "[Phi.Database.Queries.Now:getPostFileNow] database is corrupt: no corresponding row in table file for foreign key file_hash in table post"

getQuotesNow :: DB.Connection -> Post -> IO [Quote]
getQuotesNow conn post =
  DB.queryNamed conn
    "SELECT * FROM quote WHERE (board_uri, parent_no) = (:board_uri, :parent_no) LIMIT 32"
    [ ":board_uri" := pBoardUri post
    , ":parent_no" := no post
    ]

getFileNow :: DB.Connection -> Text -> IO (Maybe File)
getFileNow conn hash_ = do
  files <- DB.query conn
    "SELECT * FROM file WHERE hash = ?"
    [hash_]
  pure $ case files of
    []     -> Nothing
    [file] -> Just file
    _      -> error "[Phi.Database.Queries.Now:getFileNow] database is corrupt: non-unique hash in table file"

getFileOrErrorNow :: String -> DB.Connection -> Text -> IO File
getFileOrErrorNow caller conn hash_ = do
  mFile <- getFileNow conn hash_
  case mFile of
    Just file -> pure file
    Nothing   -> error $ "[Phi.Database.Queries.Now:" <> caller <> "->getFileOrErrorNow] database is corrupt: file with hash " <> T.unpack hash_ <> " was not found"

getUserNow :: DB.Connection -> Text -> IO (Maybe User)
getUserNow conn username_ = do
  users <- DB.query conn
    "SELECT * FROM user WHERE username = ?"
    [username_]
  pure $ case users of
    []     -> Nothing
    [user] -> Just user
    _      -> error "[Phi.Database.Queries.Now:getUser] database is corrupt: non-unique username in table user"

getUserBoardsNow :: DB.Connection -> User -> IO ([Board], [(Board, Bool)], [Board])
getUserBoardsNow conn user = do
    -- Boards owned by this user.
    ownedBoards <-
      DB.query conn
        "SELECT * FROM board WHERE owner_name = ?"
        [username user]

    -- A list of (uri, isManager) pairs decribing boards modded by this user.
    modBoards' <-
      DB.query conn
        "SELECT board_uri, manager FROM board_mod WHERE username = ?"
        [username user]
        :: IO [(Text, Bool)]

    -- Boards modded by this user.
    modBoards <- sequence $ do
      (uri_, isManager) <- modBoards'
      let getBoard = getBoardOrErrorNow "getUserBoardsNow" conn uri_
          getTuple = getBoard >>= \board -> pure (board, isManager)
      pure getTuple

    -- If the user is an admin, all the other boards.
    otherBoards <-
      if not (admin user)
      then pure []
      else do
        let uris = nub $ map fst modBoards' ++ map uri ownedBoards
        DB.query_ conn $
          DB.Query $ "SELECT * FROM board WHERE uri NOT IN " <> mkSqlListFromTexts uris

    pure (ownedBoards, modBoards, otherBoards)

updateUserLastActiveNow :: DB.Connection -> User -> IO ()
updateUserLastActiveNow conn user = do
  time <- roundDownUTCTime <$> getCurrentTime
  DB.executeNamed conn
    "UPDATE user SET last_active = :last_active WHERE username = :username"
    [ ":last_active" := time
    , ":username"    := username user
    ]

getGlobalSettingsNow :: DB.Connection -> IO GlobalSettings
getGlobalSettingsNow conn = do
  gss <- DB.query_ conn
    "SELECT * FROM global_settings WHERE ROWID = 1"
  case gss of
    [globalsettings] -> pure globalsettings
    _ -> error "[Phi.Database.Queries.Now:getGlobalSettingsNow] database is corrupt: non-one number of rows in table global_settings"

insertBoardNow :: DB.Connection -> User -> NewBoard -> IO ()
insertBoardNow conn user newboard =
  DB.executeNamed conn
    "INSERT INTO board (uri, title, description, permission, index_view_policy, total_posts, owner_name) \
    \ VALUES (:uri, :title, :description, :permission, :index_view_policy, :total_posts, :owner_name)"
    [ ":uri"               := nbUri         newboard
    , ":title"             := nbTitle       newboard
    , ":description"       := nbDescription newboard
    , ":permission"        := AnyThreadsAnyReplies
    , ":index_view_policy" := IndexViewDisallowed
    , ":total_posts"       := (0 :: Int)
    , ":owner_name"        := username user
    ]

insertPostNow :: DB.Connection -> Board -> Maybe Thread -> NewPost -> Bool -> Maybe Text -> Maybe Text -> Maybe File -> IO Post
insertPostNow conn board mThread newpost sage_ tripcode_ capcode_ mFile = do
  -- Insert/update the post's file if the post has a file.
  case mFile of
    Nothing   -> pure ()
    Just file -> do
      DB.executeNamed conn
        "INSERT INTO file (hash, size, ext, has_thumb, thumb_width, thumb_height, mime) \
        \ VALUES (:hash, :size, :ext, :has_thumb, :thumb_width, :thumb_height, :mime)   \
        \ ON CONFLICT (hash) DO UPDATE                                                  \
        \ SET (size, ext, has_thumb, thumb_width, thumb_height, mime) =                 \
        \ (:size, :ext, :has_thumb, :thumb_width, :thumb_height, :mime)                 "
        [ ":hash"         := hash        file
        , ":size"         := size        file
        , ":ext"          := ext         file
        , ":has_thumb"    := hasThumb    file
        , ":thumb_width"  := thumbWidth  file
        , ":thumb_height" := thumbHeight file
        , ":mime"         := mime        file
        ]

  -- Replace dummy quotelinks with genuine links where possible.
  -- (e.g. >>123 >>>/board/ >>>/board/123)
  (postquotes, messageWithQuotelinks) <- findAndReplaceQuotelinks conn board newpost

  -- Insert the post.
  DB.executeNamed conn
    "INSERT INTO post (board_uri, no, thread_no, sage, name, tripcode, capcode, email, subject, nomarkup, message, file_hash) \
    \ VALUES (:uri, (SELECT total_posts + 1 FROM board WHERE uri = :uri), :thread_no, :sage, :name, :tripcode, :capcode, :email, :subject, :nomarkup, :message, :file_hash)"
    [ ":uri"       := uri board
    , ":thread_no" := tPostNo <$> mThread
    , ":sage"      := sage_
    , ":name"      := if T.null (npName newpost) then anonName board else npName newpost
    , ":tripcode"  := tripcode_
    , ":capcode"   := capcode_
    , ":email"     := npEmail   newpost
    , ":subject"   := npSubject newpost
    , ":nomarkup"  := npNomarkup newpost
    , ":message"   := messageWithQuotelinks
    , ":file_hash" := hash <$> mFile
    ]

  -- Get the just-inserted post.
  post <- getInsertedPostNow conn

  -- Insert quotes.
  let
    childAbsThreadNo = fromMaybe (no post) (tPostNo <$> mThread)
    in
    forM_ postquotes $ \parentNo -> do
      parent <- getPostOrErrorNow "insertPostNow" conn board parentNo
      let parentAbsThreadNo = fromMaybe (no parent) (pThreadNo parent)
      when (parentAbsThreadNo == childAbsThreadNo) $
        DB.executeNamed conn
          "INSERT INTO quote (board_uri, parent_no, child_no) \
            \ VALUES (:board_uri, :parent_no, :child_no)"
          [ ":board_uri" := uri board
          , ":parent_no" := parentNo
          , ":child_no"  := no post
          ]

  -- Increment the board's total_posts.
  DB.execute conn
    "UPDATE board SET total_posts = total_posts + 1 WHERE uri = ?"
    [uri board]

  -- Return the inserted post.
  pure post

insertThreadPostNow :: DB.Connection -> Board -> NewPost -> Maybe Text -> Maybe Text -> Maybe File -> IO Post
insertThreadPostNow conn board newpost tripcode_ capcode_ mFile = do
  -- Insert the post.
  insertPostNow conn board Nothing newpost False tripcode_ capcode_ mFile

insertThreadReplyNow :: DB.Connection -> Board -> Thread -> NewPost -> Maybe Text -> Maybe Text -> Maybe File -> IO (Post, [File])
insertThreadReplyNow conn board thread newpost tripcode_ capcode_ mFile = do
  -- Insert the post.
  let sage_ = npEmail newpost == "sage" || bumplocked thread
  post <- insertPostNow conn board (Just thread) newpost sage_ tripcode_ capcode_ mFile

  -- Update the thread's last_activity.
  time <- roundDownUTCTime <$> getCurrentTime
  DB.executeNamed conn
    "UPDATE thread SET last_activity = :last_activity WHERE (board_uri, post_no) = (:board_uri, :post_no)"
    [ ":last_activity" := time
    , ":board_uri"     := uri board
    , ":post_no"       := tPostNo thread
    ]

  -- Bump the thread if this is a non-sage reply.
  when (not sage_) $ do
    DB.executeNamed conn
      "UPDATE thread SET bumped = :bumped WHERE (board_uri, post_no) = (:board_uri, :post_no)"
      [ ":bumped"    := time
      , ":board_uri" := uri board
      , ":post_no"   := tPostNo thread
      ]

  doomedFiles <-
    -- If the thread is cyclic...
    if lock thread == Cyclic || lock thread == LockedCyclic
    then
      -- ...and the bump limit has been EXCEEDED...
      if nReplies thread + 1 > bumpLimit board
      then do
        -- ...delete the earliest replies.
          doomedPosts <- DB.queryNamed conn
            "SELECT * FROM post WHERE (board_uri, thread_no) = (:board_uri, :thread_no) ORDER BY no LIMIT :limit"
            [ ":board_uri" := uri board
            , ":thread_no" := tPostNo thread
            , ":limit"     := nReplies thread + 1 - bumpLimit board
            ]
          doomedFileHashes <- nub . catMaybes <$> mapM (deletePostStrictlyNow conn) doomedPosts
          doomedFiles <- forM doomedFileHashes $ getFileOrErrorNow "insertThreadReplyNow" conn
          forM_ doomedFileHashes $ deleteFileNow conn
          pure doomedFiles
        else pure []

    -- If the thread is not cyclic...
    else do
      -- ...and the bump limit has been REACHED...
      when (nReplies thread + 1 >= bumpLimit board) $ do
        -- ...bumplock the thread.
        let newLock = addBumplock $ lock thread
        when (newLock /= lock thread) $
          DB.executeNamed conn
            "UPDATE thread SET lock = :lock WHERE (board_uri, post_no) = (:board_uri, :post_no)"
            [ ":lock"      := newLock
            , ":board_uri" := uri board
            , ":post_no"   := tPostNo thread
            ]
      pure []

  -- Make the thread full if the reply limit has been reached.
  when (nReplies thread + 1 >= replyLimit board) $
    DB.executeNamed conn
      "UPDATE thread SET lock = :lock WHERE (board_uri, post_no) = (:board_uri, :post_no)"
      [ ":lock"      := addFull (lock thread)
      , ":board_uri" := uri board
      , ":post_no"   := tPostNo thread
      ]

  -- Increment the thread's n_replies.
  DB.executeNamed conn
    "UPDATE thread SET n_replies = n_replies + 1 WHERE (board_uri, post_no) = (:board_uri, :post_no)"
    [ ":board_uri" := uri board
    , ":post_no"   := tPostNo thread
    ]

  -- Increment the thread's n_files if this post has a file.
  when (mFile /= Nothing) $
    DB.executeNamed conn
      "UPDATE thread SET n_files = n_files + 1 WHERE (board_uri, post_no) = (:board_uri, :post_no)"
      [ ":board_uri" := uri board
      , ":post_no"   := tPostNo thread
      ]

  pure (post, doomedFiles)

  where
    bumplocked thread =
      case lock thread of
        Free             -> False
        Bumplocked       -> True
        Cyclic           -> False
        Locked           -> error "[Phi.Database.Queries.Now:insertPostNow] tried to reply to a locked thread"
        LockedBumplocked -> error "[Phi.Database.Queries.Now:insertPostNow] tried to reply to a locked bumplocked thread"
        LockedCyclic     -> error "[Phi.Database.Queries.Now:insertPostNow] tried to reply to a locked cyclic thread"
        Full             -> error "[Phi.Database.Queries.Now:insertPostNow] tried to reply to a full thread"
    addBumplock lock_ =
      case lock_ of
        Free             -> Bumplocked
        Bumplocked       -> Bumplocked
        Cyclic           -> Cyclic
        Locked           -> LockedBumplocked
        LockedBumplocked -> LockedBumplocked
        LockedCyclic     -> LockedCyclic
        Full             -> Full
    addFull lock_ =
      case lock_ of
        Free             -> Full
        Bumplocked       -> Full
        Cyclic           -> Cyclic
        Locked           -> Full
        LockedBumplocked -> Full
        LockedCyclic     -> LockedCyclic
        Full             -> Full

insertThreadNow :: DB.Connection -> Board -> Post -> IO ()
insertThreadNow conn board post = do
  DB.executeNamed conn
    "INSERT INTO thread (board_uri, post_no, last_activity, bumped, n_replies, n_files, stickiness, lock) \
    \ VALUES (:board_uri, :post_no, :last_activity, :bumped, :n_replies, :n_files, :stickiness, :lock)"
    [ ":board_uri"     := uri board
    , ":post_no"       := no       post
    , ":last_activity" := datetime post
    , ":bumped"        := datetime post
    , ":n_replies"     := (0 :: Int)
    , ":n_files"       := (if fileHash post == Nothing then 0 else 1 :: Int)
    , ":stickiness"    := (0 :: Int)
    , ":lock"          := Free
    ]

insertUserNow :: DB.Connection -> NewUser -> IO ()
insertUserNow conn newuser = do
  pwhash_ <- hashPassword $ nuPassword newuser
  DB.executeNamed conn
    "INSERT INTO user (username, pwhash) VALUES (:username, :pwhash)"
    [ ":username" := nuUsername newuser
    , ":pwhash"   := pwhash_
    ]

getPowerlevelNow :: DB.Connection -> Board -> User -> IO Powerlevel
getPowerlevelNow conn board user
  | admin user = pure Admin
  | T.toCaseFold (ownerName board) == T.toCaseFold (username user) = pure BoardOwner
  | otherwise = do
    mIsManager <- checkBoardModNow conn board user
    case mIsManager of
      Just True  -> pure BoardManager
      Just False -> pure BoardMod
      Nothing    -> pure Commoner

checkBoardModNow :: DB.Connection -> Board -> User -> IO (Maybe Bool)
checkBoardModNow conn board user = do
  bools <- (fmap fromOnly) <$>
    DB.queryNamed conn
      "SELECT manager FROM board_mod WHERE (board_uri, username) = (:board_uri, :username)"
      [ ":board_uri" := uri board
      , ":username"  := username user
      ]
  case bools of
    []          -> pure Nothing
    [isManager] -> pure $ Just isManager
    _           -> error "[Phi.Database.Queries.Now:checkBoardModNow] database is corrupt: query selecting at most a single row returned multiple rows"

getBoardModTuplesNow :: DB.Connection -> Board -> IO [(Text, Bool)]
getBoardModTuplesNow conn board =
  DB.query conn
    "SELECT username, manager FROM board_mod WHERE board_uri = ?"
    [uri board]

setBoardSettingsNow :: DB.Connection -> Board -> BoardSettings -> IO ()
setBoardSettingsNow conn board boardsettings = do
  -- Update settings.
  DB.executeNamed conn
    "UPDATE board SET (title, description, theme, anon_name, bump_limit, reply_limit, thread_limit, permission, index_view_policy) = (:title, :description, :theme, :anon_name, :bump_limit, :reply_limit, :thread_limit, :permission, :index_view_policy) WHERE uri = :uri"
    [ ":uri"               := uri board
    , ":title"             := bsTitle           boardsettings
    , ":description"       := bsDescription     boardsettings
    , ":theme"             := bsMTheme          boardsettings
    , ":anon_name"         := bsAnonName        boardsettings
    , ":bump_limit"        := bsBumpLimit       boardsettings
    , ":reply_limit"       := bsReplyLimit      boardsettings
    , ":thread_limit"      := bsThreadLimit     boardsettings
    , ":permission"        := bsPermission      boardsettings
    , ":index_view_policy" := bsIndexViewPolicy boardsettings
    ]
  forM_ (bsSelectMods boardsettings) $ \modname ->
    case bsUntoMods boardsettings of
       -- Remove mods.
      Nothing -> do
        DB.executeNamed conn
          "DELETE FROM board_mod WHERE (board_uri, username) = (:board_uri, :username)"
          [ ":board_uri" := uri board
          , ":username"  := modname
          ]
      -- Promote mods to manager.
      Just True -> do
        DB.executeNamed conn
          "UPDATE board_mod SET manager = TRUE WHERE (board_uri, username) = (:board_uri, :username)"
          [ ":board_uri" := uri board
          , ":username"  := modname
          ]
      -- Demote mods to non-manager.
      Just False -> do
        DB.executeNamed conn
          "UPDATE board_mod SET manager = FALSE WHERE (board_uri, username) = (:board_uri, :username)"
          [ ":board_uri" := uri board
          , ":username"  := modname
          ]
  case bsAddMod boardsettings of
    Nothing      -> pure ()
    Just modname -> do
      -- Add a new mod.
      DB.executeNamed conn
        "INSERT INTO board_mod (board_uri, username) VALUES (:board_uri, :username)"
        [ ":board_uri" := uri board
        , ":username"  := modname
        ]

getLogsNow :: DB.Connection -> IO [Log]
getLogsNow conn =
  DB.query_ conn
    "SELECT * FROM log ORDER BY id DESC"

setGlobalSettingsNow :: DB.Connection -> GlobalSettings -> IO ()
setGlobalSettingsNow conn globalsettings =
  DB.executeNamed conn
    "UPDATE global_settings SET (global_theme, open_registration, user_board_creation, captcha_baseline) = (:global_theme, :open_registration, :user_board_creation, :captcha_baseline) WHERE ROWID = 1"
    [ ":global_theme"        := globalTheme globalsettings
    , ":open_registration"   := openRegistration globalsettings
    , ":user_board_creation" := userBoardCreation globalsettings
    , ":captcha_baseline"    := captchaBaseline globalsettings
    ]

-- Delete a post and any referencing quotes, and return the post's filehash if
-- it exists and the file has become orphaned. This function does not delete
-- any threads or files and it doesn't log anything.
deletePostStrictlyNow :: DB.Connection -> Post -> IO (Maybe FileHash)
deletePostStrictlyNow conn post = do
  -- Delete quotes.
  DB.executeNamed conn
    "DELETE FROM quote WHERE board_uri = :board_uri AND (parent_no = :no OR child_no = :no)"
    [ ":board_uri" := pBoardUri post
    , ":no"        := no post
    ]

  -- Delete the post.
  DB.executeNamed conn
    "DELETE FROM post WHERE (board_uri, no) = (:board_uri, :no)"
    [ ":board_uri" := pBoardUri post
    , ":no"        := no post
    ]

  -- Track the post's filehash if the file exists and has become an orphan.
  mFilehash <-
    case fileHash post of
      Nothing       -> pure Nothing
      Just filehash -> listToMaybe <$> filterM (orphaned conn) [filehash]

  case pThreadNo post of
    Nothing -> pure ()
    -- If the post is a reply to a thread...
    Just threadNo -> do
      -- ...decrement the thread's n_replies.
      DB.executeNamed conn
        "UPDATE thread SET n_replies = n_replies - 1 WHERE (board_uri, post_no) = (:board_uri, :post_no)"
        [ ":board_uri" := pBoardUri post
        , ":post_no"   := threadNo
        ]
      -- ...and the post has a file...
      when (isJust $ fileHash post) $ do
        -- ...decrement the thread's n_files.
        DB.executeNamed conn
          "UPDATE thread SET n_files = n_files - 1 WHERE (board_uri, post_no) = (:board_uri, :post_no)"
          [ ":board_uri" := pBoardUri post
          , ":post_no"   := threadNo
          ]

  pure mFilehash

deleteFileNow :: DB.Connection -> FileHash -> IO ()
deleteFileNow conn filehash =
  DB.execute conn
    "DELETE FROM file WHERE hash = ?"
    [filehash]

logAtTimeNow :: DB.Connection -> UTCTime -> (Int -> UTCTime -> Log) -> IO ()
logAtTimeNow conn time mkLog = do
  [Only id_] <- DB.query_ conn
    "SELECT (CASE WHEN MAX(id) ISNULL THEN 1 ELSE MAX(id) + 1 END) FROM log"
  DB.execute conn
    "INSERT INTO log (id, datetime, username, board_uri, post_no, file_hash, file_size, file_mime, action, value, reason) \
    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (mkLog id_ time)

logNow :: DB.Connection -> (Int -> UTCTime -> Log) -> IO ()
logNow conn mkLog = do
  time <- roundDownUTCTime <$> getCurrentTime
  logAtTimeNow conn time mkLog

logManyNow :: DB.Connection -> [(Text, Int)] -> ((Text, Int) -> Int -> UTCTime -> Log) -> IO ()
logManyNow conn postTuples mkLog = do
  time <- roundDownUTCTime <$> getCurrentTime
  forM_ postTuples $ \postTuple ->
    logAtTimeNow conn time $ mkLog postTuple

-- The number of posts that were not ignored when performing many mod actions.
type PerformedActions = Int

modSticky :: DB.Connection -> User -> Text -> [(Text, Int)] -> Int -> IO PerformedActions
modSticky conn user reason postTuples stickiness_ = do
  postTuples' <- catMaybes <$>
    forM postTuples ( \postTuple@(uri_, no_) -> do
      -- Get current stickiness.
      [Only currentStickiness] <- DB.queryNamed conn
        "SELECT stickiness FROM thread WHERE (board_uri, post_no) = (:board_uri, :post_no)"
        [ ":board_uri"  := uri_
        , ":post_no"    := no_
        ]

      if stickiness_ == currentStickiness
      then pure Nothing
      else do
        -- If it's different, set stickiness.
        DB.executeNamed conn
          "UPDATE thread SET stickiness = :stickiness WHERE (board_uri, post_no) = (:board_uri, :post_no)"
          [ ":stickiness" := stickiness_
          , ":board_uri"  := uri_
          , ":post_no"    := no_
          ]
        pure $ Just postTuple
    )

  -- Log.
  logManyNow conn postTuples' mkLog -- only the threads whose stickiness changed

  -- Return the number of performed actions.
  pure $ length postTuples'

  where
    mkLog (uri_, no_) logId_ logDatetime_ = Log
      { logId       = logId_
      , logDatetime = logDatetime_
      , logUsername = username user
      , logBoardUri = uri_
      , logMPostNo  = Just no_
      , logAction   = SetStickinessTo stickiness_
      , logReason   = reason
      }

modLockBit :: DB.Connection -> User -> Text -> [(Text, Int)] -> Lock -> Bool -> IO PerformedActions
modLockBit conn user reason postTuples bit bool = do
  case mTransformedLockAndLogAction of
    Nothing -> pure 0
    Just (transformLock, logAction_) -> do
      -- Set each thread's lock bit (iff it's different).
      postTuples' <- catMaybes <$>
        forM postTuples ( \postTuple@(uri_, no_) -> do
          thread <- getThreadOrErrorNow' "modLockBit" conn uri_ no_
          case transformLock thread of
            Nothing   -> pure Nothing -- no change, no database interaction
            Just lock -> do
              DB.executeNamed conn
                "UPDATE thread SET lock = :lock WHERE (board_uri, post_no) = (:board_uri, :post_no)"
                [ ":lock"      := transformLock thread
                , ":board_uri" := uri_
                , ":post_no"   := no_
                ]
              pure $ Just postTuple
        )
      -- Log.
      logManyNow conn postTuples' $ mkLog logAction_ -- only the posts whose lock changed

      -- Return the number of performed actions.
      pure $ length postTuples'

  where
    mkLog logAction_ (uri_, no_) logId_ logDatetime_ = Log
      { logId       = logId_
      , logDatetime = logDatetime_
      , logUsername = username user
      , logBoardUri = uri_
      , logMPostNo  = Just no_
      , logAction   = logAction_
      , logReason   = reason
      }
    mTransformedLockAndLogAction =
      case (bit, bool) of
        (Locked, True)      -> Just (setLocked,        SetLockTo     True)
        (Locked, False)     -> Just (setNotLocked,     SetLockTo     False)
        (Cyclic, True)      -> Just (setCyclic,        SetCyclicTo   True)
        (Cyclic, False)     -> Just (setNotCyclic,     SetCyclicTo   False)
        (Bumplocked, True)  -> Just (setBumplocked,    SetBumplockTo True)
        (Bumplocked, False) -> Just (setNotBumplocked, SetBumplockTo False)
        _                   -> Nothing
    setLocked thread =
      case lock thread of
        Free             -> Just Locked
        Bumplocked       -> Just LockedBumplocked
        Cyclic           -> Just LockedCyclic
        Locked           -> Nothing
        LockedBumplocked -> Nothing
        LockedCyclic     -> Nothing
        Full             -> Nothing
    setNotLocked thread =
      case lock thread of
        Free             -> Nothing
        Bumplocked       -> Nothing
        Cyclic           -> Nothing
        Locked           -> Just Free
        LockedBumplocked -> Just Bumplocked
        LockedCyclic     -> Just Cyclic
        Full             -> Nothing
    setCyclic thread =
      case lock thread of
        Free             -> Just Cyclic
        Bumplocked       -> Just Cyclic
        Cyclic           -> Nothing
        Locked           -> Just LockedCyclic
        LockedBumplocked -> Just LockedCyclic
        LockedCyclic     -> Nothing
        Full             -> Nothing
    setNotCyclic thread =
      case lock thread of
        Free             -> Nothing
        Bumplocked       -> Nothing
        Cyclic           -> Just Free
        Locked           -> Nothing
        LockedBumplocked -> Nothing
        LockedCyclic     -> Just Locked
        Full             -> Nothing
    setBumplocked thread =
      case lock thread of
        Free             -> Just Bumplocked
        Bumplocked       -> Nothing
        Cyclic           -> Just Bumplocked
        Locked           -> Just LockedBumplocked
        LockedBumplocked -> Nothing
        LockedCyclic     -> Just LockedBumplocked
        Full             -> Nothing
    setNotBumplocked thread =
      case lock thread of
        Free             -> Nothing
        Bumplocked       -> Just Free
        Cyclic           -> Nothing
        Locked           -> Nothing
        LockedBumplocked -> Just Locked
        LockedCyclic     -> Nothing
        Full             -> Nothing

getPostFileHash :: DB.Connection -> Text -> Int -> IO (Maybe FileHash)
getPostFileHash conn uri_ no_ = do
  [Only mFilehash] <- DB.queryNamed conn
    "SELECT file_hash FROM post WHERE (board_uri, no) = (:board_uri, :no)"
    [ ":board_uri" := uri_
    , ":no"        := no_
    ]
  pure mFilehash

modUnlink :: DB.Connection -> User -> Text -> [(Text, Int)] -> IO (PerformedActions, [File])
modUnlink conn user reason postTuples = do
  -- Find all the posts' files.
  filesByPostTuple <- catMaybes <$>
    forM postTuples ( \postTuple@(uri_, no_) -> do
      mFilehash <- getPostFileHash conn uri_ no_
      mFile <- case mFilehash of
        Nothing       -> pure Nothing
        Just filehash -> Just <$> getFileOrErrorNow "modUnlink" conn filehash
      pure $ ((,) postTuple) <$> mFile
    )

  -- Unlink the files.
  let postTuples' = map fst filesByPostTuple -- only the posts that have files
  forM_ postTuples' $ \postTuple@(uri_, no_) ->
    DB.executeNamed conn
      "UPDATE post SET file_hash = NULL WHERE (board_uri, no) = (:board_uri, :no)"
      [ ":board_uri" := uri_
      , ":no"        := no_
      ]

  -- For each file, if it's now an orphan, delete it from the file table.
  let files = map snd filesByPostTuple
  orphanedFiles <- catMaybes <$>
    forM files ( \file -> do
      orphan <- orphaned conn (hash file)
      if orphan
      then do
        deleteFileNow conn (hash file)
        pure $ Just file
      else pure Nothing
    )

  -- Log.
  logManyNow conn postTuples' $ mkLog filesByPostTuple

  -- Return the number of performed actions and the files that were orphaned.
  pure (length postTuples', orphanedFiles)

  where
    mkLog filesByPostTuple postTuple@(uri_, no_) logId_ logDatetime_ = Log
      { logId       = logId_
      , logDatetime = logDatetime_
      , logUsername = username user
      , logBoardUri = uri_
      , logMPostNo  = Just no_
      , logAction   = mkLogAction postTuple filesByPostTuple
      , logReason   = reason
      }
    mkLogAction postTuple filesByPostTuple =
      case lookup postTuple filesByPostTuple of
        Nothing   -> error "[Phi.Database.Queries.Now:modUnlink] posts inconsistently said to have files and not have files"
        Just file -> DidUnlinkFile (hash file) (size file) (mime file)

modPurge :: DB.Connection -> User -> Text -> [(Text, Int)] -> IO (PerformedActions, [File])
modPurge conn user reason postTuples = do
  -- Find all the posts' files.
  filesByPostTuple <- catMaybes <$>
    forM postTuples ( \postTuple@(uri_, no_) -> do
      mFilehash <- getPostFileHash conn uri_ no_
      mFile <- case mFilehash of
        Nothing       -> pure Nothing
        Just filehash -> Just <$> getFileOrErrorNow "modPurge" conn filehash
      pure $ ((,) postTuple) <$> mFile
    )

  -- Find all the posts that have any of these files.
  let files = map snd filesByPostTuple
      filehashes = map hash files
  targetPostTuples <-
    DB.query_ conn $
      DB.Query $ "SELECT board_uri, no FROM post WHERE file_hash IN " <> mkSqlListFromTexts filehashes
    :: IO [(Text, Int)]

  -- Unlink the files.
  forM_ targetPostTuples $ \postTuple@(uri_, no_) ->
    DB.executeNamed conn
      "UPDATE post SET file_hash = NULL WHERE (board_uri, no) = (:board_uri, :no)"
      [ ":board_uri" := uri_
      , ":no"        := no_
      ]

  -- Delete the files from the file table.
  let files = nubBy (\file1 file2 -> hash file1 == hash file2) $ map snd filesByPostTuple
  files <-
    forM files $ \file -> do
      deleteFileNow conn (hash file)
      pure file

  -- Log.
  let postTuples' = map fst filesByPostTuple -- only the posts that have files
  logManyNow conn postTuples' $ mkLog filesByPostTuple

  -- Return the number of performed actions and the files that were orphaned.
  pure (length postTuples', files)

  where
    mkLog filesByPostTuple postTuple@(uri_, no_) logId_ logDatetime_ = Log
      { logId       = logId_
      , logDatetime = logDatetime_
      , logUsername = username user
      , logBoardUri = uri_
      , logMPostNo  = Just no_
      , logAction   = mkLogAction postTuple filesByPostTuple
      , logReason   = reason
      }
    mkLogAction postTuple filesByPostTuple =
      case lookup postTuple filesByPostTuple of
        Nothing   -> error "[Phi.Database.Queries.Now:modPurge] posts inconsistently said to have files and not have files"
        Just file -> DidPurgeFile (hash file) (size file) (mime file)

modDelete :: DB.Connection -> User -> Text -> [(Text, Int)] -> IO (PerformedActions, [File])
modDelete conn user reason postTuples = do
  postsByPostTuple <- sequence [((,) postTuple) <$> getPostOrErrorNow' "modDelete" conn uri_ no_ | postTuple@(uri_, no_) <- postTuples]
  filehashes <- nub . concat <$>
    forM postTuples ( \postTuple@(uri_, no_) -> do
      let post = fromJust $ lookup postTuple postsByPostTuple
      filehashes <- case pThreadNo post of
        -- If the post constitutes a thread...
        Nothing -> do
          -- ...track potentially orphaned files.
          replyFilehashes <- catMaybes <$> fmap fromOnly <$>
              DB.queryNamed conn
              "SELECT file_hash FROM post WHERE (board_uri, thread_no) = (:board_uri, :thread_no)"
              [ ":board_uri" := uri_
              , ":thread_no" := no_
              ]
          -- ...delete the thread.
          DB.executeNamed conn
            "DELETE FROM thread WHERE (board_uri, post_no) = (:board_uri, :post_no)"
            [ ":board_uri" := uri_
            , ":post_no"   := no_
            ]
          -- ...delete replies to the thread.
          DB.executeNamed conn
            "DELETE FROM post WHERE (board_uri, thread_no) = (:board_uri, :thread_no)"
            [ ":board_uri" := uri_
            , ":thread_no" := no_
            ]
          case fileHash post of
            Nothing           -> pure replyFilehashes
            Just postFilehash -> pure $ postFilehash : replyFilehashes

        -- If the post is a reply to a thread...
        Just threadNo -> do
          -- ...decrement the thread's n_replies.
          DB.executeNamed conn
            "UPDATE thread SET n_replies = n_replies - 1 WHERE (board_uri, post_no) = (:board_uri, :post_no)"
            [ ":board_uri" := uri_
            , ":post_no"   := threadNo
            ]
          -- ...and the post has a file...
          when (isJust $ fileHash post) $ do
            -- ...decrement the thread's n_files.
            DB.executeNamed conn
             "UPDATE thread SET n_files = n_files - 1 WHERE (board_uri, post_no) = (:board_uri, :post_no)"
              [ ":board_uri" := uri_
              , ":post_no"   := threadNo
              ]
          -- ...unbump the thread.
          [previousBumped] <- fmap fromOnly <$>
            DB.queryNamed conn
              "SELECT datetime FROM post \
              \ WHERE (board_uri, thread_no) = (:board_uri, :thread_no) AND no != :no \
              \ AND sage = FALSE \
              \ OR (board_uri, no) = (:board_uri, :thread_no) \
              \ ORDER BY datetime DESC LIMIT 1"
              [ ":board_uri" := uri_
              , ":thread_no" := threadNo
              , ":no"        := no_
              ]
            :: IO [UTCTime]
          DB.executeNamed conn
            "UPDATE thread SET bumped = :bumped WHERE (board_uri, post_no) = (:board_uri, :post_no)"
            [ ":bumped"    := previousBumped
            , ":board_uri" := uri_
            , ":post_no"   := threadNo
            ]
          -- ...roll back the thread's last_activty.
          [previousLastActivity] <- fmap fromOnly <$>
            DB.queryNamed conn
              "SELECT datetime FROM post \
              \ WHERE (board_uri, thread_no) = (:board_uri, :thread_no) AND no != :no \
              \ OR (board_uri, no) = (:board_uri, :thread_no) \
              \ ORDER BY datetime DESC LIMIT 1"
              [ ":board_uri" := uri_
              , ":thread_no" := threadNo
              , ":no"        := no_
              ]
            :: IO [UTCTime]
          DB.executeNamed conn
            "UPDATE thread SET last_activity = :last_activity \
            \ WHERE (board_uri, post_no) = (:board_uri, :post_no)"
            [ ":last_activity"  := previousLastActivity
            , ":board_uri"      := uri_
            , ":post_no"        := threadNo
            ]

          pure $ maybeToList $ fileHash post

      -- Delete quotes.
      DB.executeNamed conn
        "DELETE FROM quote WHERE board_uri = :board_uri AND (parent_no = :no OR child_no = :no)"
        [ ":board_uri" := uri_
        , ":no"        := no_
        ]

      -- Delete the post.
      DB.executeNamed conn
        "DELETE FROM post WHERE (board_uri, no) = (:board_uri, :no)"
        [ ":board_uri" := uri_
        , ":no"        := no_
        ]

      pure filehashes
    )

  -- Track orphaned files.
  orphanedFilehashes <- filterM (orphaned conn) filehashes
  orphanedFiles <- forM orphanedFilehashes $ getFileOrErrorNow "modDelete" conn

  -- Delete orphaned files from the file table.
  DB.execute_ conn $
    DB.Query $ "DELETE FROM file WHERE hash in " <> mkSqlListFromTexts orphanedFilehashes

  -- Log.
  logManyNow conn postTuples $ mkLog postsByPostTuple

  -- Return the number of performed actions and the files that were orphaned.
  pure (length postTuples, orphanedFiles)

  where
    mkLog postsByPostTuple postTuple@(uri_, no_) logId_ logDatetime_ = Log
      { logId       = logId_
      , logDatetime = logDatetime_
      , logUsername = username user
      , logBoardUri = uri_
      , logMPostNo  = Just no_
      , logAction   = mkLogAction postTuple postsByPostTuple
      , logReason   = reason
      }

    mkLogAction :: (Text, Int) -> [((Text, Int), Post)] -> LogAction
    mkLogAction postTuple postsByPostTuple =
      case lookup postTuple postsByPostTuple of
        Nothing   -> error "[Phi.Database.Queries.Now:modDelete] posts inconsistently said to exist and not exist"
        Just post -> if isThread post then DidDeleteThread else DidDeletePost

    isThread :: Post -> Bool
    isThread = isNothing . pThreadNo

orphaned :: DB.Connection -> FileHash -> IO Bool
orphaned conn filehash = do
  [Only n] <- DB.query conn
    "SELECT COUNT(*) FROM post WHERE file_hash = ?"
    [filehash]
    :: IO [Only Int]
  pure $ n < 1
