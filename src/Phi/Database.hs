{-# LANGUAGE OverloadedStrings #-}

module Phi.Database where

import           Data.Pool (withResource)
import qualified Database.SQLite.Simple as DB
import           Database.SQLite.Simple (Only(..))

import           Phi.Context (Context(..))

createTables :: Context -> IO ()
createTables context =
  withResource (db context) $ \conn -> do
    DB.execute_ conn
      "PRAGMA foreign_keys = ON"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS board                                \
      \ ( uri               TEXT    NOT NULL PRIMARY KEY               \
      \ , title             TEXT    NOT NULL                           \
      \ , description       TEXT    NOT NULL                           \
      \ , theme             INTEGER                                    \
      \ , anon_name         TEXT    NOT NULL DEFAULT 'Anonymous'       \
      \ , bump_limit        INTEGER NOT NULL DEFAULT  256              \
      \ , reply_limit       INTEGER NOT NULL DEFAULT  512              \
      \ , thread_limit      INTEGER NOT NULL DEFAULT 1024              \
      \ , permission        INTEGER NOT NULL                           \
      \ , index_view_policy INTEGER NOT NULL                           \
      \ , total_posts       INTEGER NOT NULL                           \
      \ , created           TEXT    NOT NULL DEFAULT CURRENT_TIMESTAMP \
      \ , owner_name        TEXT    NOT NULL                           \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS post                                              \
      \ ( board_uri TEXT    NOT NULL                                                \
      \ , no        INTEGER NOT NULL                                                \
      \ , thread_no INTEGER                                                         \
      \ , sage      INTEGER NOT NULL                                                \
      \ , name      TEXT    NOT NULL                                                \
      \ , tripcode  TEXT                                                            \
      \ , capcode   TEXT                                                            \
      \ , email     TEXT    NOT NULL                                                \
      \ , subject   TEXT    NOT NULL                                                \
      \ , datetime  TEXT    NOT NULL DEFAULT CURRENT_TIMESTAMP                      \
      \ , nomarkup  TEXT    NOT NULL                                                \
      \ , message   TEXT    NOT NULL                                                \
      \ , file_hash TEXT                                                            \
      \ , origin    TEXT    NOT NULL DEFAULT 'clearnet'                            \
      \ , PRIMARY KEY (board_uri, no)                                               \
      \ , FOREIGN KEY (board_uri, thread_no) REFERENCES thread (board_uri, post_no) \
      \ , FOREIGN KEY (file_hash) REFERENCES file (hash)                            \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS thread                                   \
      \ ( board_uri     TEXT    NOT NULL                                   \
      \ , post_no       INTEGER NOT NULL                                   \
      \ , last_activity TEXT    NOT NULL                                   \
      \ , bumped        TEXT    NOT NULL                                   \
      \ , n_replies     INTEGER NOT NULL                                   \
      \ , n_files       INTEGER NOT NULL                                   \
      \ , stickiness    INTEGER NOT NULL                                   \
      \ , lock          INTEGER NOT NULL                                   \
      \ , PRIMARY KEY (board_uri, post_no)                                 \
      \ , FOREIGN KEY (board_uri, post_no) REFERENCES post (board_uri, no) \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS file           \
      \ ( hash         TEXT    NOT NULL PRIMARY KEY \
      \ , ext          TEXT    NOT NULL             \
      \ , size         INTEGER NOT NULL             \
      \ , has_thumb    INTEGER NOT NULL             \
      \ , thumb_width  INTEGER                      \
      \ , thumb_height INTEGER                      \
      \ , mime         TEXT                         \
      \ , original_name TEXT NOT NULL DEFAULT ''   \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS user                           \
      \ ( username    TEXT    NOT NULL UNIQUE COLLATE NOCASE     \
      \ , pwhash      TEXT    NOT NULL                           \
      \ , admin       INTEGER NOT NULL DEFAULT FALSE             \
      \ , last_active TEXT    NOT NULL DEFAULT CURRENT_TIMESTAMP \
      \ , PRIMARY KEY (username)                                 \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS log            \
      \ ( id        INTEGER NOT NULL PRIMARY KEY \
      \ , datetime  TEXT    NOT NULL             \
      \ , username  TEXT    NOT NULL             \
      \ , board_uri TEXT    NOT NULL             \
      \ , post_no   INTEGER                      \
      \ , file_hash TEXT                         \
      \ , file_size INTEGER                      \
      \ , file_mime TEXT                         \
      \ , action    INTEGER NOT NULL             \
      \ , value     INTEGER                      \
      \ , reason    TEXT    NOT NULL             \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS global_settings            \
       \ ( global_theme        INTEGER NOT NULL DEFAULT 0     \
       \ , open_registration   INTEGER NOT NULL DEFAULT TRUE  \
       \ , user_board_creation INTEGER NOT NULL DEFAULT FALSE \
       \ , captcha_baseline    INTEGER NOT NULL DEFAULT TRUE  \
       \ , captcha_provider    INTEGER NOT NULL DEFAULT 0     \
        \ , origin_indicators   INTEGER NOT NULL DEFAULT TRUE  \
       \ , origin_indicators  INTEGER NOT NULL DEFAULT 1     \
       \ )"
    _ <- DB.withTransaction conn $ do
      [Only n] <- DB.query_ conn
        "SELECT COUNT(*) FROM global_settings"
        :: IO [Only Int]
      case n of
        0 -> DB.execute_ conn "INSERT INTO global_settings DEFAULT VALUES"
        1 -> pure ()
        _ -> error "[Phi.Database:createTables] database is corrupt: more than one row in table global_settings"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS quote                                      \
      \ ( board_uri TEXT    NOT NULL                                         \
      \ , parent_no INTEGER NOT NULL                                         \
      \ , child_no  INTEGER NOT NULL                                         \
      \ , PRIMARY KEY (board_uri, parent_no, child_no)                       \
      \ , FOREIGN KEY (board_uri, parent_no) REFERENCES post (board_uri, no) \
      \ , FOREIGN KEY (board_uri, child_no)  REFERENCES post (board_uri, no) \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS banner                 \
      \ ( board_uri TEXT NOT NULL                        \
      \ , hash      TEXT NOT NULL                        \
      \ , ext       TEXT NOT NULL                        \
      \ , PRIMARY KEY (board_uri, hash)                  \
      \ , FOREIGN KEY (board_uri) REFERENCES board (uri) \
      \ )"
    DB.execute_ conn
      "CREATE TABLE IF NOT EXISTS board_mod                 \
      \ ( board_uri TEXT    NOT NULL                        \
      \ , username  TEXT    NOT NULL COLLATE NOCASE         \
      \ , manager   INTEGER NOT NULL DEFAULT FALSE          \
      \ , PRIMARY KEY (board_uri, username)                 \
      \ , FOREIGN KEY (board_uri) REFERENCES board (uri)    \
      \ , FOREIGN KEY (username) REFERENCES user (username) \
      \ )"
