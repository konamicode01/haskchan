module Phi.Database.Queries.Types where

import Data.Text (Text)

data NoSuchBoard = NoSuchBoard deriving (Eq, Show)
data ExtantBoard = ExtantBoard deriving (Eq, Show)
data ViewDisabled = ViewDisabled deriving (Eq, Show)

data NoSuchThread
  = DeletedThread
  | FutureThread
  deriving (Eq, Show)

data PermissionFail
  = PermissionFail
  deriving (Eq, Show)

data ReplyFail
  = LockedThread
  | FullThread
  deriving (Eq, Show)

data FileRejected
  = FileTooLarge Int
  | FileBadMime [Text]
  | FileMissing
  deriving (Eq, Show)

data NoSuchPost = NoSuchPost deriving (Eq, Show)

data LoginFail
  = NoSuchUser
  | WrongPassword
  deriving (Eq, Show)
data RegisterFail
  = ExtantUser
  | ClosedRegistration
  deriving (Eq, Show)

data Powerlevel
  = Admin
  | BoardOwner
  | BoardManager
  | BoardMod
  | Commoner
  deriving (Eq, Show)

instance Ord Powerlevel where
  Commoner     <= Commoner     = True
  Commoner     <= BoardMod     = True
  Commoner     <= BoardManager = True
  Commoner     <= BoardOwner   = True
  Commoner     <= Admin        = True

  BoardMod     <= BoardMod     = True
  BoardMod     <= BoardManager = True
  BoardMod     <= BoardOwner   = True
  BoardMod     <= Admin        = True

  BoardManager <= BoardManager = True
  BoardManager <= BoardOwner   = True
  BoardManager <= Admin        = True

  BoardOwner   <= BoardOwner   = True
  BoardOwner   <= Admin        = True

  Admin        <= Admin        = True

  (<=) _ _                     = False

data NoAuthority
  = UserNotFound
  | Forbidden
  deriving (Eq, Show)

data AddModFail
  = AlreadyAMod
  | NotAUser
  deriving (Eq, Show)

data DeleteBannersFail
  = NoSuchBanner
  | NoBanners
  deriving (Eq, Show)

data PostHasNoFile = PostHasNoFile deriving (Eq, Show)
