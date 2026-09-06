{-# LANGUAGE OverloadedStrings #-}

module Phi.Files where

import           Control.Exception.Safe (SomeException, try)
import           Control.Monad (void)
import           Data.Maybe (isJust, isNothing)

import           Crypto.Hash (Digest)
import           Crypto.Hash.Conduit (hashFile)
import           Crypto.Hash.Algorithms (SHA3_256)
import           Data.ByteString (hGet)
import           Data.Text (Text)
import qualified Web.Fn as Fn (File(..))
import qualified Data.Text as T (dropEnd, isPrefixOf, isSuffixOf, pack, unpack)
import qualified Web.Fn as Fn (File(..))
import           System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import           System.IO (hFileSize, IOMode(ReadMode), withBinaryFile)

import qualified Web.Fn as Fn (File(..))

import           Phi.Context (Context(..))
import           Phi.Database.Models (Banner(..), File(..))
import           Phi.Database.Queries.Types (FileRejected(..))
import           Phi.Files.Filetypes (getMimeAndExt)
import           Phi.Files.Thumbnails (makeThumbnails)

readableFilesize :: (Integral a, Show a) => a -> Text
readableFilesize n =
  case (amount, unit) of
    (1024, "KiB") -> "1 MiB"
    _             -> (strip . T.pack . show $ amount) <> " " <> unit
  where
    strip text
      | ".0" `T.isSuffixOf` text = T.dropEnd 2 text
      | otherwise                = text

    (unit, amount)
      | n < 1024   = ("B",              fromIntegral n)
      | n < 1024^2 = ("KiB", round' 0 $ fromIntegral n / 1024)
      | n < 1024^3 = ("MiB", round' 1 $ fromIntegral n / 1024^2)
      | otherwise  = ("B",              fromIntegral n)

    round' :: (RealFrac a, Fractional b, Show b) => Int -> a -> b
    round' digits n = fromIntegral (round $ n * 10^digits) / 10^digits

postMaxSize :: Int
postMaxSize = 8 * 1024^2

bannerMaxSize :: Int
bannerMaxSize = 512 * 1024

prepareForPost :: Context -> Fn.File -> IO (Either FileRejected File)
prepareForPost context fnFile =
  prepare context postMaxSize Nothing (\(filepath, hash_, ext_, size_, mime_) -> do
    -- Move the file from its temporary location to its permanent location.
    let location = static context <> "/file/" <> hash_ <> T.unpack ext_
    createDirectoryIfMissing True $ static context <> "/file"
    renameFile filepath location

    -- Create thumbnails
    thumbWidthHeight <-
      if shouldThumb mime_
      then makeThumbnails (canAnimate mime_) (canAlpha mime_) location (\ext' -> static context <> "/thumb/" <> hash_ <> ext')
      else pure Nothing

    pure $ File
      { hash = T.pack hash_
      , ext = ext_
      , size = size_
      , hasThumb = isJust thumbWidthHeight
      , thumbWidth = fst <$> thumbWidthHeight
      , thumbHeight = snd <$> thumbWidthHeight
      , mime = if isNothing thumbWidthHeight then toAudio mime_ else mime_
      , originalName = Fn.fileName fnFile
      -- ^ If thumbnailing failed and this is a video file, assume there's no
      -- video stream and this is actually an audio file. If thumbnailing
      -- fails for some other reason this will falsely trigger. It doesn't
      -- ever happen for video/x-m4v though, that always stays the same.
      }

    ) fnFile
  where
    shouldThumb :: Maybe Text -> Bool
    shouldThumb Nothing  = False
    shouldThumb (Just t) = "image/" `T.isPrefixOf` t || "video/" `T.isPrefixOf` t

    canAnimate :: Maybe Text -> Bool
    canAnimate Nothing  = False
    canAnimate (Just t) = "image/" `T.isPrefixOf` t

    canAlpha :: Maybe Text -> Bool
    canAlpha Nothing  = False
    canAlpha (Just t) = t `elem` ["image/png", "image/webp"]

    toAudio :: Maybe Text -> Maybe Text
    toAudio (Just "video/webm")      = Just "audio/webm"
    toAudio (Just "video/mp4")       = Just "audio/mp4"
    toAudio (Just "video/quicktime") = Just "audio/quicktime"
    toAudio mime_                    = mime_

prepareBanner :: Context -> Text -> Fn.File -> IO (Either FileRejected Banner)
prepareBanner context uri_ = do
  prepare context bannerMaxSize (Just ["image/png", "image/jpeg"]) $ \(filepath, hash_, ext_, _size, _mime) -> do
    -- Move the file from its temporary location to its permanent location.
    createDirectoryIfMissing True $ static context <> "/banner/" <> T.unpack uri_
    renameFile filepath $ static context <> "/banner/" <> T.unpack uri_ <> "/" <> hash_ <> T.unpack ext_

    pure $ Banner
      { bnBoardUri = uri_
      , bnHash = T.pack hash_
      , bnExt = ext_
      }

type FileElements =
  ( FilePath   -- temporary location
  , String     -- hash as a string
  , Text       -- ext
  , Int        -- size
  , Maybe Text -- mime
  )

prepare :: Context -> Int -> Maybe [Text] -> (FileElements -> IO a) -> Fn.File -> IO (Either FileRejected a)
prepare context maxsize mMimes continue fnFile =
  case Fn.fileName fnFile of
    "\"\"" -> pure $ Left FileMissing
    _      -> do
      -- Determine the file's MIME type and ext from its front 256 bytes (at most).
      (ext_, size_, mime_) <-
        withBinaryFile (Fn.filePath fnFile) ReadMode $ \handle -> do
          size_ <- fromIntegral <$> hFileSize handle
          front <- hGet handle 256
          let (mime_, ext_) = getMimeAndExt front
          pure $ (ext_, size_, mime_)

      if size_ > maxsize
      then pure $ Left $ FileTooLarge maxsize
      else
        let
          success = do
            hash_ <- makeHash
            Right <$> continue (Fn.filePath fnFile, hash_, ext_, size_, mime_)
          in
        case mMimes of
          Nothing    -> success
          Just mimes ->
            if maybe True (`elem` mimes) mime_
            then success
            else pure $ Left $ FileBadMime mimes
  where
    makeHash :: IO String
    makeHash = do
      digest <- hashFile (Fn.filePath fnFile) :: IO (Digest SHA3_256)
      pure $ show digest

deleteFile :: Context -> File -> IO ()
deleteFile context file =
  mapM_ (void . remove) $
    [ static context <> "/file/"  <> (T.unpack $ hash file) <> (T.unpack $ ext file)
    , static context <> "/thumb/" <> (T.unpack $ hash file) <> ".webp"
    , static context <> "/thumb/" <> (T.unpack $ hash file) <> ".jpg"
    , static context <> "/thumb/" <> (T.unpack $ hash file) <> ".png"
    ]
  where
    remove :: FilePath -> IO (Either SomeException ())
    remove = try . removeFile

deleteBanner :: Context -> Banner -> IO ()
deleteBanner context banner =
  void . remove $
    static context <> "/banner/"  <> (T.unpack $ bnBoardUri banner) <> "/" <> (T.unpack $ bnHash banner) <> (T.unpack $ bnExt banner)
  where
    remove :: FilePath -> IO (Either SomeException ())
    remove = try . removeFile
