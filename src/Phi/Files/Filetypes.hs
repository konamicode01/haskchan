{-# LANGUAGE OverloadedStrings #-}

module Phi.Files.Filetypes where

import           Data.Binary.Get (getWord32be, runGetOrFail)
import           Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as BS (drop, length, take)
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')

isIsobmff :: ByteString -> Bool
isIsobmff front =
 "ftyp" `isPrefixOf` BS.drop 4 front && boxheaderSizeConstraint
  where
    boxheaderSizeConstraint =
      case isobmffBoxheaderSize front of
        Nothing -> False
        Just n  -> BS.length front >= n

isobmffBoxheaderSize :: Integral a => ByteString -> Maybe a
isobmffBoxheaderSize front =
  case runGetOrFail getWord32be $ BSL.fromStrict $ BS.take 4 front of
    Left _          -> Nothing
    Right (_, _, n) -> Just $ fromIntegral n

isobmffMajorBrand :: ByteString -> ByteString
isobmffMajorBrand = BS.take 4 . BS.drop 8

isobmffCompatibleBrands :: ByteString -> [ByteString]
isobmffCompatibleBrands front =
  case isobmffBoxheaderSize front of
    Nothing            -> []
    Just boxheaderSize -> [ BS.take 4 . BS.drop n $ front
                          | n <- [16, 20 .. min (BS.length front - 1) (boxheaderSize -  1)]
                          ]

isMp4 :: ByteString -> Bool
isMp4 front =
  (isIsobmff front &&) $
    any (`elem` brands) (isobmffCompatibleBrands front)
    || isobmffMajorBrand front `elem` brands
  where
    brands = ["mp41", "mp42", "isom"]

isMov :: ByteString -> Bool
isMov front = isIsobmff front && isobmffMajorBrand front == "qt  "

isM4a :: ByteString -> Bool
isM4a front = "M4A " `isPrefixOf` front || "ftypM4A" `isPrefixOf` (BS.drop 4 front)

isAac :: ByteString -> Bool
isAac front = "\xff\xf1" `isPrefixOf` front || "\xff\xf9" `isPrefixOf` front

isMp3 :: ByteString -> Bool
isMp3 front =
  any (`isPrefixOf` front) $
    [ "ID3"
    , "\xff\xf2"
    , "\xff\xf3"
    , "\xff\xfb"
    ]

isWebp :: ByteString -> Bool
isWebp front = "RIFF" `isPrefixOf` front && "WEBPVP8" `isPrefixOf` BS.drop 8 front

isPdf :: ByteString -> Bool
isPdf = isPrefixOf "%PDF-"

isUtf8 :: ByteString -> Bool
isUtf8 front =
  case decodeUtf8' front of
    Left  _ -> False
    Right _ -> True

type Filetype = (Text, Text)

filetypes :: [(Filetype, ByteString -> Bool)]
filetypes =
  [ (("application/pdf", ".pdf"),  isPdf)
  , (("text/plain", ".txt"),      isUtf8)
  , (("image/jpeg", ".jpg"),      isPrefixOf "\xff\xd8\xff")
  , (("image/png", ".png"),       isPrefixOf "\x89PNG")
  , (("image/gif", ".gif"),       isPrefixOf "GIF")
  , (("image/webp", ".webp"),     isWebp)
  , (("video/webm", ".webm"),     isPrefixOf "\x1a\x45\xdf\xa3")
  , (("video/mp4", ".mp4"),       isMp4)
  , (("video/quicktime", ".mov"), isMov)
  , (("video/x-m4v", ".m4v"),     isPrefixOf "\x00\x00\x00\x1cftypM4V")
  , (("audio/ogg", ".ogg"),       isPrefixOf "OggS")
  , (("audio/flac", ".flac"),     isPrefixOf "fLaC")
  , (("audio/mp4", ".m4a"),       isM4a)
  , (("audio/aac", ".aac"),       isAac)
  , (("audio/mpeg", ".mp3"),      isMp3)
  ]

getMimeAndExt :: ByteString -> (Maybe Text, Text)
getMimeAndExt front =
  case lookup True $ map transform filetypes of
    Nothing            -> (Nothing, ".bin")
    Just (mime_, ext_) -> (Just mime_, ext_)
  where
    transform (filetype, check) = (check front, filetype)
