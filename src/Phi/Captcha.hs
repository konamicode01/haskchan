{-# LANGUAGE OverloadedStrings #-}

module Phi.Captcha where

import           Control.Exception.Safe (SomeException, try)
import           Control.Monad (filterM)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Directory (createDirectoryIfMissing, getFileSize, listDirectory, removeFile)

import qualified Data.ByteString as BS (foldr)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import           Data.ByteString.Builder (byteStringHex, toLazyByteString)
import           Data.ByteString.Base64 (decodeBase64, encodeBase64)
import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T (all, dropEnd, isSuffixOf, pack, replace, splitOn, toLower, unpack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8')

import           Crypto.Random (getRandomBytes)
import           System.Random (mkStdGen, randoms, StdGen)

import           Codec.Picture (Image, DynamicImage(ImageRGBA8), PixelRGBA8(..), saveJpgImage)
import           Graphics.Text.TrueType (Font, loadFontFile)
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture

import           Phi.Context (Context(captcha, font))
import           Phi.Database.Models (GlobalSettings(captchaBaseline))
import           Phi.Database.Queries (getGlobalSettings)

textGradient
  :: (Float, Float, Float, Float, Float, Float, Float, Float, Float)
  -> [(Float, PixelRGBA8)]
textGradient (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
  [ (0.2, PixelRGBA8 r1   g1   b1   0x8f)
  , (0.4, PixelRGBA8 0x38 0x30 0x30 0x7f)
  , (0.5, PixelRGBA8 r2   g2   b2   0x77)
  , (0.6, PixelRGBA8 0x30 0x38 0x38 0x7f)
  , (0.8, PixelRGBA8 r3   g3   b3   0x8f)
  ]
  where
    r1 = fromIntegral . floor $ f1 * 0x20 + 0x10
    g1 = fromIntegral . floor $ f2 * 0x20 + 0x10
    b1 = fromIntegral . floor $ f3 * 0x20 + 0x10
    r2 = fromIntegral . floor $ f4 * 0x40 + 0x20
    g2 = fromIntegral . floor $ f5 * 0x40 + 0x20
    b2 = fromIntegral . floor $ f6 * 0x40 + 0x20
    r3 = fromIntegral . floor $ f7 * 0x20 + 0x10
    g3 = fromIntegral . floor $ f8 * 0x20 + 0x10
    b3 = fromIntegral . floor $ f9 * 0x20 + 0x10

lineGradient :: [(Float, PixelRGBA8)]
lineGradient =
  [ (0.0, PixelRGBA8 0xc0 0x50 0x70 0xb0)
  , (0.4, PixelRGBA8 0xc0 0x50 0x70 0xd0)
  , (0.5, PixelRGBA8 0x60 0x60 0x60 0x90)
  , (0.6, PixelRGBA8 0xc0 0x50 0x70 0xd0)
  , (1.0, PixelRGBA8 0xc0 0x50 0x70 0xb0)
  ]

geometry :: [Float] -> [Path]
geometry floats =
  [ Path (V2 (-16) (-8)) True $ pathcommands 0
  , Path (V2 (-16)  56 ) True $ pathcommands 1
  , Path (V2  144  (-8)) True $ pathcommands 2
  , Path (V2  144   56 ) True $ pathcommands 3
  ]
  where
    pathcommands n =
      [ PathCubicBezierCurveTo (mkP $ n * 6 + 0) (mkP $ n * 6 + 1) (mkP $ n * 6 + 2)
      , PathCubicBezierCurveTo (mkP $ n * 6 + 3) (mkP $ n * 6 + 4) (mkP $ n * 6 + 5)
      ]
    mkP n = V2 (floats !! (n * 2 + 0) * 160 - 16) (floats !! (n * 2 + 1) * 64 - 8)

makeCaptcha :: Font -> StdGen -> Text -> Drawing PixelRGBA8 ()
makeCaptcha font_ gen text = do
  -- Left background circle
  withTexture (uniformTexture $ PixelRGBA8 0x00 0x86 0xc1 0x60) $
    fill $ circle (V2 36 24) 32

  -- Right background circle
  withTexture (uniformTexture $ PixelRGBA8 0xff 0xf4 0xc1 0x70) $
    fill $ circle (V2 92 24) 32

  -- Small background circle
  withTexture (uniformTexture $ PixelRGBA8 0x30 0x80 0x80 0xa0) $
    fill $ circle p1 r1

  -- Text
  withTexture (linearGradientTexture (textGradient floats') (V2 0 0) (V2 128 48)) $
    withPathOrientation path 0 $ printTextAt font_ (PointSize 32) (V2 0 0) (T.unpack text)

  -- Geometry
  withTexture (uniformTexture $ PixelRGBA8 0x50 0x40 0x20 0x60) $
    fillWithMethod FillEvenOdd $ geometry floats''

  -- Horizontal line
  withTexture (linearGradientTexture lineGradient (V2 0 0) (V2 128 48)) $
    stroke 2 JoinRound (CapRound, CapRound) $
      CubicBezier q1 q2 q3 q4

  -- Foreground circle
  withTexture (uniformTexture $ PixelRGBA8 0x60 0x50 0x70 0xa0) $
    fill $ circle p2 r2

  where
    floats = randoms gen :: [Float]

    p1 = V2 (floats !! 0 * 128)      (floats !! 1 * 48)
    p2 = V2 (floats !! 2 * 128)      (floats !! 3 * 48)
    r1 = floats !! 4 * 10 + 2
    r2 = floats !! 5 * 12 + 6

    q1 = V2 (floats !!  6 *  24 -  12) (floats !!  7 * 64 - 8)
    q2 = V2 (floats !!  8 * 160 -  16)  (floats !!  9 * 64 - 8)
    q3 = V2 (floats !! 10 * 160 -  16)  (floats !! 11 * 64 - 8)
    q4 = V2 (floats !! 12 *  24 + 116) (floats !! 13 * 64 - 8)

    path = Path p3 False [PathQuadraticBezierCurveTo p4 p5]
    p3 = V2 (floats !! 14 *  28 +  4) (floats !! 15 * 12 + 28)
    p4 = V2 (floats !! 16 *  16 + 56) (floats !! 17 * 24 + 24)
    p5 = V2                 128       (floats !! 18 * 20 + 28)

    floats'  = ( floats !! 19
               , floats !! 20
               , floats !! 21
               , floats !! 22
               , floats !! 23
               , floats !! 24
               , floats !! 25
               , floats !! 26
               , floats !! 27
               )
    floats'' = drop 28 floats

getRandomInt :: IO Int
getRandomInt = do
  bytes <- getRandomBytes 8
  pure $ BS.foldr (\word8 acc -> acc * 256 + fromIntegral word8) 0 bytes

makeRandomCaptcha :: Font -> IO (Maybe (Text, String, Image PixelRGBA8))
makeRandomCaptcha font_ = do
  seed <- getRandomInt
  bytes <- getRandomBytes 3
  let text = encodeBase64 bytes
  case makeKey text of
    Nothing  -> pure Nothing
    Just key -> pure $ Just (text, key, mkDrawing seed text)
  where
    mkDrawing seed text =
      renderDrawing 128 48 (PixelRGBA8 0xc0 0xe0 0xf0 0xff) $
          makeCaptcha font_ (mkStdGen seed) text

makeKey :: Text -> Maybe String
makeKey text =
  case decodeBase64 . encodeUtf8 $ T.replace "1" "l" . T.replace "o" "0" . T.toLower $ text of
    Left  _     -> Nothing
    Right bytes ->
      case decodeUtf8' . BSL.toStrict . toLazyByteString $ byteStringHex bytes of
        Left  _   -> Nothing
        Right key -> Just $ T.unpack key

mkFilename :: (Integral a, Show a) => a -> String -> FilePath
mkFilename expiry key = show expiry <> "_" <> key <> ".jpg"

makeAndSaveNewCaptcha :: Context -> IO (Maybe FilePath)
makeAndSaveNewCaptcha context = do
  eFont <- loadFontFile (font context)
  case eFont of
    Left  _    -> pure Nothing
    Right font_ -> do
      mCaptcha <- makeRandomCaptcha font_
      case mCaptcha of
        Nothing -> pure Nothing
        Just (_text, key, image) -> do
          expiry <- (+3600) <$> floor <$> getPOSIXTime
          let filename = mkFilename expiry key
          let filepath = captcha context <> "/" <> filename
          createDirectoryIfMissing True $ captcha context
          saveJpgImage 90 filepath (ImageRGBA8 image)
          pure $ Just filename

-- Safe version of (!!).
at :: [a] -> Int -> Maybe a
at xs n =
  case drop n xs of
    []    -> Nothing
    (x:_) -> Just x

getCaptcha :: Context -> IO (Maybe FilePath)
getCaptcha context = do
  filenames <- getAllCaptchas context Nothing
  if length filenames < 256
  then makeAndSaveNewCaptcha context
  else do
    index <- (`mod` length filenames) <$> getRandomInt
    pure $ filenames `at` index

getAllCaptchas :: Context -> Maybe Text -> IO [FilePath]
getAllCaptchas context mMatch = do
  allFilenames <- listDirectory $ captcha context
  filterM inspect allFilenames
  where
    inspect filename = do
      eSize <- try $ getFileSize $ captcha context <> "/" <> filename
        :: IO (Either SomeException Integer)
      case eSize of
        Left  _    -> pure False
        Right size ->
          if size == 0
          then delete filename
          else do
            case parse filename of
              Nothing -> delete filename
              Just (expiry, key) -> do
                time <- floor <$> getPOSIXTime
                if time >= expiry
                then delete filename
                else
                  case mMatch of
                    Nothing    -> pure True
                    Just match -> pure (makeKey match == Just key)
    delete filename = do
      _ <- try $ removeFile $ captcha context <> "/" <> filename
        :: IO (Either SomeException ())
      pure False
    parse filename =
      case T.splitOn "_" $ T.pack filename of
        [expiryText, endText] ->
          case reads (T.unpack expiryText) of
            [(expiry, _)] ->
              if ".jpg" `T.isSuffixOf` endText
              then
                let key = T.dropEnd 4 endText in
                if T.all isHex key
                then Just (expiry, T.unpack key)
                else Nothing
              else Nothing
            _ -> Nothing
        _ -> Nothing
    isHex char =
      ord char >= 48 && ord char < 58 || ord char >= 97 && ord char < 103

checkCaptcha :: Context -> Text -> IO Bool
checkCaptcha context work = do
  filenames <- getAllCaptchas context (Just work)
  mapM_ delete filenames
  pure $ not . null $ filenames
  where
    delete filename = do
      _ <- try $ removeFile $ captcha context <> "/" <> filename
        :: IO (Either SomeException ())
      pure ()

enforceCaptchaIf :: Context -> Bool -> Text -> IO a -> IO a -> IO a
enforceCaptchaIf context condition work failure success
  | condition = do
    ok <- checkCaptcha context work
    if ok then success else failure
  | otherwise = success

enforceCaptcha :: Context -> Text -> IO a -> IO a -> IO a
enforceCaptcha context = enforceCaptchaIf context True

shouldEnforceCaptchaForBoard :: Context -> Text -> IO (Maybe Bool)
shouldEnforceCaptchaForBoard context uri_ = do
  globalsettings <- getGlobalSettings context
  pure $ captchaBaseline <$> globalsettings

enforceCaptchaForBoard :: Context -> Text -> Maybe Text -> (Bool -> IO a) -> IO a -> IO a
enforceCaptchaForBoard context uri_ mWork failure' success = do
  mBool <- shouldEnforceCaptchaForBoard context uri_
  case mBool of
    Nothing    -> failure' False
    Just False -> success
    Just True  ->
      case mWork of
        Nothing   -> failure' True
        Just work -> enforceCaptcha context work (failure' True) success
