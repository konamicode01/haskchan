module Phi.Files.Thumbnails where

import Data.Maybe (isJust)
import Data.Tuple (swap)

import Control.Exception.Safe (try, SomeException)

import Codec.FFmpeg as FF
import Codec.Picture (Image(..), pixelAt, pixelOpacity, PixelRGBA8, DynamicImage(ImageRGBA8), saveJpgImage)

type Pixel = PixelRGBA8
type Frame = Image Pixel

type Animate = IO (Maybe Frame)

shrink :: Integral a => a -> a -> a -> (a, a)
shrink _maxlength 0 0 = (0, 0)
shrink  maxlength width height
  | width < height     = swap $ shrink maxlength height width
  | maxlength >= width = (width, height)
  | otherwise          = (maxlength, newheight)
  where
    newheight = round $ fromIntegral (height * maxlength) / fromIntegral width

makeThumbnails :: Bool -> Bool -> FilePath -> (String -> FilePath) -> IO (Maybe (Int, Int))
makeThumbnails canAnimate canAlpha src mkDst = do
  initFFmpeg
  setLogLevel avLogDebug
  eImage <- try $ imageReader (FF.File src) :: IO (Either SomeException (IO (Maybe Frame), IO ()))

  mWidthHeight <- case eImage of
    Left  _ -> pure Nothing
    Right (getFrame, cleanup) -> do
      mFrame <- getFrame
      case mFrame of
        Nothing    -> pure Nothing
        Just frame -> do
          let (width, height) = shrink 192 (imageWidth frame) (imageHeight frame)
              -- Animate when possible.
              --animate = if canAnimate then getFrame else pure Nothing
              -- Never animate.
              animate = pure Nothing
              alpha = canAlpha && isTransparent frame

          writeWebp animate alpha frame width height (mkDst ".webp")
          if alpha
          then do
            writePng alpha frame width height (mkDst ".png")
            cleanup
          else do
            -- JPEG encoding with ffmpeg-light is broken, so this sidesteps the
            -- problem by reading the just-encoded WebP and saving it as a JPEG
            -- with saveJpgImage. This is not the best.
            cleanup
            (getFrame', cleanup') <- imageReader (FF.File $ mkDst ".webp")
            mFrame' <- getFrame'
            case mFrame' of
              Nothing     -> pure ()
              Just frame' -> saveJpgImage 70 (mkDst ".jpg") (ImageRGBA8 frame')
            cleanup'

          pure $ Just (width, height)

  pure mWidthHeight

  where
    isTransparent :: Frame -> Bool
    isTransparent frame =
      any (/= 0xff) $ map pixelOpacity (take 1048576 $ pixels frame)

    pixels :: Frame -> [Pixel]
    pixels frame = do
      x <- [0 .. imageWidth frame - 1]
      y <- [0 .. imageHeight frame - 1]
      pure $ pixelAt frame x y

-- JPEG encoding is broken. This will generate an exception at runtime.
writeJpeg :: Frame -> Int -> Int -> FilePath -> IO ()
writeJpeg frame width height dst = do
  putFrame <- imageWriter params dst
  writeSingleImage putFrame frame
  where
    params = EncodingParams
      { epWidth       = fromIntegral width
      , epHeight      = fromIntegral height
      , epFps         = 25
      , epCodec       = Just avCodecIdMjpeg
      , epPixelFormat = pixFmt
      , epPreset      = ""
      , epFormatName  = Just "mjpeg"
      }
    pixFmt = Nothing

writePng :: Bool -> Frame -> Int -> Int -> FilePath -> IO ()
writePng alpha frame width height dst = do
  putFrame <- imageWriter params dst
  writeSingleImage putFrame frame
  where
    params = EncodingParams
      { epWidth       = fromIntegral width
      , epHeight      = fromIntegral height
      , epFps         = 25
      , epCodec       = Just avCodecIdPng
      , epPixelFormat = Just pixFmt
      , epPreset      = ""
      , epFormatName  = Just "image2"
      }
    pixFmt
      | alpha     = avPixFmtRgba
      | otherwise = avPixFmtRgb24

writeWebp :: Animate -> Bool -> Frame -> Int -> Int -> FilePath -> IO ()
writeWebp getFrame alpha frame width height dst = do
  putFrame <- imageWriter params dst
  writeImage getFrame putFrame frame
  where
    params = EncodingParams
      { epWidth       = fromIntegral width
      , epHeight      = fromIntegral height
      , epFps         = 100
      , epCodec       = Just avCodecIdWebp
      , epPixelFormat = Just pixFmt
      , epPreset      = ""
      , epFormatName  = Just "webp"
      }
    pixFmt
      | alpha     = avPixFmtBgra
      | otherwise = avPixFmtYuv420p

{- TODO for animated thumbnails
  - fps needs to be detected and specified explicitly in EncodingParams
  - webp doesn't loop by default, `-loop 0` (infinite loop) needs to be set manually somehow
-}

writeSingleImage :: (Maybe Frame -> IO ()) -> Frame -> IO ()
writeSingleImage = writeImage $ pure Nothing

writeImage :: Animate -> (Maybe Frame -> IO ()) -> Frame -> IO ()
writeImage getFrame putFrame frame = do
  --print "writeImage"
  putFrame (Just frame)
  mFrame' <- getFrame
  case mFrame' of
    Nothing     -> putFrame Nothing
    Just frame' -> writeImage getFrame putFrame frame'
