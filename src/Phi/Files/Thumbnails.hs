module Phi.Files.Thumbnails where

import Data.Tuple (swap)

import Control.Exception.Safe (try, SomeException)
import System.Directory (removeFile)
import System.Process (callProcess)

import Codec.FFmpeg as FF
import Codec.Picture
  ( Image(..)
  , pixelAt
  , pixelOpacity
  , PixelRGBA8
  , DynamicImage(ImageRGBA8)
  , savePngImage
  )

type Pixel = PixelRGBA8
type Frame = Image Pixel

type Animate = IO (Maybe Frame)

shrink :: Integral a => a -> a -> a -> (a, a)
shrink _maxlength 0 0 = (0, 0)
shrink maxlength width height
  | width < height     = swap $ shrink maxlength height width
  | maxlength >= width = (width, height)
  | otherwise          = (maxlength, newheight)
  where
    newheight = round $ fromIntegral (height * maxlength) / fromIntegral width

makeThumbnails :: Bool -> Bool -> FilePath -> (String -> FilePath) -> IO (Maybe (Int, Int))
makeThumbnails _canAnimate canAlpha src mkDst = do
  initFFmpeg
  setLogLevel avLogDebug

  eImage <- try $ imageReader (FF.File src)
    :: IO (Either SomeException (IO (Maybe Frame), IO ()))

  mWidthHeight <- case eImage of
    Left _ -> pure Nothing

    Right (getFrame, cleanup) -> do
      mFrame <- getFrame

      case mFrame of
        Nothing -> do
          cleanup
          pure Nothing

        Just frame -> do
          let (width, height) =
                shrink 192 (imageWidth frame) (imageHeight frame)

              alpha =
                canAlpha && isTransparent frame

          writeWebp frame width height (mkDst ".webp")

          if alpha
            then do
              cleanup

            else do
              writeJpegImage frame width height (mkDst ".jpg")
              cleanup

          pure $ Just (width, height)

  pure mWidthHeight

  where
    isTransparent :: Frame -> Bool
    isTransparent frame =
      any (/= 0xff) $
        map pixelOpacity
          (take 1048576 (pixels frame))

    pixels :: Frame -> [Pixel]
    pixels frame = do
      x <- [0 .. imageWidth frame - 1]
      y <- [0 .. imageHeight frame - 1]
      pure $ pixelAt frame x y

writeWebp :: Frame -> Int -> Int -> FilePath -> IO ()
writeWebp frame width height dst = do
  let tmp = dst ++ ".input.png"

  savePngImage tmp (ImageRGBA8 frame)

  callProcess
    "convert"
    [ tmp
    , "-thumbnail"
    , show width ++ "x" ++ show height ++ ">"
    , dst
    ]

  removeFile tmp

writeJpegImage :: Frame -> Int -> Int -> FilePath -> IO ()
writeJpegImage frame width height dst = do
  let tmp = dst ++ ".input.png"

  savePngImage tmp (ImageRGBA8 frame)

  callProcess
    "convert"
    [ tmp
    , "-thumbnail"
    , show width ++ "x" ++ show height ++ ">"
    , "-quality"
    , "70"
    , dst
    ]

  removeFile tmp
