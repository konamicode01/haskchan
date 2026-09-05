{-# LANGUAGE OverloadedStrings #-}

module Phi.Captchouli
  ( getCaptchouli
  , makeAndSaveNewCaptchouli
  , checkCaptchouli
  ) where

import           Control.Exception.Safe (SomeException, try)
import           Control.Monad (filterM)
import           Data.Char (toLower)
import           Data.List (isSuffixOf, stripPrefix)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (byteStringHex, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           System.Directory
  ( createDirectoryIfMissing
  , getFileSize
  , listDirectory
  , removeFile
  )
import           System.Random (mkStdGen, randomRs)

import           Codec.Picture
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           Graphics.Text.TrueType
import           Crypto.Random (getRandomBytes)

import           Data.Pool (withResource)
import qualified Database.SQLite.Simple as DB

import           Phi.Context (Context(captcha, db, font))

captchouliWidth :: Int
captchouliWidth = 360

captchouliHeight :: Int
captchouliHeight = 140

captchouliExpiry :: Integer
captchouliExpiry = 600

challengeAlphabet :: [Char]
challengeAlphabet = "23456789abcdefghijkmnpqrstuvwxyz"

randomAnswer :: IO String
randomAnswer = do
  bytes <- getRandomBytes 16
  let indexes =
        map (\b -> fromIntegral b `mod` length challengeAlphabet)
            (BS.unpack bytes)
  pure $ take 6 [challengeAlphabet !! i | i <- indexes]

randomToken :: IO Text
randomToken = do
  bytes <- getRandomBytes 16
  pure $ decodeUtf8
       $ toStrict
       $ toLazyByteString
       $ byteStringHex bytes

randomSeed :: IO Int
randomSeed = do
  bytes <- getRandomBytes 8
  pure $ BS.foldr (\b n -> n * 256 + fromIntegral b) 0 bytes

mkFilename :: Text -> FilePath
mkFilename token =
  "captchouli_" <> T.unpack token <> ".jpg"

captchaWidth :: Int
captchaWidth = 360

captchaHeight :: Int
captchaHeight = 140

girlsDirectory :: Context -> FilePath
girlsDirectory context = captcha context <> "/girls"

supportedGirlImage :: FilePath -> Bool
supportedGirlImage filename =
     ".jpg"  `isSuffixOf` lower
  || ".jpeg" `isSuffixOf` lower
  || ".png"  `isSuffixOf` lower
  || ".webp" `isSuffixOf` lower
  where
    lower = map toLower filename

randomGirl :: Context -> IO (Maybe (Image PixelRGBA8))
randomGirl context = do
  createDirectoryIfMissing True (girlsDirectory context)

  names <- listDirectory (girlsDirectory context)

  let files =
        [ girlsDirectory context <> "/" <> name
        | name <- names
        , supportedGirlImage name
        ]

  case files of
    [] -> pure Nothing

    _ -> do
      seed <- randomSeed
      let filepath = files !! (seed `mod` length files)

      result <- try (readImage filepath)
        :: IO (Either SomeException (Either String DynamicImage))

      case result of
        Left _ -> pure Nothing
        Right (Left _) -> pure Nothing
        Right (Right image) ->
          pure $ Just (convertRGBA8 image)

fitGirl :: Image PixelRGBA8 -> Image PixelRGBA8
fitGirl source =
  generateImage pixel captchaWidth captchaHeight
  where
    sw = max 1 (imageWidth source)
    sh = max 1 (imageHeight source)

    sx =
      fromIntegral captchaWidth / fromIntegral sw

    sy =
      fromIntegral captchaHeight / fromIntegral sh

    scale = max sx sy

    scaledWidth =
      max 1 (round (fromIntegral sw * scale))

    scaledHeight =
      max 1 (round (fromIntegral sh * scale))

    cropX =
      max 0 ((scaledWidth - captchaWidth) `div` 2)

    cropY =
      max 0 ((scaledHeight - captchaHeight) `div` 2)

    pixel x y =
      let sourceX =
            min (sw - 1)
              (max 0
                (floor
                  ((fromIntegral (x + cropX) + 0.5) / scale)))

          sourceY =
            min (sh - 1)
              (max 0
                (floor
                  ((fromIntegral (y + cropY) + 0.5) / scale)))

      in pixelAt source sourceX sourceY

blendPixel :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blendPixel
  (PixelRGBA8 br bg bb ba)
  (PixelRGBA8 fr fg fb fa) =
    PixelRGBA8
      (blend br fr)
      (blend bg fg)
      (blend bb fb)
      (fromIntegral outA)
  where
    aFront = fromIntegral fa :: Int
    aBack  = fromIntegral ba :: Int

    outA =
      aFront + (aBack * (255 - aFront) `div` 255)

    blend base front =
      if outA <= 0
      then 0
      else
        fromIntegral
          ((fromIntegral front * aFront
            + fromIntegral base
                * aBack
                * (255 - aFront)
                `div` 255)
              `div` outA)

makeImage
  :: Font
  -> Int
  -> String
  -> Maybe (Image PixelRGBA8)
  -> Image PixelRGBA8
makeImage font_ seed answer mGirl =
  let background =
        case mGirl of
          Nothing ->
            generateImage
              (\_ _ -> PixelRGBA8 0x30 0x20 0x40 0xff)
              captchaWidth
              captchaHeight

          Just girl ->
            fitGirl girl

      overlay =
        renderDrawing
          captchaWidth
          captchaHeight
          (PixelRGBA8 0 0 0 0)
          (drawing font_ seed answer)

  in generateImage
       (\x y ->
          blendPixel
            (pixelAt background x y)
            (pixelAt overlay x y))
       captchaWidth
       captchaHeight

drawing :: Font -> Int -> String -> Drawing PixelRGBA8 ()
drawing font_ seed answer = do
  let gen = mkStdGen seed
      rs = randomRs (0 :: Int, 255) gen

  -- Transparent dark veil. The anime artwork remains visible underneath.
  withTexture
    (uniformTexture (PixelRGBA8 8 8 16 0x30))
    (fill $ rectangle
      (V2 0 0)
      (fromIntegral captchaWidth)
      (fromIntegral captchaHeight))

  -- Dark translucent text panel.
  withTexture
    (uniformTexture (PixelRGBA8 8 8 16 0xa8))
    (fill $ rectangle
      (V2 24 30)
      312
      80)

  -- Distortion lines.
  sequence_
    [ withTexture
        (uniformTexture
          (PixelRGBA8
            (fromIntegral (rs !! n))
            (fromIntegral (rs !! (n + 1)))
            (fromIntegral (rs !! (n + 2)))
            0x78))
        (stroke 2 JoinRound (CapRound, CapRound)
          (line
            (V2
              (fromIntegral (rs !! (n + 3)))
              (fromIntegral (rs !! (n + 4))))
            (V2
              (fromIntegral (rs !! (n + 5)))
              (fromIntegral (rs !! (n + 6))))))
    | n <- [0,7..70]
    ]

  -- Decorative circles.
  sequence_
    [ withTexture
        (uniformTexture (PixelRGBA8 0xff 0xff 0xff 0x20))
        (fill $ circle
          (V2
            (fromIntegral (rs !! n))
            (fromIntegral (rs !! (n + 1))))
          (fromIntegral (15 + rs !! (n + 2) `mod` 30)))
    | n <- [80,83..98]
    ]

  -- CAPTCHA answer.
  withTexture
    (uniformTexture (PixelRGBA8 0xff 0xf0 0xff 0xff))
    (printTextAt
      font_
      (PointSize 50)
      (V2 48 86)
      answer)

  -- Speckles.
  sequence_
    [ withTexture
        (uniformTexture (PixelRGBA8 0xff 0xff 0xff 0x78))
        (fill $ circle
          (V2
            (fromIntegral (rs !! n))
            (fromIntegral (rs !! (n + 1))))
          2)
    | n <- [100,102..150]
    ]

makeAndSaveNewCaptchouli :: Context -> IO (Maybe FilePath)
makeAndSaveNewCaptchouli context = do
  eFont <- loadFontFile (font context)
  case eFont of
    Left _ -> pure Nothing
    Right font_ -> do
      answer <- randomAnswer
      token <- randomToken
      seed <- randomSeed
      expiry <- (+ captchouliExpiry) . floor <$> getPOSIXTime

      mGirl <- randomGirl context

      let filename = mkFilename token
          filepath = captcha context <> "/" <> filename
          image = makeImage font_ seed answer mGirl

      createDirectoryIfMissing True (captcha context)

      result <- try $
        saveJpgImage 90 filepath (ImageRGBA8 image)
        :: IO (Either SomeException ())

      case result of
        Left _ -> pure Nothing
        Right _ -> do
          dbResult <- try
            (withResource (db context) $ \conn ->
              DB.execute conn
                "INSERT INTO captchouli_challenge (token, answer, expiry) VALUES (?, ?, ?)"
                (token, T.pack answer, expiry)
            )
            :: IO (Either SomeException ())

          case dbResult of
            Left _ -> do
              _ <- try (removeFile filepath)
                :: IO (Either SomeException ())
              pure Nothing
            Right _ ->
              pure $ Just filename

cleanupExpired :: Context -> IO ()
cleanupExpired context = do
  now <- (floor <$> getPOSIXTime) :: IO Integer

  expired <- withResource (db context) $ \conn ->
    DB.query conn
      "SELECT token FROM captchouli_challenge WHERE expiry <= ?"
      (DB.Only now) :: IO [DB.Only Text]

  mapM_ (deleteChallenge context) expired

deleteChallenge :: Context -> DB.Only Text -> IO ()
deleteChallenge context (DB.Only token) = do
  let filename = mkFilename token

  _ <- try $
    removeFile (captcha context <> "/" <> filename)
    :: IO (Either SomeException ())

  withResource (db context) $ \conn ->
    DB.execute conn
      "DELETE FROM captchouli_challenge WHERE token = ?"
      (DB.Only token)

getCaptchouli :: Context -> IO (Maybe FilePath)
getCaptchouli context = do
  cleanupExpired context

  tokens <- withResource (db context) $ \conn ->
    DB.query_ conn
      "SELECT token FROM captchouli_challenge ORDER BY expiry DESC"
      :: IO [DB.Only Text]

  existing <- filterM
    (\(DB.Only token) -> do
        let filepath = captcha context <> "/" <> mkFilename token
        eSize <- try (getFileSize filepath)
          :: IO (Either SomeException Integer)
        pure $ case eSize of
          Right size -> size > 0
          Left _ -> False)
    tokens

  case existing of
    [] ->
      makeAndSaveNewCaptchouli context
    _ -> do
      seed <- randomSeed
      let DB.Only token = existing !! (seed `mod` length existing)
      pure $ Just (mkFilename token)

checkCaptchouli :: Context -> Text -> IO Bool
checkCaptchouli context answer = do
  cleanupExpired context
  now <- (floor <$> getPOSIXTime) :: IO Integer

  let supplied = T.toLower $ T.strip answer

  matches <- withResource (db context) $ \conn ->
    DB.query conn
      "SELECT token FROM captchouli_challenge WHERE lower(answer) = ? AND expiry > ? LIMIT 1"
      (supplied, now)
      :: IO [DB.Only Text]

  case matches of
    [] -> pure False
    (tokenRow:_) -> do
      deleteChallenge context tokenRow
      pure True

parseToken :: FilePath -> Maybe Text
parseToken filename =
  case stripPrefix "captchouli_" filename of
    Just rest
      | ".jpg" `isSuffixOf` rest ->
          let token = take (length rest - 4) rest
          in if validToken token
               then Just (T.pack token)
               else Nothing
    _ -> Nothing

validToken :: String -> Bool
validToken token =
  length token == 32 &&
  all (\c -> c `elem` ("0123456789abcdef" :: String) ||
             toLower c `elem` ("0123456789abcdef" :: String))
      token
