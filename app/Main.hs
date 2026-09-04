module Main where

import Control.Concurrent (forkIO)
import Data.String (fromString)

import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setHost
  , setPort
  )

import Network.Wai.Handler.WarpTLS
  ( runTLS
  , tlsSettings
  )

import Phi (phi)
import Phi.Context (Config(..), mkContext)

font :: FilePath
font = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"

config :: Config
config = Config
  { databaseFile = "phi.db"
  , secretFile = "secret.bin"
  , staticFolder = "static/"
  , captchaFolder = "captcha/"
  , fontFile = font
  }

main :: IO ()
main = do
  context <- mkContext config
  app <- phi context

  let tls = tlsSettings
        "certs/origin.crt"
        "certs/origin.key"

      httpsSettings =
        setPort 443 defaultSettings

      onionSettings =
        setHost (fromString "127.0.0.1")
        $ setPort 7000 defaultSettings

  _ <- forkIO $ runSettings onionSettings app

  runTLS tls httpsSettings app
