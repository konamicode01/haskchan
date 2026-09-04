module Main where

import Network.Wai.Handler.Warp
  ( defaultSettings
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

      settings = setPort 443 defaultSettings

  runTLS tls settings app

