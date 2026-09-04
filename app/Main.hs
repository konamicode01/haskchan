module Main where

import Network.Wai.Handler.Warp (run)
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
  run 7000 app
