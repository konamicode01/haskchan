module Common where

import Test.Hspec
import Test.Hspec.Fn

import Phi (phi)
import Phi.Context (Context, mkContext)

databaseFilePath :: FilePath
databaseFilePath = "" -- in-memory database; replace with "test.db" to inspect

staticFolder :: FilePath
staticFolder = "tmp/static/"

captchaFolder :: FilePath
captchaFolder = "tmp/captcha/"

fontFilePath :: FilePath
fontFilePath = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"

fnTests :: SpecWith (FnHspecState Context) -> Spec
fnTests fnSpecs = do
  fn (mkContext databaseFilePath "secret.bin" staticFolder captchaFolder fontFilePath)
    phi [] (const $ pure ()) fnSpecs
