module FileSpec where

import           Test.Hspec hiding (context)
import           Test.Hspec.Fn

import Common (fnTests)

spec :: Spec
spec = fnTests $ pure ()
