module Test.Spec.UI.Hyrule where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn, fail)


spec :: Spec Unit
spec = do

    describe "hyrule" $ do
        pure unit