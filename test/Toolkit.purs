module RpdTest.Toolkit
    ( spec ) where

import Prelude (Unit, pure, unit, ($))

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "toolkit" $ do
    it "works" $ do
        pure unit
