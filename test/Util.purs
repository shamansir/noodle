module RpdTest.Util
    ( spec
    ) where

import Prelude

import Effect.Class (liftEffect)

import Data.String as String

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


import Rpd.UUID as UUID


spec :: Spec Unit
spec =
  describe "UUID generation" do
    it "does what it says" do
      uuid <- liftEffect $ UUID.new
      _ <- 36 `shouldEqual` String.length (UUID.toRawString uuid)
      pure unit
