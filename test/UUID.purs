module Test.UUID where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.List ((:))
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

import Data.UUID as UUID

spec :: Spec Unit
spec = do

    describe "UUID" $ do

        it "works" $ liftEffect $ do

            uuid <- UUID.generate

            uuid `shouldEqual` "foo"

            pure unit
