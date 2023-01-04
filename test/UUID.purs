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
import Noodle.Id as N

spec :: Spec Unit
spec = do

    describe "UUID" $ do

        it "works" $ liftEffect $ do

            uuid <- UUID.generate

            UUID.toString uuid `shouldEqual` "foo"

            pure unit


    describe "Node ID" $ do

        it "also works" $ liftEffect $ do

            nodeId <- N.makeNodeId $ N.family' (N.Family :: N.Family "foo")

            N.reflect' nodeId `shouldEqual` "foo"

            pure unit
