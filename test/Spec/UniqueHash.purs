module Test.Spec.UniqueHash where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.List ((:))
import Data.List as List
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

import Data.UniqueHash as UniqueHash
import Noodle.Id as N
import Data.SProxy (reflect')
import Effect.Console (log) as Console

spec :: Spec Unit
spec = do

    describe "UniqueHash" $ do

        it "works" $ liftEffect $ do

            uuid <- UniqueHash.generate

            (String.length (UniqueHash.toString uuid) > 0) `shouldEqual` true
            (String.length (UniqueHash.toString uuid) <= 40) `shouldEqual` true

            pure unit


    describe "Node ID" $ do

        it "also works" $ liftEffect $ do

            nodeId <- N.makeNodeId $ N.family' (N.Family :: N.Family "foo")

            String.take 5 (reflect' nodeId) `shouldEqual` "foo::"
            (String.length (reflect' nodeId) > 0) `shouldEqual` true
            (String.length (reflect' nodeId) <= 40) `shouldEqual` true

            pure unit
