module Test.Spec.Trees where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.List ((:))
import Data.List as List
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)

import Data.UniqueHash as UniqueHash
import Noodle.Id as N
import Effect.Console (log) as Console

spec :: Spec Unit
spec = do

    describe "Trees" $ do

        pending' "building trees from network" $ liftEffect $ do

            pure unit