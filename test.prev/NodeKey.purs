module Test.NodeKey where

import Prelude

import Effect.Console as Console
import Effect.Class (liftEffect)

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)

import Data.Maybe (Maybe(..))

import Blessed.Internal.NodeKey as NK


spec :: Spec Unit
spec = do

    describe "chaining keys" $ do

        it "properly creates chains" $ do
            (NK.getN <$> NK.chain 3) `shouldEqual` [ Just 0, Just 1, Just 2 ]

        it "properly continues chains" $ do
            (NK.getN <$> NK.continue (NK.next $ NK.next $ NK.first) 3) `shouldEqual` [ Just 2, Just 3, Just 4 ]

        it "properly nests chains" $ do
            (NK.getN <$> NK.nestChain (NK.next $ NK.next $ NK.first) 3) `shouldEqual` [ Just 2000, Just 2001, Just 2002 ]
