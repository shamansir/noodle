module Test.Layouts where

import Prelude

import Data.Layout.Ordered as O

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Vec2 ((<+>))

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


spec :: Spec Unit
spec = do
    describe "Ordered layout" do
        it "one item poulates all the space" $ do
            let layout = O.make [ O.auto /\ [ O.auto /\ unit ] ]
            (O.sizeOf unit $ O.fit (20.0 <+> 30.0) layout) `shouldEqual` (Just $ 20.0 <+> 30.0)
