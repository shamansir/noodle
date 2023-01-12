module Test.Layouts where

import Prelude

import Web.App.Layout.Flex as O

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Vec2 ((<+>))

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


spec :: Spec Unit
spec = do
    describe "Flex layout" do

        it "one item poulates all the space" $ do
            let layout = O.make [ O.auto /\ [ O.auto /\ unit ] ]
            (O.sizeOf unit $ O.fit (20.0 <+> 30.0) layout) `shouldEqual` (Just $ 20.0 <+> 30.0)

        it "two auto-items share the space" $ do
            let
                layout = O.make
                            [ O.auto /\ [ O.auto /\ "one" ]
                            , O.auto /\ [ O.auto /\ "two" ]
                            ]
                sized = O.fit (20.0 <+> 30.0) layout
            (O.sizeOf "one" $ sized) `shouldEqual` (Just $ 20.0 <+> 15.0)
            (O.sizeOf "two" $ sized) `shouldEqual` (Just $ 20.0 <+> 15.0)

        it "two auto-items share the space, horizontal case" $ do
            let
                layout = O.make
                            [ O.auto /\
                                [ O.auto /\ "one", O.auto /\ "two" ]
                            ]
                sized = O.fit (20.0 <+> 30.0) layout
            (O.sizeOf "one" $ sized) `shouldEqual` (Just $ 10.0 <+> 30.0)
            (O.sizeOf "two" $ sized) `shouldEqual` (Just $ 10.0 <+> 30.0)

        it "percentage forces auto items to shrink" $ do
            let
                layout = O.make
                            [ O.auto /\
                                [ O.percents 75 /\ "one", O.auto /\ "two" ]
                            ]
                sized = O.fit (20.0 <+> 30.0) layout
            (O.sizeOf "one" $ sized) `shouldEqual` (Just $ 15.0 <+> 30.0)
            (O.sizeOf "two" $ sized) `shouldEqual` (Just $ 5.0 <+> 30.0)
            let
                layout' = O.make
                            [ O.auto /\
                                [ O.auto /\ "one", O.percents 75 /\ "two" ]
                            ]
                sized' = O.fit (20.0 <+> 30.0) layout'
            (O.sizeOf "one" $ sized') `shouldEqual` (Just $ 5.0 <+> 30.0)
            (O.sizeOf "two" $ sized') `shouldEqual` (Just $ 15.0 <+> 30.0)

        it "fixed values force auto items to shrink" $ do
            let
                layout = O.make
                            [ O.auto /\
                                [ O.fixed 14.0 /\ "one", O.auto /\ "two" ]
                            ]
                sized = O.fit (20.0 <+> 30.0) layout
            (O.sizeOf "one" $ sized) `shouldEqual` (Just $ 14.0 <+> 30.0)
            (O.sizeOf "two" $ sized) `shouldEqual` (Just $ 6.0 <+> 30.0)
            let
                layout' = O.make
                            [ O.auto /\
                                [ O.auto /\ "one", O.fixed 14.0 /\ "two" ]
                            ]
                sized' = O.fit (20.0 <+> 30.0) layout'
            (O.sizeOf "one" $ sized') `shouldEqual` (Just $ 6.0 <+> 30.0)
            (O.sizeOf "two" $ sized') `shouldEqual` (Just $ 14.0 <+> 30.0)

        pending "folding with position works"

        pending "searching for items works"