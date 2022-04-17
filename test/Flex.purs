module Test.Flex where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console as Console

import Data.Tuple.Nested ((/\))
import Data.Traversable (sequence_)

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)


import App.Layout.Flex (Flex, flex, nest)
import App.Layout.Flex as Flex



testPlain :: Flex Int String
testPlain =
    flex [ 5 /\ "a", 10 /\ "f", 2 /\ "3" ]


testNested2 :: Flex Int String
testNested2 =
    nest [ 5 /\ flex [ 2 /\ "a" ], 10 /\ flex [ 3 /\ "b", 5 /\ "d" ], 2 /\ flex [ 4 /\ "c" ] ]


testNested :: Flex Int String
testNested =
    nest
        [ 5 /\ nest [ 2 /\ testNested2 ]
        , 10 /\ flex [ 3 /\ "b", 5 /\ "d"]
        , 7 /\ nest [ 0 /\ testPlain ]
        , 2 /\ flex [ 4 /\ "c" ]
        ]


testNestedMixed :: Flex Int String
testNestedMixed =
    nest
        [ 5 /\ flex [ 2 /\ "a"]
        , 10 /\ flex [ 3 /\ "b", 5 /\ "d" ]
        , 7 /\ nest [ 0 /\ testPlain ]
        , 2 /\ flex [ 4 /\ "c" ]
        ]


logFold path prev n str eff =
    eff <> (Console.log $ show path <> " --- " <> show prev <> " --- " <> show n <> " --- " <> show str)


logFoldN pos size str eff =
    eff <> (Console.log $ show pos <> " --- " <> show size <> " --- " <> show str)


spec :: Spec Unit
spec = do

    describe "flex" $ do

        describe "fold" $ do

            it "testPlain" $ do
                liftEffect $
                    Flex.fold
                        logFold
                        (pure unit)
                        testPlain

            it "testNested2" $ do
                liftEffect $
                    Flex.fold
                        logFold
                        (pure unit)
                        testNested2

        describe "foldN" $ do

            it "testPlain" $ do
                liftEffect $
                    Flex.foldN
                        logFoldN
                        (pure unit)
                        testPlain

            it "testNested2" $ do
                liftEffect $
                    Flex.foldN
                        logFoldN
                        (pure unit)
                        testNested2

    describe "bar" $ do
        pure unit