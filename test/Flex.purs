module Test.Flex where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (sequence_)
import Data.Vec2 (Pos_, Size_, (<+>))

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)


import App.Layout.Flex (Flex, flex, flex1, nest, nest1)
import App.Layout.Flex as Flex



flexRow :: Flex Int String
flexRow =
    flex1 5 [ 5 /\ "a", 10 /\ "f", 2 /\ "3" ]


flexRows :: Flex Int String
flexRows =
    flex
        [ 10 /\ [ 5 /\ "a", 10 /\ "f", 2 /\ "k" ]
        , 3 /\ [ 2 /\ "c", 7 /\ "d", 2 /\ "e" ]
        , 7 /\ [ 1 /\ "x", 2 /\ "m", 14 /\ "n" ]
        ]


testNested2 :: Flex Int String
testNested2 =
    nest
        [ 3 /\
            [ 5 /\ flex1 7 [ 2 /\ "a" ]
            , 10 /\ flex1 2 [ 3 /\ "b", 5 /\ "d" ]
            , 2 /\ flex1 3 [ 4 /\ "c" ]
            ]
        , 4 /\
            [ 3 /\ flex1 5 [ 0 /\ "e", 6 /\ "h", 1 /\ "j" ]
            , 5 /\ flex1 9 [ 4 /\ "i" ]
            , 6 /\ flex1 1 [ 17 /\ "f", 22 /\ "g" ]
            , 1 /\ flexRows
            ]
        ]


{- testNested22 :: Flex Int String
testNested22 =
    nest1 2
        [ 5 /\ flex1 2 [ 2 /\ "a" ]
        , 10 /\ flex1 3 [ 3 /\ "b", 5 /\ "d" ]
        , 2 /\ flex1 5 [ 4 /\ "c" ]
        , 1 /\ flex1 7 [ 2 /\ "f", 4 /\ "m", 0 /\ "n", 3 /\ "o" ]
        ] -}


{-
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
        [ 5 /\ flex [ 2 /\ "a" ]
        , 10 /\ flex [ 3 /\ "b", 5 /\ "d" ]
        , 7 /\ nest [ 0 /\ testPlain ]
        , 2 /\ flex [ 4 /\ "c" ]
        ]
-}


spec :: Spec Unit
spec = do

    describe "flex" $ do

        describe "fold" $ do

            it "testPlain" $ pure unit {-do
                liftEffect $ testFold
                    [ [] /\ [] /\ 5 /\ "a"
                    , [] /\ [5] /\ 10 /\ "f"
                    , [] /\ [5,10] /\ 2 /\ "3"
                    ]
                    testPlain -}

            {- it "testNested2" $ do
                liftEffect $ testFold
                    [ [5] /\ [] /\ 2 /\ "a"
                    , [10] /\ [] /\ 3 /\ "b" -- should include prev items in a row as well, i.e. [5]?
                    , [10] /\ [3] /\ 5 /\ "d"
                    , [2] /\ [] /\ 4 /\ "c" -- should include prev items in a row as well, i.e. [5.10]?
                    ]
                    testNested2

            it "testNested22" $ do
                liftEffect $ testFold
                    [ [5] /\ [] /\ 2 /\ "a"
                    , [10] /\ [] /\ 3 /\ "b" -- should include prev items in a row as well, i.e. [5]?
                    , [10] /\ [3] /\ 5 /\ "d"
                    , [2] /\ [] /\ 4 /\ "c" -- should include prev items in a row as well, i.e. [5.10]?
                    , [1] /\ [] /\ 2 /\ "f" -- should include prev items in a row as well, i.e. [5.10,2]?
                    , [1] /\ [2] /\ 4 /\ "m"
                    , [1] /\ [2,4] /\ 0 /\ "n"
                    , [1] /\ [2,4,0] /\ 3 /\ "o"
                    ]
                    testNested22 -}

        describe "foldN" $ do

            it "testPlain" $ pure unit {- do
                liftEffect $ testFoldN
                    [ (0 <+> 0) /\ (0 <+> 5) /\ "a"
                    , (0 <+> 5) /\ (0 <+> 10) /\ "f"
                    , (0 <+> 15) /\ (0 <+> 2) /\ "3"
                    ]
                    testPlain -}

            {- it "testNested2" $ do
                liftEffect $ testFoldN
                    [ (0 <+> 0) /\ (2 <+> 5) /\ "a"
                    , (0 <+> 5) /\ (3 <+> 10) /\ "b"
                    , (3 <+> 5) /\ (5 <+> 10) /\ "d"
                    , (0 <+> 15) /\ (4 <+> 2) /\ "c"
                    ]
                    testNested2

            it "testNested22" $ do
                liftEffect $ testFoldN
                    [ (0 <+> 0) /\ (2 <+> 5) /\ "a"
                    , (0 <+> 5) /\ (3 <+> 10) /\ "b"
                    , (3 <+> 5) /\ (5 <+> 10) /\ "d"
                    , (0 <+> 15) /\ (4 <+> 2) /\ "c"
                    , (0 <+> 17) /\ (2 <+> 1) /\ "f"
                    , (2 <+> 17) /\ (4 <+> 1) /\ "m"
                    , (6 <+> 17) /\ (0 <+> 1) /\ "n"
                    , (6 <+> 17) /\ (3 <+> 1) /\ "o"
                    ]
                    testNested22 -}

    describe "bar" $ do
        pure unit


{-
type FoldSample s a = Array s /\ Array s /\ s /\ a


testFold
    :: forall s a
     . Eq s => Eq a
    => Show s => Show a
    => Array (FoldSample s a)
    -> Flex s a
    -> Effect Unit
testFold items flex =
    let
        foldF sRef path prev n str = do
            arr <- Ref.read sRef
            let next = Array.head arr
            Ref.write (Maybe.fromMaybe [] $ Array.tail arr) sRef
            case next of
                Just (path' /\ prev' /\ n' /\ str') -> do
                    -- Console.log $ show path <> " --- " <> show prev <> " --- " <> show n <> " --- " <> show str
                    -- Console.log $ show path' <> " --- " <> show prev' <> " --- " <> show n' <> " --- " <> show str'
                    path' `shouldEqual` path
                    prev' `shouldEqual` prev
                    n' `shouldEqual` n
                    str' `shouldEqual` str
                Nothing -> do
                    fail $ "excessive call at " <> show path <> " --- " <> show prev <> " --- " <> show n <> " --- " <> show str
            pure unit
    in do
        sRef <- Ref.new items
        Flex.fold
            (\path prev n str eff -> eff <> foldF sRef path prev n str)
            (pure unit)
            flex
        arr <- Ref.read sRef
        if Array.length arr > 0 then fail $ (show $ Array.length items) <> " items not fullfilled"
        else pure unit


type FoldNSample s a = Pos_ s /\ Size_ s /\ a


testFoldN
    :: forall s a
     . Eq s => Eq a
    => Show s => Show a
    => Semiring s
    => Array (FoldNSample s a)
    -> Flex s a
    -> Effect Unit
testFoldN items flex =
    let
        foldF sRef pos size str = do
            arr <- Ref.read sRef
            let next = Array.head arr
            Ref.write (Maybe.fromMaybe [] $ Array.tail arr) sRef
            case next of
                Just (pos' /\ size' /\ str') -> do
                    --Console.log $ show pos <> " --- " <> show size <> " --- " <> show str
                    --Console.log $ show pos' <> " --- " <> show size' <> " --- " <> show str'
                    pos' `shouldEqual` pos
                    size' `shouldEqual` size
                    str' `shouldEqual` str
                Nothing -> do
                    fail $ "excessive call at " <> show pos <> " --- " <> show size <> " --- " <> show str
            pure unit
    in do
        sRef <- Ref.new items
        Flex.foldN
            (\pos size str eff -> eff <> foldF sRef pos size str)
            (pure unit)
            flex
        arr <- Ref.read sRef
        if Array.length arr > 0 then fail $ (show $ Array.length items) <> " items not fullfilled"
        else pure unit
-}