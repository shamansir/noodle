module Test.Flex where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Data.Maybe as Maybe
import Data.Either as Either
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (sequence_)
import Data.Vec2 (Pos_, Size_, (<+>))

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)


import Layout.Flex.Rule (Rule)
import Layout.Flex.Rule as R
import Layout.Flex (Flex, flex, flex1, put, putAll, nest, nest', nest1, fit)
import Layout.Flex as Flex



oneRow :: Flex Int String
oneRow =
    flex1 5
        [ 5 /\ put "a"
        , 10 /\ put "f"
        , 2 /\ put "3"
        , 30 /\ put "i"
        ]


severalRows :: Flex Int String
severalRows =
    flex
        [ 10 /\ [ 5 /\ put "5x10. a", 10 /\ put "10x10. f", 2 /\ put "2x10. k" ]
        , 3 /\ [ 2 /\ put "2x3. c", 7 /\ put "7x3. d", 2 /\ put "2x3. e" ]
        , 7 /\ [ 1 /\ put "1x7. x", 2 /\ put "2x7. m", 14 /\ put "14x7. n" ]
        ]


complexNesting :: Flex Int String
complexNesting =
    flex
        [ 3 /\
            [ 5 /\ nest1 7 [ 2 /\ put "5x3 -> 2x7. a" ]
            , 10 /\ nest1 2 [ 3 /\ put "10x3 -> 3x2. b", 5 /\ put "10x3 -> 5x2. d" ]
            , 2 /\ nest1 3 [ 4 /\ put "2x3 -> 4x3. c" ]
            ]
        , 4 /\
            [ 3 /\ nest1 5 [ 0 /\ put "3x4 -> 0x5. e", 6 /\ put "3x4 -> 6x5. h", 1 /\ put "3x4 -> 1x5. j" ]
            , 5 /\ nest1 9 [ 4 /\ put "5x4 -> 4x9. i" ]
            , 6 /\ nest1 1 [ 17 /\ put "6x4 -> 17x1. f", 22 /\ put "6x4 -> 22x1. g" ]
            , 1 /\ nest' (map ((<>) "1x4 -> ") severalRows)
            ]
        , 2 /\ [ 3 /\ put "3x2. k" ]
        ]


-- testNodeDef :: Array (Flex Rule String)
testNodeDef :: Flex Rule String
testNodeDef =
    flex
        [ R.units 30.0 /\
            [ R.units 30.0 /\ put "padding-left"
            , R.fill /\ (nest' $ flex1 R.fill [ R.fill /\ put "title", R.units 10.0 /\ put "close-button" ])
            , R.units 30.0 /\ put "padding-right"
            ]
        , R.fill /\ -- vert
            [ R.units 30.0 /\ -- horz
                nest
                    [ R.fill /\
                        [ R.fill /\
                            nest'
                                (flex
                                    [ R.units 5.0 /\ [ R.fill /\ put "inlet1" ]
                                    , R.units 5.0 /\ [ R.fill /\ put "inlet2" ]
                                    , R.units 5.0 /\ [ R.fill /\ put "inlet3" ]
                                    , R.fill /\ [ R.fill /\ put "spacei" ]
                                    ]

                                )
                        ]
                    ]
            , R.fill /\ put "body"
            , R.units 30.0 /\ -- horz
                nest
                    [ R.fill /\
                        [ R.fill /\
                            nest'
                                (flex
                                    [ R.fill /\ [ R.fill /\ put "spaceo" ]
                                    , R.units 5.0 /\ [ R.fill /\ put "outlet1" ]
                                    , R.units 5.0 /\ [ R.fill /\ put "outlet2" ]
                                    ]

                                )
                        ]
                    ]
            ]
        ]


spec :: Spec Unit
spec = do

    describe "flex" $ do

        describe "folding with position and size" $ do

            it "one row" $ do
                liftEffect $ testFoldN
                    [ (0 <+> 0) /\ (5 <+> 5) /\ "a"
                    , (5 <+> 0) /\ (10 <+> 5) /\ "f"
                    , (15 <+> 0) /\ (2 <+> 5) /\ "3"
                    , (17 <+> 0) /\ (30 <+> 5) /\ "i"
                    ]
                    oneRow

            it "several rows" $ do
                liftEffect $ testFoldN
                    [ (0 <+> 0) /\ (5 <+> 10) /\ "5x10. a"
                    , (5 <+> 0) /\ (10 <+> 10) /\ "10x10. f"
                    , (15 <+> 0) /\ (2 <+> 10) /\ "2x10. k"
                    , (0 <+> 10) /\ (2 <+> 3) /\ "2x3. c"
                    , (2 <+> 10) /\ (7 <+> 3) /\ "7x3. d"
                    , (9 <+> 10) /\ (2 <+> 3) /\ "2x3. e"
                    , (0 <+> 13) /\ (1 <+> 7) /\ "1x7. x"
                    , (1 <+> 13) /\ (2 <+> 7) /\ "2x7. m"
                    , (3 <+> 13) /\ (14 <+> 7) /\ "14x7. n"
                    ]
                    severalRows

            it "complex nesting" $ do
                liftEffect $ testFoldN
                    [ ( 0 <+>  0) /\ ( 2 <+>  7) /\  "5x3 -> 2x7. a"
                    , ( 5 <+>  0) /\ ( 3 <+>  2) /\ "10x3 -> 3x2. b"
                    , ( 8 <+>  0) /\ ( 5 <+>  2) /\ "10x3 -> 5x2. d"
                    , (15 <+>  0) /\ ( 4 <+>  3) /\  "2x3 -> 4x3. c"
                    , ( 0 <+>  3) /\ ( 0 <+>  5) /\  "3x4 -> 0x5. e"
                    , ( 0 <+>  3) /\ ( 6 <+>  5) /\  "3x4 -> 6x5. h"
                    , ( 6 <+>  3) /\ ( 1 <+>  5) /\  "3x4 -> 1x5. j"
                    , ( 3 <+>  3) /\ ( 4 <+>  9) /\  "5x4 -> 4x9. i"
                    , ( 8 <+>  3) /\ (17 <+>  1) /\  "6x4 -> 17x1. f"
                    , (25 <+>  3) /\ (22 <+>  1) /\  "6x4 -> 22x1. g"
                    , (14 <+>  3) /\ ( 5 <+> 10) /\  "1x4 -> 5x10. a"
                    , (19 <+>  3) /\ (10 <+> 10) /\  "1x4 -> 10x10. f"
                    , (29 <+>  3) /\ ( 2 <+> 10) /\  "1x4 -> 2x10. k"
                    , (14 <+> 13) /\ ( 2 <+>  3) /\  "1x4 -> 2x3. c"
                    , (16 <+> 13) /\ ( 7 <+>  3) /\  "1x4 -> 7x3. d"
                    , (23 <+> 13) /\ ( 2 <+>  3) /\  "1x4 -> 2x3. e"
                    , (14 <+> 16) /\ ( 1 <+>  7) /\  "1x4 -> 1x7. x"
                    , (15 <+> 16) /\ ( 2 <+>  7) /\  "1x4 -> 2x7. m"
                    , (17 <+> 16) /\ (14 <+>  7) /\  "1x4 -> 14x7. n"
                    , (0  <+> 7)  /\ ( 3 <+>  2) /\         "3x2. k"
                    ]
                    complexNesting

        describe "fit & fold" $ do

            it "node structure" $ do
                liftEffect $ testFoldS
                    [ ( 0.0 <+>  0.0) /\ (30.0 <+> 30.0) /\ "padding-left"
                    , (30.0 <+>  0.0) /\ (30.0 <+> 30.0) /\ "title"
                    , (60.0 <+>  0.0) /\ (10.0 <+> 30.0) /\ "close-button"
                    , (70.0 <+>  0.0) /\ (30.0 <+> 30.0) /\ "padding-right"
                    , ( 0.0 <+> 30.0) /\ (30.0 <+>  5.0) /\ "inlet1"
                    , ( 0.0 <+> 35.0) /\ (30.0 <+>  5.0) /\ "inlet2"
                    , ( 0.0 <+> 40.0) /\ (30.0 <+>  5.0) /\ "inlet3"
                    , ( 0.0 <+> 45.0) /\ (30.0 <+> 55.0) /\ "spacei"
                    , (30.0 <+> 30.0) /\ (40.0 <+> 70.0) /\ "body"
                    , (70.0 <+> 30.0) /\ (30.0 <+> 60.0) /\ "spaceo"
                    , (70.0 <+> 90.0) /\ (30.0 <+>  5.0) /\ "outlet1"
                    , (70.0 <+> 95.0) /\ (30.0 <+>  5.0) /\ "outlet2"
                    ]
                    $ Flex.fit (100.0 <+> 100.0) testNodeDef

            it "node structure v.2" $ do
                liftEffect $ testFoldS
                    [ (  0.0 <+>   0.0) /\ ( 30.0 <+> 30.0) /\ "padding-left"
                    , ( 30.0 <+>   0.0) /\ (100.0 <+> 30.0) /\ "title"
                    , (130.0 <+>   0.0) /\ ( 10.0 <+> 30.0) /\ "close-button"
                    , (140.0 <+>   0.0) /\ ( 30.0 <+> 30.0) /\ "padding-right"
                    , (  0.0 <+>  30.0) /\ ( 30.0 <+>  5.0) /\ "inlet1"
                    , (  0.0 <+>  35.0) /\ ( 30.0 <+>  5.0) /\ "inlet2"
                    , (  0.0 <+>  40.0) /\ ( 30.0 <+>  5.0) /\ "inlet3"
                    , (  0.0 <+>  45.0) /\ ( 30.0 <+> 65.0) /\ "spacei"
                    , ( 30.0 <+>  30.0) /\ (110.0 <+> 80.0) /\ "body"
                    , (140.0 <+>  30.0) /\ ( 30.0 <+> 70.0) /\ "spaceo"
                    , (140.0 <+> 100.0) /\ ( 30.0 <+>  5.0) /\ "outlet1"
                    , (140.0 <+> 105.0) /\ ( 30.0 <+>  5.0) /\ "outlet2"
                    ]
                    $ Flex.fit (170.0 <+> 110.0) testNodeDef

    describe "bar" $ do
        pure unit


type FoldSample s a = Array (s /\ s) /\ a


testFold
    :: forall s a
     . Eq s => Eq a
    => Show s => Show a
    => Semiring s
    => Array (FoldSample s a)
    -> Flex s a
    -> Effect Unit
testFold items flex =
    let
        foldF sRef path str = do
            arr <- Ref.read sRef
            let next = Array.head arr
            Ref.write (Maybe.fromMaybe [] $ Array.tail arr) sRef
            case next of
                Just (path' /\ str') -> do
                    -- Console.log $ show path <> " --- " <> show prev <> " --- " <> show n <> " --- " <> show str
                    -- Console.log $ show path' <> " --- " <> show prev' <> " --- " <> show n' <> " --- " <> show str'
                    path' `shouldEqual` path
                    str' `shouldEqual` str
                Nothing -> do
                    fail $ "excessive call at " <> show path <> " --- " <> " --- " <> show str
            pure unit
    in do
        sRef <- Ref.new items
        Flex.fold
            (\path str eff -> eff <> foldF sRef path str)
            (pure unit)
            flex
        arr <- Ref.read sRef
        if Array.length arr > 0 then fail $ (show $ Array.length items) <> " items not fullfilled"
        else pure unit


        -- TODO: test order of folding for layers (from bottom to top) and order of searching for layers (from top to bottom), express it in the rules


type FoldNSample s a = Pos_ s /\ Size_ s /\ a


testFoldN
    :: forall s a
     . Eq s => Eq a
    => Show s => Show a
    => Ring s
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


type FoldSSample s a = Pos_ s /\ Size_ s /\ a


testFoldS
    :: forall s a
     . Eq s => Eq a
    => Show s => Show a
    => Ring s
    => Array (FoldSSample s a)
    -> Flex (Size_ s) a
    -> Effect Unit
testFoldS items flex =
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
        Flex.foldS
            (\pos size str eff -> eff <> foldF sRef pos size str)
            (pure unit)
            flex
        arr <- Ref.read sRef
        if Array.length arr > 0 then fail $ (show $ Array.length items) <> " items not fullfilled"
        else pure unit