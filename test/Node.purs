module Test.Node where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console
import Effect.Aff (Aff)


import Test.Spec (Spec, pending, describe, describeOnly, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

import Data.List ((:))
import Data.List (List(..), fromFoldable) as List
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Int as Int
import Data.Bifunctor (bimap)
import Data.SOrder (SOrder, type (:::), T)
import Data.KeyHolder as KH
import Data.Repr as R
import Data.Maybe (Maybe(..))
import Data.SProxy (reflect, reflect')

import Type.Proxy (Proxy(..))

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Fn.Process as P
import Noodle.Id (Family(..)) as Node
import Noodle.Id (Input(..), Output(..), InputR(..), HoldsInput, HoldsOutput) as Fn

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


_sum = Node.Family :: _ "sum"


_aI = Fn.Input 0 :: _ "a"
_bI = Fn.Input 1 :: _ "b"
_sumO = Fn.Output 0 :: _ "sum"


iso = Proxy :: _ ("a" ::: "b" ::: T)
oso = Proxy :: _ ("sum" ::: T)


_corder = Node.Family :: _ "corder"


{-
_acoI = Fn.Input 2 :: _ "a"
_bcoI = Fn.Input 1 :: _ "b"
_ccoI = Fn.Input 3 :: _ "c"
_dcoI = Fn.Input 4 :: _ "d"
_ecoI = Fn.Input 0 :: _ "e"
_acoO = Fn.Output 0 :: _ "a"
_bcoO = Fn.Output 1 :: _ "b"
_ccoO = Fn.Output 3 :: _ "c"
_dcoO = Fn.Output 2 :: _ "d"
_ecoO = Fn.Output 4 :: _ "e"
-}


ico = Proxy :: _ ("e" ::: "b" ::: "a" ::: "c" ::: "d" ::: T)
-- ico = Proxy :: _ ("a" ::: "b" ::: "c" ::: "d" ::: "e" ::: T)
oco = Proxy :: _ ("a" ::: "f" ::: "b" ::: "d" ::: "c" ::: "e" ::: T)


spec :: Spec Unit
spec = do

    describe "creating & initial values" $ do

        it "is initialized properly" $ do
            node <- Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 } $ pure unit

            state <- Node.state node
            state `shouldEqual` unit

            atA <- Node.inputs node <#> _.a
            atA `shouldEqual` 2
            atA' <- node `Node.atI` _aI
            atA' `shouldEqual` 2
            atA'' <- node `Node._at` _.a
            atA'' `shouldEqual` 2

            atB <- Node.inputs node <#> _.b
            atB `shouldEqual` 3
            atB' <- node `Node.atI` _bI
            atB' `shouldEqual` 3
            atB'' <- node `Node._at` _.b
            atB'' `shouldEqual` 3

            atSum <- Node.outputs node <#> _.sum
            atSum `shouldEqual` 0
            atSum' <- node `Node.atO` _sumO
            atSum' `shouldEqual` 0
            atSum'' <- node `Node.at_` _.sum
            atSum'' `shouldEqual` 0

            pure unit

        it "function is performed properly" $ do

            node <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send (Fn.Output 0 :: Fn.Output "sum") $ a + b

            atSum <- node `Node.at_` _.sum
            atSum `shouldEqual` 0

            _ <- Node.run node

            atSumAfter <- node `Node.at_` _.sum
            atSumAfter `shouldEqual` 5

            pure unit

    describe "shapes" $ do

        it "is possible to extract shape" $ do
            node <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 } $ pure unit

            (reflect' <$> Node.inputsShape node) `shouldEqual` ( "a" : "b" : List.Nil )
            (reflect' <$> Node.outputsShape node) `shouldEqual` ( "sum" : List.Nil )

            (bimap (map reflect') (map reflect') $ Node.shape node)
                `shouldEqual`
                (( "a" : "b" : List.Nil ) /\ ( "sum" : List.Nil ))


    describe "connecting & disconnecting" $ do

        pending' "is possible to connect nodes" $ do -- FIXME: hangs indefinitely
            nodeA <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send (Fn.Output 0 :: Fn.Output "sum") $ a + b

            nodeB <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send (Fn.Output 0 :: Fn.Output "sum") $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            _ <- Node.connect
                    (Fn.Output 0 :: Fn.Output "sum")
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            pure unit


        pending' "is possible to connect nodes and keep sending values" $ do -- FIXME: hangs indefinitely
            nodeA <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            nodeB <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            _ <- Node.connect
                    _sumO
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            -- Node.with nodeA $ P.sendIn _aI 7
            Node.sendIn nodeA _aI 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB' <- nodeB `Node.at_` _.sum
            atSumB' `shouldEqual` (7 + 3 + 2)

            pure unit

        pending' "disconnecting works" $ do -- FIXME: hangs indefinitely
            nodeA <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            nodeB <-
                Node.make _sum unit iso oso { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            link <- Node.connect
                    _sumO
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            success <- Node.disconnect link nodeA nodeB
            success `shouldEqual` true

            -- Node.with nodeA $ P.sendIn _aI 7
            Node.sendIn nodeA _aI 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB -- FIXME: now `connect` runs the second node automatically

            atSumB' <- nodeB `Node.at_` _.sum
            atSumB' `shouldEqual` (4 + 3 + 2)

            pure unit

    describe "inputs / outputs orders" $ do

        it "works from `Node.HoldsInput` / `Node.HoldsOutput`" $ do
            node <-
                Node.make _corder unit ico oco { a : 3, b: "a", c : 5, d : 9, e : 10 } { a : 1, b : 2, c : 3, d : 4, e : 5, f : 7 }
                    $ pure unit

            inputsRec <- Node.inputs node
            outputsRec <- Node.outputs node

            let inputsRow = Node.inputsRow node
            let outputsRow = Node.outputsRow node

            let (aaa :: Array (Node.HoldsInputInNodeMRepr Aff MyRepr)) = Node.orderedNodeInputsTest' node
            let (bbb :: Array (Node.HoldsInputInNodeMRepr Aff MyRepr)) = Node.orderedNodeInputsTest' node

            (reflect' <$> (KH.orderedKeys' (Proxy :: _ Fn.Input) (Node.inputsOrder node) inputsRec :: Array Fn.HoldsInput)) `shouldEqual` [ "e", "b", "a", "c", "d" ]
            (reflect' <$> (KH.orderedKeys' (Proxy :: _ Fn.Output) (Node.outputsOrder node) outputsRec :: Array Fn.HoldsOutput)) `shouldEqual` [ "a", "f", "b", "d", "c", "e" ]

            (reflect' <$> (KH.orderedKeys' (Proxy :: _ Fn.Input) (Node.inputsOrder node) inputsRow :: Array Fn.HoldsInput)) `shouldEqual` [ "e", "b", "a", "c", "d" ]
            (reflect' <$> (KH.orderedKeys' (Proxy :: _ Fn.Output) (Node.outputsOrder node) outputsRow :: Array Fn.HoldsOutput)) `shouldEqual` [ "a", "f", "b", "d", "c", "e" ]

            (reflect' <$> (Node.orderedNodeBoundKeysTest (Proxy :: _ Fn.Input) (Node.inputsOrder node) inputsRow :: Array Fn.HoldsInput))  `shouldEqual` [ "e", "b", "a", "c", "d" ]
            (reflect' <$> (Node.orderedNodeBoundKeysTest' (Proxy :: _ Fn.Input) (Node.inputsOrder node) inputsRow node :: Array Fn.HoldsInput))  `shouldEqual` [ "e", "b", "a", "c", "d" ]

            (reflect' <$> (Node.orderedNodeInputsTest node :: Array Fn.HoldsInput))  `shouldEqual` [ "e", "b", "a", "c", "d" ]
            (reflect' <$> (Node.orderedNodeInputsTest' node :: Array (Node.HoldsInputInNodeM Aff)))  `shouldEqual` [ "e", "b", "a", "c", "d" ]
            (reflect' <$> (Node.orderedNodeInputsTest' node :: Array (Node.HoldsInputInNodeMRepr Aff MyRepr)))  `shouldEqual` [ "e", "b", "a", "c", "d" ]
            (reflect' <$> (Node.orderedNodeOutputsTest' node :: Array (Node.HoldsOutputInNodeMRepr Aff MyRepr)))  `shouldEqual`[ "a", "f", "b", "d", "c", "e" ]

            pure unit
            -- (reflect' <$> Node.orderedInputs nodeA) `shouldEqual` [ "e", "b", "a", "c", "d" ]
            -- (reflect' <$> Node.orderedOutputs nodeA) `shouldEqual` [ "a", "b", "d", "c", "e" ]

        -- TODO: test hashes


data MyRepr
    = IntRepr Int
    | StringRepr String


instance R.ToRepr Int MyRepr where toRepr = R.exists <<< IntRepr
instance R.ToRepr String MyRepr where toRepr = R.exists <<< StringRepr

instance R.ReadRepr MyRepr where
    readRepr :: String -> Maybe (_ MyRepr)
    readRepr str =
        case (Int.fromString str :: Maybe Int) of -- <|>
            Just int -> Just $ R.Repr $ IntRepr int
            Nothing -> Just $ R.Repr $ StringRepr str

instance R.WriteRepr MyRepr where
    writeRepr :: R.Repr MyRepr -> String
    writeRepr (R.Repr (IntRepr int)) = show int
    writeRepr (R.Repr (StringRepr str)) = str


instance R.FromRepr MyRepr Int where
    fromRepr (R.Repr (IntRepr n)) = Just n
    fromRepr _ = Nothing

instance R.FromRepr MyRepr String where
    fromRepr (R.Repr (StringRepr s)) = Just s
    fromRepr _ = Nothing
