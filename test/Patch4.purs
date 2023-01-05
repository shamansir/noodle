module Test.Patch4 where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.String as String

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Id (reflectFamily', reflect', NodeId, familyOf, Family')
import Noodle.Id (Family(..), Family') as Node
import Noodle.Id (Input(..), Output(..)) as Fn
import Type.Data.Symbol (reflectSymbol)

import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch

import Unsafe.Coerce (unsafeCoerce)

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


_foo = (Node.Family :: Node.Family "foo")
_bar = (Node.Family :: Node.Family "bar")


spec :: Spec Unit
spec = do

    describe "patch" $ do

        let toolkit =
                Toolkit.from "test"
                    { foo :
                        unit
                        /\ { foo : "aaa", bar : "bbb", c : 32 }
                        /\ { out : false }
                        /\ Fn.make "foo" (pure unit)
                    , bar :
                        unit
                        /\ { a : "aaa", b : "bbb", c : 32 }
                        /\ { x : false }
                        /\ Fn.make "bar" (pure unit)
                    }

        it "adding node to the patch works" $ do

            node <- Toolkit.spawn toolkit _foo

            let
                patch = Patch.init toolkit # Patch.registerNode node

            Patch.howMany _foo patch `shouldEqual` 1
            Patch.howMany _bar patch `shouldEqual` 0

            pure unit

        it "storing links works" $ do

            nodeA <- Toolkit.spawn toolkit _foo
            nodeB <- Toolkit.spawn toolkit _bar

            let
                patch = Patch.init toolkit
                outA = Fn.Output :: Fn.Output "out"
                inC = Fn.Input :: Fn.Input "c"

            link <- Node.connect outA inC (if _ then 1 else 0) nodeA nodeB

            let nextPath = Patch.registerLink link patch

            -- TODO

            pure unit


        it "converting works" $ do

            nodeA <- Toolkit.spawn toolkit _foo
            nodeB <- Toolkit.spawn toolkit _bar
            nodeC <- Toolkit.spawn toolkit _bar

            let
                patch = Patch.init toolkit
                            # Patch.registerNode nodeA
                            # Patch.registerNode nodeB
                            # Patch.registerNode nodeC

            (reflectFamily' <$> Patch.nodes_ patch) `shouldEqual` [ "bar", "bar", "foo" ]

            (Node.family >>> reflectFamily' <$> Patch.nodes patch) `shouldEqual` [ "bar", "bar", "foo" ]

            Array.all (\(Patch.NodeInfo (fsym /\ _ /\ nodeId)) ->
                (reflect' fsym == "foo" || reflect' fsym == "bar") &&
                (reflect' (familyOf nodeId) == "foo" || reflect' (familyOf nodeId) == "bar")
            ) (Patch.nodesIndexed patch :: Array (Patch.NodeInfo _)) `shouldEqual` true

            -- liftEffect $ Console.log $ show (Patch.nodesIndexed patch :: Array (String /\ Int /\ NodeId _))

            Patch.nodesIndexed patch `shouldEqual` [ I 0, I 1, I 0 ]

            -- TODO

            pure unit


newtype I = I Int

derive newtype instance Show I
derive newtype instance Eq I

-- FIMXE: include `nodes` type into constraint
instance Patch.ConvertNodeIndexed I where
    convertNodeIndexed _ n _ = I n