module Test.Patch4 where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.String as String
import Data.List (List)
import Data.List as List

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Id (reflectFamily', reflect', NodeId, familyOf, Family', class HasInputsAt, class HasOutputsAt)
import Noodle.Id (Family(..), Family') as Node
import Noodle.Id (Input(..), Output(..)) as Fn
import Type.Data.Symbol (reflectSymbol)

import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Patch4.MapsFolds as PMF
import Noodle.Family.Def as Family

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
                        Family.def
                            unit
                            { foo : "aaa", bar : "bbb", c : 32 }
                            { out : false }
                            $ Fn.make "foo" $ pure unit
                    , bar :
                        Family.def
                            unit
                            { a : "aaa", b : "bbb", c : 32 }
                            { x : false }
                            $ Fn.make "bar" $ pure unit
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

            {- (Node.state <$> Patch.nodes patch) `shouldEqual` [ "bar", "bar", "foo" ] -}

            Array.all (\(PMF.NodeInfo (fsym /\ _ /\ nodeId)) ->
                (reflect' fsym == "foo" || reflect' fsym == "bar") &&
                (reflect' (familyOf nodeId) == "foo" || reflect' (familyOf nodeId) == "bar")
            ) (Patch.nodesIndexed_ patch :: Array (PMF.NodeInfo _)) `shouldEqual` true

            Array.all (\(PMF.NodeWithIndex (fsym /\ _ /\ node)) ->
                (reflect' fsym == "foo" || reflect' fsym == "bar") &&
                (reflect' (familyOf $ Node.id node) == "foo" || reflect' (familyOf $ Node.id node) == "bar")
            ) (Patch.nodesIndexed patch) `shouldEqual` true

            -- liftEffect $ Console.log $ show (Patch.nodesIndexed patch :: Array (String /\ Int /\ NodeId _))

            Patch.nodesIndexed_ patch `shouldEqual` [ I 0, I 1, I 0 ]

            Patch.nodesMap patch `shouldEqual`
                { foo : [ S { foo : "foo" } ]
                , bar : [ S { foo : "bar" }, S { foo : "bar" } ]
                }-- [ I 0, I 1, I 0 ]

            {-
            Patch.nodesMap patch `shouldEqual`
                { foo :
                    [ Shape
                        { inputs : Array.toUnfoldable [ "foo", "bar", "c" ]
                        , outputs : Array.toUnfoldable [ "out" ]
                        }
                    ]
                , bar :
                    [ Shape
                        { inputs : Array.toUnfoldable [ "a", "b", "c" ]
                        , outputs : Array.toUnfoldable [ "x" ]
                        }
                    , Shape
                        { inputs : Array.toUnfoldable [ "a", "b", "c" ]
                        , outputs : Array.toUnfoldable [ "x" ]
                        }
                    ]
                }
            -}

            -- TODO

            pure unit

newtype S = S { foo :: String }

derive newtype instance Show S
derive newtype instance Eq S

instance PMF.ConvertNodeTo S where
    convertNode node = S { foo : reflectFamily' (Node.family node) }


newtype I = I Int

derive newtype instance Show I
derive newtype instance Eq I

-- FIMXE: include `nodes` type into constraint
instance PMF.ConvertNodeIndexedTo I where
    convertNodeIndexed _ n _ = I n


newtype Shape =
    Shape { inputs :: List String, outputs :: List String }


{-
instance PMF.ConvertNodeTo Shape where
    convertNode
        :: forall f' state' is' os' m' ksi kso
         . HasInputsAt is' ksi
        => HasOutputsAt os' kso
        => Node f' state' is' os' m'
        -> Shape
    convertNode node =
        case Node.shape node of
            inputs /\ outputs ->
                Shape { inputs : reflect' <$> inputs, outputs : reflect' <$> outputs }
-}


derive newtype instance Show Shape
derive newtype instance Eq Shape
