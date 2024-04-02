module Test.Patch where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Aff (Aff)
import Record as Record

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (reflectSymbol)

import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple.Nested (over1, T2) as Tuple
import Data.Array as Array
import Data.String as String
import Data.List (List)
import Data.List as List
import Data.Bifunctor (bimap, lmap)
import Data.Map as Map
import Data.SOrder (SOrder, type (:::), T)
import Data.SProxy (reflect', proxify)

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Id (reflectFamily', NodeId, familyOf, Family', class HasInputsAt, class HasOutputsAt)
import Noodle.Id (Family(..), Family') as Node
import Noodle.Id (Input(..), Output(..), InputR, OutputR) as Fn

import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Node.MapsFolds as NMF
import Noodle.Node.MapsFolds.Repr as NMF
import Noodle.Node.MapsFolds.Flatten as NMF
import Noodle.Patch.MapsFolds as PMF
import Noodle.Patch.MapsFolds.Repr as PMF
import Noodle.Family.Def as Family

import Test.Repr.Patch (MyRepr(..))

import Unsafe.Coerce (unsafeCoerce)

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


_foo = (Node.Family :: Node.Family "foo")
_bar = (Node.Family :: Node.Family "bar")


type Families m =
    ( foo :: Family.Def Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m
    , bar :: Family.Def Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) m
    )

type MyToolkit m
    = Toolkit Unit (Families m)


type Instances m =
    ( foo :: Array (Node "foo" Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m)
    , bar :: Array (Node "bar" Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) m)
    )


{-
type ReprInstances m =
    ( foo :: m (Array (PMF.NodeLineRec "foo" MyRepr ( foo :: MyRepr, bar :: MyRepr, c :: MyRepr ) ( out :: MyRepr )))
    , bar :: m (Array (PMF.NodeLineRec "bar" MyRepr ( a :: MyRepr, b :: MyRepr, c :: MyRepr ) ( x :: MyRepr )))
    -- , sum :: m (Array (PMF.NodeLineRec "sum" MyRepr ( a :: MyRepr, b :: MyRepr ) ( sum :: MyRepr )))
    ) -}


familiesOrder
    = Proxy :: _ ( "foo" ::: "bar" ::: T )


fooOrders =
    { inputs : Proxy :: _ ( "foo" ::: "bar" ::: "c" ::: T )
    , outputs : Proxy :: _ ( "out" ::: T )
    }


barOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: "c" ::: T )
    , outputs : Proxy :: _ ( "x" ::: T )
    }


spec :: Spec Unit
spec = do

    describe "patch" $ do

        let
            (toolkit :: MyToolkit Aff) =
            -- toolkit =
                Toolkit.from "test"
                    familiesOrder
                    { foo :
                        Family.def
                            unit
                            { foo : "aaa", bar : "bbb", c : 32 }
                            { out : false }
                            $ Fn.make "foo" fooOrders $ pure unit
                    , bar :
                        Family.def
                            unit
                            { a : "aaa", b : "bbb", c : 32 }
                            { x : false }
                            $ Fn.make "bar" barOrders $ pure unit
                    }

            extractFamily :: forall f z. Tuple.T2 (NodeId f) z -> Tuple.T2 String z
            extractFamily = Tuple.over1 (familyOf >>> reflectFamily')

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
                outA = Fn.Output 0 :: Fn.Output "out"
                inC = Fn.Input 1 :: Fn.Input "c"

            link <- Node.connect outA inC (if _ then 1 else 0) nodeA nodeB

            let nextPatch = Patch.registerLink link patch

            -- TODO

            pure unit


        it "storing links works, p.2" $ do

            nodeA <- Toolkit.spawn toolkit _foo
            nodeB <- Toolkit.spawn toolkit _bar

            let
                patch = Patch.init toolkit
                outA = Fn.Output 0 :: Fn.Output "out"
                inC = Fn.Input 1 :: Fn.Input "c"

            link /\ nextPatch <- Patch.connect outA inC (if _ then 1 else 0) nodeA nodeB patch

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

            (reflectF <$> Patch.nodes_ patch) `shouldEqual` [ "bar", "bar", "foo" ] -- [ "foo", "bar", "bar" ]

            (Node.family >>> reflectFamily' <$> Patch.nodes patch) `shouldEqual` [ "bar", "bar", "foo" ] -- [ "foo", "bar", "bar" ]

            {- (Node.state <$> Patch.nodes patch) `shouldEqual` [ "bar", "bar", "foo" ] -}

            {-
            Array.all (\(PMF.NodeInfo (fsym /\ _ /\ nodeId)) ->
                (reflect' fsym == "foo" || reflect' fsym == "bar") &&
                (reflect' (familyOf nodeId) == "foo" || reflect' (familyOf nodeId) == "bar")
            ) (Patch.nodesIndexed_ patch :: Array (PMF.NodeInfo _)) `shouldEqual` true

            Array.all (\(PMF.NodeWithIndex (fsym /\ _ /\ node)) ->
                (reflect' fsym == "foo" || reflect' fsym == "bar") &&
                (reflect' (familyOf $ Node.id node) == "foo" || reflect' (familyOf $ Node.id node) == "bar")
            ) (Patch.nodesIndexed patch) `shouldEqual` true

            -}

            -- liftEffect $ Console.log $ show (Patch.nodesIndexed patch :: Array (String /\ Int /\ NodeId _))


            Patch.nodesIndexed_ patch `shouldEqual` [ I 0, I 1, I 0 ]

            Patch.nodesMap patch `shouldEqual`
                { foo : [ S { foo : "foo" } ]
                , bar : [ S { foo : "bar" }, S { foo : "bar" } ]
                }-- [ I 0, I 1, I 0 ]

        it "repr-ing works with one family registered" $ do
            nodeA <- Toolkit.spawn toolkit _foo

            let
                -- NB! Notice that type is defined here, w/o having it defined, the compiler fails, at least in PS 0.14.5
                patch :: Patch Unit (Instances Aff)
                patch = Patch.init toolkit
                            # Patch.registerNode nodeA

            let reprMap = Patch.toRepr (Proxy :: _ Aff) (NMF.Repr :: _ MyRepr) patch

            fooReprs <- Record.get (proxify _foo) reprMap
            barReprs <- Record.get (proxify _bar) reprMap

            (extractFamily <$> fooReprs) `shouldEqual`
                [ "foo"
                    /\ Unit_
                    /\ { foo : String_ "aaa", bar : String_ "bbb", c : Int_ 32 }
                    /\ { out : Bool_ false }
                ]

            (extractFamily <$> barReprs) `shouldEqual`
                [ ]


        it "repr-ing works with all families registered" $ do

            nodeA <- Toolkit.spawn toolkit _foo
            nodeB <- Toolkit.spawn toolkit _bar
            nodeC <- Toolkit.spawn toolkit _bar

            let
                -- NB! Notice the lack of type definition in comparison with previous test
                patch = Patch.init toolkit
                            # Patch.registerNode nodeA
                            # Patch.registerNode nodeB
                            # Patch.registerNode nodeC

            let reprMap = Patch.toRepr (Proxy :: _ Aff) (NMF.Repr :: _ MyRepr) patch

            fooReprs <- Record.get (proxify _foo) reprMap
            barReprs <- Record.get (proxify _bar) reprMap

            (extractFamily <$> fooReprs) `shouldEqual`
                [ "foo"
                    /\ Unit_
                    /\ { foo : String_ "aaa", bar : String_ "bbb", c : Int_ 32 }
                    /\ { out : Bool_ false }
                ]

            (extractFamily <$> barReprs) `shouldEqual`
                [ "bar"
                    /\ Unit_
                    /\ { a : String_ "aaa", b : String_ "bbb", c : Int_ 32 }
                    /\ { x : Bool_ false }
                , "bar"
                    /\ Unit_
                    /\ { a : String_ "aaa", b : String_ "bbb", c : Int_ 32 }
                    /\ { x : Bool_ false }
                ]

            pure unit


        it "repr-ing to map works with all families registered" $ do

            nodeA <- Toolkit.spawn toolkit _foo
            nodeB <- Toolkit.spawn toolkit _bar
            nodeC <- Toolkit.spawn toolkit _bar

            let
                patch :: Patch Unit (Instances Aff)
                patch = Patch.init toolkit
                            # Patch.registerNode nodeA
                            # Patch.registerNode nodeB
                            # Patch.registerNode nodeC

            (reprsArr :: Array (NMF.NodeLineMap MyRepr)) <- Patch.toReprFlat (Proxy :: _ Aff) (NMF.Repr :: _ MyRepr) patch

            (NMF.flatten'' <$> reprsArr) `shouldEqual`
                [
                    "bar"
                        /\ Unit_
                        /\ [ "a" /\ String_ "aaa", "b" /\ String_ "bbb", "c" /\ Int_ 32 ]
                        /\ [ "x" /\ Bool_ false ]

                ,
                    "bar"
                        /\ Unit_
                        /\ [ "a" /\ String_ "aaa", "b" /\ String_ "bbb", "c" /\ Int_ 32 ]
                        /\ [ "x" /\ Bool_ false ]


                , "foo"
                        /\ Unit_
                        /\ [ "foo" /\ String_ "aaa", "bar" /\ String_ "bbb", "c" /\ Int_ 32 ]
                        /\ [ "out" /\ Bool_ false ]


                    {- TODO: right sorting ?
                    "foo"
                        /\ Unit_
                        /\ [ "foo" /\ String_ "aaa", "bar" /\ String_ "bbb", "c" /\ Int_ 32 ]
                        /\ [ "out" /\ Bool_ false ]

                ,

                     "bar"
                        /\ Unit_
                        /\ [ "a" /\ String_ "aaa", "b" /\ String_ "bbb", "c" /\ Int_ 32 ]
                        /\ [ "x" /\ Bool_ false ]

                ,
                    "bar"
                        /\ Unit_
                        /\ [ "a" /\ String_ "aaa", "b" /\ String_ "bbb", "c" /\ Int_ 32 ]
                        /\ [ "x" /\ Bool_ false ]
                    -}

                ]

            pure unit

newtype S = S { foo :: String }

derive newtype instance Show S
derive newtype instance Eq S

instance NMF.ConvertNodeTo S where
    convertNode node = S { foo : reflectFamily' (Node.family node) }


newtype I = I Int

derive newtype instance Show I
derive newtype instance Eq I



newtype F f = F (Family' f)

reflectF :: forall f. F f -> String
reflectF (F family) = reflectFamily' family

instance extractFamily :: NMF.ConvertNodeTo (F f') where
    convertNode :: forall f state is os m. Node f state is os m -> F f'
    convertNode node = F $ (unsafeCoerce $ Node.family node :: Family' f')

-- FIMXE: include `nodes` type into constraint
instance NMF.ConvertNodeIndexedTo I where
    convertNodeIndexed _ n _ = I n


newtype Shape =
    Shape { inputs :: List String, outputs :: List String }


instance (HasInputsAt is ksi
        , HasOutputsAt os kso
        ) => NMF.ConvertNodeTo' is os ksi kso Shape where
    convertNode'
        :: forall f' state' m'
         . Node f' state' is os m'
        -> Shape
    convertNode' node =
        case Node.shape node of
            inputs /\ outputs ->
                Shape { inputs : reflect' <$> inputs, outputs : reflect' <$> outputs }


derive newtype instance Show Shape
derive newtype instance Eq Shape
