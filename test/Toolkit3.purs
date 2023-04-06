module Test.Toolkit3 where

import Prelude

import Record.Extra (class Keys)
import Record.Extra as Record
import Record (get) as Record
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)
import Type.Proxy (Proxy(..))
import Effect.Random (randomInt)


import Data.Maybe (Maybe(..))
import Data.List ((:), List)
import Data.List as List
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log) as Console


import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Id (Family(..), Family', class HasInputs, class HasInputsAt, FamilyR) as Node
import Noodle.Id (Input(..), Output(..), InputR) as Fn
import Noodle.Id (inputs) as Def
import Noodle.Id (reflect', keysToInputsR, keysToOutputsR, reflectInputR, reflectOutputR, reflectFamily'')
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Node2.MapsFolds as NMF
import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Toolkit3.MapsFolds as TMF
import Noodle.Toolkit3.MapsFolds.Repr as TMF
import Noodle.Fn2.Process as Fn
import Noodle.Family.Def as Family
import Noodle.Patch4 as Patch

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT

import Test.Signal (expectFn, expect)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Data.Symbol (reflectSymbol, class IsSymbol)

import Test.Repr.Toolkit3 (MyRepr(..))


type Families1 m
    = ( foo :: Family.Def Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m )

type Toolkit1 m
    = Toolkit Unit (Families1 m)

type Families2 m =
    ( foo :: Family.Def Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m
    , bar :: Family.Def Unit ( a :: String, b :: String, c :: String ) ( x :: Int ) m
    , sum :: Family.Def Unit ( a :: Int, b :: Int ) ( sum :: Int ) m
    )

type Toolkit2 m
    = Toolkit Unit (Families2 m)


spec :: Spec Unit
spec = do

    describe "toolkit" $ do

        let (toolkit :: Toolkit1 Aff) =
                Toolkit.from "test"
                    { foo :
                        Family.def
                            unit
                            { foo : "aaa", bar : "bbb", c : 32 }
                            { out : false }
                            $ Fn.make "foo" $ pure unit
                    -- , bar :
                    --     unit
                    --     /\ { foo : "aaa", bar : "bbb", c : 32 }
                    --     /\ { out : false }
                    --     /\ Fn.make "bar" (pure unit)

                        -- /\ { a : "aaa", b : "bbb", c : "ccc" }
                        -- /\ { x : 12 }
                        -- /\ Fn.make "bar" (pure unit)
                    }

            (toolkit2 :: Toolkit2 Aff) =
                Toolkit.from "test2"
                    { foo :
                        Family.def
                            unit
                            { foo : "aaa", bar : "bbb", c : 32 }
                            { out : false }
                            $ Fn.make "foo" $ pure unit
                    , bar :
                        Family.def
                            unit
                            { a : "aaa", b : "bbb", c : "ccc" }
                            { x : 12 }
                            $ Fn.make "bar" $ pure unit
                    , sum :
                        Family.def
                            unit
                            { a : 40, b : 2 }
                            { sum : 42 }
                            $ Fn.make "sumFn" $ pure unit
                    }

        -- TODO: add `Random` effect

        it "spawning works" $ do

            nodeA <- Toolkit.spawn toolkit (Node.Family :: _ "foo")

            state <- Node.state nodeA
            state `shouldEqual` unit

            atC <- Node.inputs nodeA <#> _.c
            atC `shouldEqual` 32

            nodeB <- Toolkit.spawn toolkit2 (Node.Family :: _ "sum")
            atSum <- Node.outputs nodeB <#> _.sum

            atSum `shouldEqual` 42

            pure unit


        it "getting family list" $ do
            (reflect' <$> Toolkit.nodeFamilies toolkit) `shouldEqual` ( "foo" : List.Nil )
            (reflect' <$> Toolkit.nodeFamilies toolkit2) `shouldEqual` ( "bar" : "foo" : "sum" : List.Nil )

            Toolkit.familyDefs toolkit `shouldEqual` [ NI "foo" ]

            Toolkit.familyDefsIndexed toolkit `shouldEqual` [ FI "foo" ]

            Toolkit.familyDefs toolkit2 `shouldEqual` [ NI "sumFn", NI "foo", NI "bar" ]

            Toolkit.familyDefsIndexed toolkit2 `shouldEqual` [ FI "sum", FI "foo", FI "bar" ]

        it "getting shapes" $ do
            (TMF.extractShapes $ Toolkit.toShapes toolkit2)
                `shouldEqual`

                    { bar :
                        [ "a", "b", "c" ] /\ [ "x" ]
                    , foo :
                        [ "bar", "c", "foo" ] /\ [ "out" ]
                    , sum :
                        [ "a", "b" ] /\ [ "sum" ]
                    }


        it "getting representations" $ do
            (Toolkit.toRepr (TMF.Repr :: _ MyRepr) toolkit2)
                `shouldEqual`

                    { foo :
                        Unit_
                        /\ { foo : String_ "aaa", bar : String_ "bbb", c : Int_ 32 }
                        /\ { out : Bool_ false }
                    , bar :
                        Unit_
                        /\ { a : String_ "aaa", b : String_ "bbb", c : String_ "ccc" }
                        /\ { x : Int_ 12 }
                    , sum :
                        Unit_
                        /\ { a : Int_ 40, b : Int_ 2 }
                        /\ { sum : Int_ 42 }
                    }


        it "spawn" $ do
            let families_ = { foo : Node.Family :: _ "foo", bar : Node.Family :: _ "bar" }
            let families = Toolkit.familyDefsIndexed toolkit2 :: Array FamilyRT
            let patch = Patch.init toolkit2
            let
                testFn "foo" = do
                    node <- Toolkit.spawn toolkit2 families_.foo
                    pure $ Patch.registerNode node patch
                testFn "bar" =
                    do
                    node <- Toolkit.spawn toolkit2 families_.bar
                    pure $ Patch.registerNode node patch
                testFn _ = pure patch
            case Array.index families 0 of
                Just (RT familyR) -> do
                    -- maybeNode <- Toolkit.spawn toolkit2 families_.foo -- FIXME: spawn node of the given family
                    pure unit
                Nothing -> fail "families list wasn't produced"

        {-
        it "unsafe spawn" $ do
            let families = Toolkit.familyDefsIndexed toolkit2 :: Array FamilyRT
            case Array.index families 0 of
                Just (RT familyR) -> do
                    maybeNode <- Toolkit.unsafeSpawnR toolkit2 familyR
                    case maybeNode of
                        Just _ -> pure unit
                        Nothing -> fail "falied to spawn"
                Nothing -> fail "families list wasn't produced"
        -}



--newtype Inputs = Inputs (List Fn.InputR)

families_ = { foo : Node.Family :: _ "foo", bar : Node.Family :: _ "bar" }
testFn' tk patch "foo" = do
    Patch.spawnAndRegisterNodeIfKnown families_.foo tk patch
    -- node <- Toolkit.spawn tk families_.foo
    -- pure $ Patch.registerNode node patch
testFn' tk patch "bar" =
    Patch.spawnAndRegisterNodeIfKnown families_.bar tk patch
testFn' _ patch _ = pure patch


newtype NameNT = NI String

newtype FamilyNT = FI String

newtype FamilyRT = RT Node.FamilyR


derive newtype instance Show NameNT
derive newtype instance Eq NameNT

derive newtype instance Show FamilyNT
derive newtype instance Eq FamilyNT

derive newtype instance Show FamilyRT
derive newtype instance Eq FamilyRT


instance TMF.ConvertFamilyDefTo NameNT where
    convertFamilyDef
            :: forall state is os m
             . Family.Def state is os m
            -> NameNT
    convertFamilyDef def = NI $ Fn.name $ Family.fn def


instance TMF.ConvertFamilyDefIndexedTo FamilyNT where
    convertFamilyDefIndexed
        :: forall f state is os m. IsSymbol f => Node.Family' f -> Family.Def state is os m -> FamilyNT
    convertFamilyDefIndexed family _ = FI $ reflect' family


instance TMF.ConvertFamilyDefIndexedTo FamilyRT where
    convertFamilyDefIndexed
        :: forall f state is os m. IsSymbol f => Node.Family' f -> Family.Def state is os m -> FamilyRT
    convertFamilyDefIndexed family _ = RT $ reflectFamily'' family


{-
instance Node.HasInputsAt is ks => TMF.ConvertFamilyDefTo (Inputs is ks)
    where
        convertFamilyDef
            :: forall state' is' os' m'
             . Node.HasInputsAt is ks
            => Family.Def state' is' os' m'
            -> Inputs is ks
        convertFamilyDef def = Inputs (Def.inputs def)
-}
