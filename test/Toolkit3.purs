module Test.Toolkit3 where

import Prelude

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
import Noodle.Id (Family(..), Family', class HasInputs, class HasInputsAt) as Node
import Noodle.Id (InputR) as Fn
import Noodle.Id (inputs) as Def
import Noodle.Id (reflect', keysToInputsR, keysToOutputsR, reflectInputR, reflectOutputR)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Toolkit3.MapsFolds as TMF
import Noodle.Fn2.Process as Fn
import Noodle.Family.Def as Family
import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT
import Test.Signal (expectFn, expect)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Data.Symbol (reflectSymbol, class IsSymbol)
import Record.Extra (class Keys)
import Record.Extra as Record
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)
import Type.Proxy (Proxy(..))


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

        it "spawning works" $ do

            nodeA <- Toolkit.spawn toolkit (Node.Family :: Node.Family "foo")

            state <- Node.state nodeA
            state `shouldEqual` unit

            atC <- Node.inputs nodeA <#> _.c
            atC `shouldEqual` 32

            nodeB <- Toolkit.spawn toolkit2 (Node.Family :: Node.Family "sum")
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
            (Toolkit.toRepr (TMF.Repr :: TMF.Repr MyRepr) toolkit2)
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


--newtype Inputs = Inputs (List Fn.InputR)

newtype NameNT = NI String

newtype FamilyNT = FI String


derive newtype instance Show NameNT
derive newtype instance Eq NameNT

derive newtype instance Show FamilyNT
derive newtype instance Eq FamilyNT


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



data MyRepr
    = Unit_
    | String_ String
    | Int_ Int
    | Bool_ Boolean
    | Other_


instance Show MyRepr
    where
        show Unit_ = "Unit"
        show (String_ str) = "String::" <> str
        show (Int_ int) = "Int::" <> show int
        show (Bool_ bool) = "Bool_::" <> show bool
        show Other_ = "Other"


instance Eq MyRepr
    where
        eq Unit_ Unit_ = true
        eq (String_ strA) (String_ strB) = strA == strB
        eq (Int_ intA) (Int_ intB) = intA == intB
        eq (Bool_ boolA) (Bool_ boolB) = boolA == boolB
        eq Other_ Other_ = true
        eq _ _ = false


instance TMF.HasRepr String MyRepr  where toRepr _ = String_
instance TMF.HasRepr Int MyRepr where toRepr _ = Int_
instance TMF.HasRepr Unit MyRepr where toRepr _ _ = Unit_
instance TMF.HasRepr  Boolean MyRepr where toRepr _ = Bool_