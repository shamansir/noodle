module Test.Toolkit3 where

import Prelude

import Data.List ((:), List)
import Data.List as List
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log) as Console
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Id (Family(..), Family', class HasInputs) as Node
import Noodle.Id (InputR) as Fn
import Noodle.Id (reflect', keysToInputsR)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Toolkit3.MapsFolds as TMF
import Noodle.Fn2.Process as Fn
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
    = ( foo :: TMF.FamilyDef Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m )

type Toolkit1
    = Toolkit Unit (Families1 Aff)

type Families2 m =
    ( foo :: TMF.FamilyDef Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m
    , bar :: TMF.FamilyDef Unit ( a :: String, b :: String, c :: String ) ( x :: Int ) m
    , sum :: TMF.FamilyDef Unit ( a :: Int, b :: Int ) ( sum :: Int ) m
    )

type Toolkit2
    = Toolkit Unit (Families2 Aff)


spec :: Spec Unit
spec = do

    describe "toolkit" $ do

        let (toolkit :: Toolkit1) =
                Toolkit.from "test"
                    { foo :
                        unit
                        /\ { foo : "aaa", bar : "bbb", c : 32 }
                        /\ { out : false }
                        /\ Fn.make "foo" (pure unit)
                    -- , bar :
                    --     unit
                    --     /\ { foo : "aaa", bar : "bbb", c : 32 }
                    --     /\ { out : false }
                    --     /\ Fn.make "bar" (pure unit)

                        -- /\ { a : "aaa", b : "bbb", c : "ccc" }
                        -- /\ { x : 12 }
                        -- /\ Fn.make "bar" (pure unit)
                    }

            (toolkit2 :: Toolkit2) =
                Toolkit.from "test2"
                    { foo :
                        unit
                        /\ { foo : "aaa", bar : "bbb", c : 32 }
                        /\ { out : false }
                        /\ Fn.make "foo" (pure unit)
                    , bar :
                        unit
                        /\ { a : "aaa", b : "bbb", c : "ccc" }
                        /\ { x : 12 }
                        /\ Fn.make "bar" (pure unit)
                    , sum :
                        unit
                        /\ { a : 40, b : 2 }
                        /\ { sum : 42 }
                        /\ Fn.make "sumFn" (pure unit)
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

        pending "getting inputs list"
            -- (Toolkit.mapFamilies toolkit) `shouldEqual` { foo : Inputs ( "foo" : "bar" : "c" : List.Nil ) }
            --pure unit

            -- pure unit


newtype Inputs is = Inputs (List String)

newtype NameNT = NI String

newtype FamilyNT = FI String

derive newtype instance Show (Inputs is)
derive newtype instance Eq (Inputs is)

derive newtype instance Show NameNT
derive newtype instance Eq NameNT

derive newtype instance Show FamilyNT
derive newtype instance Eq FamilyNT


instance TMF.ConvertFamilyDefTo NameNT where
    convertFamilyDef
            :: forall state is os m
             . TMF.FamilyDef state is os m
            -> NameNT
    convertFamilyDef (_ /\ _ /\ _ /\ fn) = NI $ Fn.name fn


instance TMF.ConvertFamilyDefIndexedTo FamilyNT where
    convertFamilyDefIndexed
        :: forall f state is os m. IsSymbol f => Node.Family' f -> TMF.FamilyDef state is os m -> FamilyNT
    convertFamilyDefIndexed family _ = FI $ reflect' family


{- instance TMF.ConvertFamilyDefTo (Inputs is)
    where
        convertFamilyDef
            :: forall sso rli state is' os m ks
             . Node.HasInputs is' ks => TMF.FamilyDef state is' os m
            -> Inputs is
        convertFamilyDef def =
            -- Inputs (reflect' <$> (unsafeCoerce (Toolkit.inputsFromDef (unsafeCoerce (unsafeCoerce def :: forall kl s i o mm. Node.HasInputs i kl => TMF.FamilyDef s i o mm))) :: List Fn.InputR))
            Inputs (reflect' <$> keysToInputsR (Proxy :: Proxy is')) -}