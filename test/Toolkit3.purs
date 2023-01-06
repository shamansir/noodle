module Test.Toolkit3 where

import Prelude

import Data.List ((:), List)
import Data.List as List
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Id (Family(..)) as Node
import Noodle.Id (reflect')
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


spec :: Spec Unit
spec = do

    describe "toolkit" $ do

        let toolkit =
                Toolkit.from "test"
                    { foo :
                        unit
                        /\ { foo : "aaa", bar : "bbb", c : 32 }
                        /\ { out : false }
                        /\ Fn.make "foo" (pure unit)
                    }

        it "spawning works" $ do

            node <- Toolkit.spawn toolkit (Node.Family :: Node.Family "foo")

            state <- Node.state node
            state `shouldEqual` unit

            atC <- Node.inputs node <#> _.c
            atC `shouldEqual` 32

            pure unit


        it "getting family list" $ do
           -- (Toolkit.mapFamilies toolkit) `shouldEqual` { foo : Inputs ( "foo" : "bar" : "c" : List.Nil ) }

            (reflect' <$> Toolkit.nodeFamilies toolkit) `shouldEqual` ( "foo" : List.Nil )
            -- let
            --     myFn :: (forall f. IsSymbol f => Node.Family f) -> String
            --     myFn f = reflectSymbol f
            -- (?wh <$> Toolkit.nodeFamilies' toolkit) `shouldEqual` ( "foo" : List.Nil )

        it "getting inputs list" $ do
            -- (Toolkit.mapFamilies toolkit) `shouldEqual` { foo : F ( "foo" : "bar" : "c" : List.Nil ) }

            --Toolkit.familyDefs toolkit `shouldEqual` [ FI "foo", FI "bar", FI "c" ]
            pure unit


newtype Inputs = Inputs (List String)

newtype FI = FI String

derive newtype instance Show Inputs
derive newtype instance Eq Inputs

derive newtype instance Show FI
derive newtype instance Eq FI


{-
instance ConvertFamilyDefTo F
    where
        convertFamilyDef
            :: forall sso rlo rli (is :: Row Type) state os m
             . Record.Keys rli
            => RL.RowToList is rli
            => Record.Keys rlo
            => RL.RowToList os rlo
            => Toolkit.FamilyDef state is os m
            -> F
        convertFamilyDef def =
            F $ Toolkit.inputsFromDef def
-}