module Toolkit.Test where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Tuple.Nested ((/\))
import Data.SOrder (SOrder, type (:::), T)
import Data.Repr as R

import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn as Fn
import Noodle.Fn.Process as P
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Toolkit.MapsFolds as TMF -- FIXME
import Noodle.Family.Def as Family -- FIXME


type Family = Family.Def


type Families repr m =
    ( foo :: FooFamily repr m
    , bar :: BarFamily repr m
    , sum :: SumFamily repr m
    , concat :: ConcatFamily repr m
    )

type Instances repr m =
    ( foo :: Array (FooNode repr m)
    , bar :: Array (BarNode repr m)
    , sum :: Array (SumNode repr m)
    , concat :: Array (ConcatNode repr m)
    )


type FooNode repr m   = Node "foo" Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) repr m
type FooFamily repr m =     Family Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) repr m

type BarNode repr m   = Node "bar" Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) repr m
type BarFamily repr m =     Family Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) repr m

type SumNode repr m   = Node "sum" Unit ( a :: Int, b :: Int ) ( sum :: Int ) repr m
type SumFamily repr m =     Family Unit ( a :: Int, b :: Int ) ( sum :: Int ) repr m

type ConcatNode repr m   = Node "concat" Unit ( one :: String, two :: String ) ( concat :: String ) repr m
type ConcatFamily repr m =        Family Unit ( one :: String, two :: String ) ( concat :: String ) repr m


type TestToolkit repr m =
    Toolkit Unit (Families repr m)


-- toolkit :: Toolkit
toolkit
    :: forall repr m
     . R.ToRepr Int repr => R.FromRepr repr Int
    => R.ToRepr String repr => R.FromRepr repr String
    => TestToolkit repr m
toolkit =
    Toolkit.from "test"
        familiesOrder
        { foo :
            Family.def
                unit
                { foo : "aaa", bar : "bbb", c : 32 }
                { out : false }
                $ Fn.make "foo" fooOrders
                $ pure unit
        , bar :
            Family.def
                unit
                { a : "aaa", b : "bbb", c : 32 }
                { x : false }
                $ Fn.make "bar" barOrders
                $ pure unit
        , concat :
            Family.def
                unit
                { one : "", two : "" }
                { concat : "" }
                $ Fn.make "concat" concatOrders
                $ do
                    one <- P.receive (Fn.Input 0 :: _ "one")
                    two <- P.receive (Fn.Input 1 :: _ "two")
                    P.send (Fn.Output 0 :: _ "concat") $ one <> two
        , sum :
            Family.def
                unit
                { a : 2, b : 3 }
                { sum : 0 }
                $ Fn.make "sum" sumOrders
                $ do
                    a <- P.receive (Fn.Input 0 :: _ "a")
                    b <- P.receive (Fn.Input 1 :: _ "b")
                    P.send (Fn.Output 0 :: _ "sum") $ a + b
        }

    {-
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
        , sum :
            unit
            /\ { a : 2, b : 3 }
            /\ { sum : 0 }
            /\ Fn.make "sum" ( do
                    a <- P.receive (Fn.Input :: Fn.Input "a")
                    b <- P.receive (Fn.Input :: Fn.Input "b")
                    P.send (Fn.Output :: Fn.Output "sum") $ a + b
                )
        }
    -}


familiesOrder
    = Proxy :: _ ( "foo" ::: "bar" ::: "sum" ::: T )


fooOrders :: Fn.Orders _ _
fooOrders =
    { inputs : Proxy :: _ ( "foo" ::: "bar" ::: "c" ::: T )
    , outputs : Proxy :: _ ( "out" ::: T )
    }


barOrders :: Fn.Orders _ _
barOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: "c" ::: T )
    , outputs : Proxy :: _ ( "out" ::: T )
    }


concatOrders :: Fn.Orders _ _
concatOrders =
    { inputs : Proxy :: _ ( "one" ::: "two" ::: T )
    , outputs : Proxy :: _ ( "concat" ::: T )
    }


sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }