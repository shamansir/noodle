module Toolkit.Test where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Tuple.Nested ((/\))
import Data.SOrder (SOrder, type (:::), T)

import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Toolkit3.MapsFolds as TMF -- FIXME
import Noodle.Family.Def as Family -- FIXME


type Family = Family.Def


type Families m =
    ( foo :: Family Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m
    , bar :: Family Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) m
    , sum :: Family Unit ( a :: Int, b :: Int ) ( sum :: Int ) m
    )

type Instances m =
    ( foo :: Array (Node "foo" Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m)
    , bar :: Array (Node "bar" Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) m)
    , sum :: Array (Node "sum" Unit ( a :: Int, b :: Int ) ( sum :: Int ) m)
    )


type TestToolkit m =
    Toolkit Unit (Families m)


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


sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }


-- toolkit :: Toolkit
toolkit :: forall m. TestToolkit m
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
        , sum :
            Family.def
                unit
                { a : 2, b : 3 }
                { sum : 0 }
                $ Fn.make "sum" sumOrders
                $ do
                    a <- P.receive (Fn.Input :: Fn.Input "a")
                    b <- P.receive (Fn.Input :: Fn.Input "b")
                    P.send (Fn.Output :: Fn.Output "sum") $ a + b
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