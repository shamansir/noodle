module Toolkit.Test where

import Prelude

import Data.Tuple.Nested ((/\))

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Fn2.Flow as Fn
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Toolkit3 (Toolkit, NodeDef)
import Noodle.Toolkit3 as Toolkit


type Nodes m =
    ( foo :: NodeDef Unit ( foo :: String, bar :: String, c :: Int ) ( out :: Boolean ) m
    , bar :: NodeDef Unit ( a :: String, b :: String, c :: Int ) ( x :: Boolean ) m
    , sum :: NodeDef Unit ( a :: Int, b :: Int ) ( sum :: Int ) m
    )


type TestToolkit m =
    Toolkit Unit (Nodes m)


-- toolkit :: Toolkit
toolkit :: forall m. TestToolkit m
toolkit =
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