module Toolkit.Test where

import Prelude

import Data.Tuple.Nested ((/\))

import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Fn2.Flow as Fn
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit

-- toolkit :: Toolkit
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