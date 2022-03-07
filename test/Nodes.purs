module Test.Nodes where

import Prelude

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node (Node)
import Noodle.Node as Node

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


spec :: Spec Unit
spec = do

    pure unit