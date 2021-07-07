module Noodle.Node.Unit
    ( Def, Node
    , empty
    , define, defineEffectful
    , make
    , send, connect, disconnect
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    , fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    ) where

import Prelude (unit, Unit)

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)

import Signal (Signal)

import Noodle.Node.Define as D
import Noodle.Node as N


type Def d = D.Def Unit d


type Node d = N.Node d Unit


empty :: forall d. Def d
empty = D.empty unit


define
    :: forall d
     . (D.Receive d -> D.Pass d)
    -> Def d
define = D.define unit


defineEffectful
    :: forall d
     . (D.Receive d -> Effect (D.Pass d))
    -> Def d
defineEffectful = D.defineEffectful unit


make
    :: forall d
     . d
    -> Def d
    -> Effect (Node d)
make = N.make


send :: forall d. Node d -> (String /\ d) -> Effect Unit
send = N.send


connect :: forall d. (Node d /\ String) -> (Node d /\ String) -> Effect N.Link
connect = N.connect


disconnect :: N.Link -> Effect Unit
disconnect = N.disconnect


inlets :: forall d. Node d -> Signal (String /\ d)
inlets = N.inlets


outlets :: forall d. Node d -> Signal (String /\ d)
outlets = N.outlets


inlet :: forall d. Node d -> String -> Signal d
inlet = N.inlet


outlet :: forall d. Node d -> String -> Signal d
outlet = N.outlet


outletFlipped :: forall d. String -> Node d -> Signal d
outletFlipped = N.outletFlipped


fromFn1 :: forall d. (d -> d) -> Def d
fromFn1 = D.fromFn1 unit


fromFn1' :: forall d. (d -> Maybe d) -> Def d
fromFn1' = D.fromFn1' unit


fromFn2 :: forall d. (d -> d -> d) -> Def d
fromFn2 = D.fromFn2 unit


fromFn2' :: forall d. (d -> d -> Maybe d) -> Def d
fromFn2' = D.fromFn2' unit


fromFn3 :: forall d. (d -> d -> d -> d) -> Def d
fromFn3 = D.fromFn3 unit


fromFn3' :: forall d. (d -> d -> d -> Maybe d) -> Def d
fromFn3' = D.fromFn3' unit


fromFn4 :: forall d. (d -> d -> d -> d -> d) -> Def d
fromFn4 = D.fromFn4 unit


fromFn4' :: forall d. (d -> d -> d -> d -> Maybe d) -> Def d
fromFn4' = D.fromFn4' unit


fromFn5 :: forall d. (d -> d -> d -> d -> d -> d) -> Def d
fromFn5 = D.fromFn5 unit


fromFn5' :: forall d. (d -> d -> d -> d -> d -> Maybe d) -> Def d
fromFn5' = D.fromFn5' unit