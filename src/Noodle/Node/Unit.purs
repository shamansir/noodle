module Noodle.Node.Unit
    ( Node
    , send, connect
    , empty, make, makeEffectful
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    , fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    )
    where

import Prelude (pure, ($), (<$>), (<*>), (<<<), (=<<), unit, Unit, identity)

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)

import Signal (Signal)

import Noodle.Node as N


type Node d = N.Node d Unit


empty :: forall d. d -> Effect (Node d)
empty = N.empty unit


makeEffectful
    :: forall d
     . d
    -> (N.Receive d -> Effect (N.Pass d))
    -> Effect (Node d)
makeEffectful = N.makeEffectful unit


make
    :: forall d
     . d
    -> (N.Receive d -> N.Pass d)
    -> Effect (Node d)
make = N.make unit


send :: forall d. Node d -> (String /\ d) -> Effect Unit
send = N.send


connect :: forall d. (Node d /\ String) -> (Node d /\ String) -> Effect N.Link
connect = N.connect


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


fromFn1 :: forall d. d -> (d -> d) -> Effect (Node d)
fromFn1 = N.fromFn1 unit


fromFn1' :: forall d. d -> (d -> Maybe d) -> Effect (Node d)
fromFn1' = N.fromFn1' unit


fromFn2 :: forall d. d -> (d -> d -> d) -> Effect (Node d)
fromFn2 = N.fromFn2 unit


fromFn2' :: forall d. d -> (d -> d -> Maybe d) -> Effect (Node d)
fromFn2' = N.fromFn2' unit


fromFn3 :: forall d. d -> (d -> d -> d -> d) -> Effect (Node d)
fromFn3 = N.fromFn3 unit


fromFn3' :: forall d. d -> (d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn3' = N.fromFn3' unit


fromFn4 :: forall d. d -> (d -> d -> d -> d -> d) -> Effect (Node d)
fromFn4 = N.fromFn4 unit


fromFn4' :: forall d. d -> (d -> d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn4' = N.fromFn4' unit


fromFn5 :: forall d. d -> (d -> d -> d -> d -> d -> d) -> Effect (Node d)
fromFn5 = N.fromFn5 unit


fromFn5' :: forall d. d -> (d -> d -> d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn5' = N.fromFn5' unit