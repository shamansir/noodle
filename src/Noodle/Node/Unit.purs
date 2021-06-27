module Noodle.Node.Unit
    ( Node
    , send, connect
    , make, makeEff
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


makeEff
    :: forall d
     . d
    -> (N.Receive d -> Effect (N.Pass d))
    -> Effect (Node d)
makeEff = N.makeEff unit


make
    :: forall d
     . d
    -> (N.Receive d -> N.Pass d)
    -> Effect (Node d)
make def fn = makeEff def (pure <<< fn)


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
fromFn1 def fn =
    make def $ \r -> N.pass' [ "0" /\ (fn <$> N.receive "0" r) ]


fromFn1' :: forall d. d -> (d -> Maybe d) -> Effect (Node d)
fromFn1' def fn =
    make def $ \r -> N.pass' [ "0" /\ (fn =<< N.receive "0" r) ]


fromFn2 :: forall d. d -> (d -> d -> d) -> Effect (Node d)
fromFn2 def fn =
    make def $ \r -> N.pass' [ "0" /\ (fn <$> N.receive "0" r <*> N.receive "1" r) ]


fromFn2' :: forall d. d -> (d -> d -> Maybe d) -> Effect (Node d)
fromFn2' def fn =
    make def $ \r -> N.pass' [ "0" /\ (identity =<< fn <$> N.receive "0" r <*> N.receive "1" r) ]



fromFn3 :: forall d. d -> (d -> d -> d -> d) -> Effect (Node d)
fromFn3 def fn =
    make def $
        \r -> N.pass' [ "0" /\ (fn <$> N.receive "0" r <*> N.receive "1" r <*> N.receive "2" r) ]


fromFn3' :: forall d. d -> (d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn3' def fn =
    make def $
        \r -> N.pass' [ "0" /\ (identity =<< fn <$> N.receive "0" r <*> N.receive "1" r <*> N.receive "2" r) ]


fromFn4 :: forall d. d -> (d -> d -> d -> d -> d) -> Effect (Node d)
fromFn4 def fn =
    make def $
        \r -> N.pass' [ "0" /\ (fn <$> N.receive "0" r <*> N.receive "1" r <*> N.receive "2" r <*> N.receive "3" r) ]


fromFn4' :: forall d. d -> (d -> d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn4' def fn =
    make def $
        \r -> N.pass' [ "0" /\ (identity =<< fn <$> N.receive "0" r <*> N.receive "1" r <*> N.receive "2" r <*> N.receive "3" r) ]


fromFn5 :: forall d. d -> (d -> d -> d -> d -> d -> d) -> Effect (Node d)
fromFn5 def fn =
    make def $ \r -> N.pass'
        [ "0" /\ (fn <$> N.receive "0" r <*> N.receive "1" r <*> N.receive "2" r <*> N.receive "3" r <*> N.receive "4" r) ]


fromFn5' :: forall d. d -> (d -> d -> d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn5' def fn =
    make def $ \r -> N.pass'
        [ "0" /\ (identity =<< fn <$> N.receive "0" r <*> N.receive "1" r <*> N.receive "2" r <*> N.receive "3" r <*> N.receive "4" r) ]