module Noodle.Node.Shaped
    ( Def, Node
    , empty
    , define, defineEffectful
    , make
    , addInlet, addOutlet
    , reshape, reshapeInlet, reshapeOutlet
    , inletShape, outletShape
    , send, connect, disconnect
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    , fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    ) where



import Prelude (pure, ($), (#), (>>>), (<<<), (<$>), map, Unit)

import Data.Tuple (curry, uncurry)
import Data.Array (snoc)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map.Extra (type (/->))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Bifunctor (lmap, rmap)
import Data.Functor.Invariant (class Invariant)

import Noodle.Node as N
import Noodle.Node.Define as D
import Noodle.Node.Define (Receive, Pass)
import Noodle.Channel.Shape as Channel
import Noodle.Node.Shape (Shape)
import Noodle.Node.Shape as Shape


import Signal (Signal)

import Effect (Effect)


{-
class IsShaped m where
    inlet :: forall d. String -> m -> Maybe (Inlet d)
    outlet :: forall d. String -> m -> Maybe (Outlet d)
    inlets :: forall d. m -> Array (String /\ Inlet d)
    outlets :: forall d. m -> Array (String /\ Outlet d)

ShapeOf.inlet
ShapeOf.oulet
ShapeOf.inlets
ShapeOf.oulets
etc.
-}


type Def d =
    D.Def (Shape d) d


{- instance invariantDef :: Invariant (D.Def (Shape d)) where
    imap aToB bToA (D.Def shape fn) =
        D.Def (shape) $ \receive -> (<$>) aToB <$> (fn $ bToA <$> receive) -}


type Node d =
    N.Node d (Shape d)


empty :: forall d. Def d
empty = D.empty Shape.empty


define
    :: forall d
     . Shape.Inlets d
    -> Shape.Outlets d
    -> (D.Receive d -> D.Pass d)
    -> Def d
define inlets outlets = D.define $ inlets /\ outlets


defineEffectful
    :: forall d
     . Shape.Inlets d
    -> Shape.Outlets d
    -> (D.Receive d -> Effect (D.Pass d))
    -> Def d
defineEffectful inlets outlets = D.defineEffectful $ inlets /\ outlets


make
    :: forall d
     . d
    -> Def d
    -> Effect (Node d)
make = N.make


addInlet :: forall d. String -> Channel.Shape d -> Def d -> Def d
-- addInlet name shape = (<$>) (lmap $ Map.insert name shape)
addInlet name shape (D.Def shapes fn) = D.Def ((lmap $ Map.insert name shape) shapes) fn


addOutlet :: forall d. String -> Channel.Shape d -> Def d -> Def d
-- addOutlet name shape = (<$>) (rmap $ Map.insert name shape)
addOutlet name shape (D.Def shapes fn) = D.Def ((rmap $ Map.insert name shape) shapes) fn


reshape :: forall d. Shape.Inlets d -> Shape.Outlets d -> Def d -> Def d
reshape inlets outlets =
    D.set (inlets /\ outlets)
        -- FIXME: update the handler to monitor hot/cold inlets as well


reshapeInlet :: forall d. String -> Channel.Shape d -> Def d -> Def d
reshapeInlet = addInlet


reshapeOutlet :: forall d. String -> Channel.Shape d -> Def d -> Def d
reshapeOutlet = addOutlet


inletShape :: forall d. String -> Def d -> Maybe (Channel.Shape d)
inletShape inlet = D.get >>> Tuple.snd >>> Map.lookup inlet


outletShape :: forall d. String -> Def d -> Maybe (Channel.Shape d)
outletShape outlet = D.get >>> Tuple.fst >>> Map.lookup outlet


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


fromFn1 :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d) -> Def d
fromFn1 inlets outlets = D.fromFn1 $ inlets /\ outlets


fromFn1' :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> Maybe d) -> Def d
fromFn1' inlets outlets = D.fromFn1' $ inlets /\ outlets


fromFn2 :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d) -> Def d
fromFn2 inlets outlets = D.fromFn2 $ inlets /\ outlets


fromFn2' :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> Maybe d) -> Def d
fromFn2' inlets outlets = D.fromFn2' $ inlets /\ outlets


fromFn3 :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d -> d) -> Def d
fromFn3 inlets outlets = D.fromFn3 $ inlets /\ outlets


fromFn3' :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d -> Maybe d) -> Def d
fromFn3' inlets outlets = D.fromFn3' $ inlets /\ outlets


fromFn4 :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d -> d -> d) -> Def d
fromFn4 inlets outlets = D.fromFn4 $ inlets /\ outlets


fromFn4' :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d -> d -> Maybe d) -> Def d
fromFn4' inlets outlets = D.fromFn4' $ inlets /\ outlets


fromFn5 :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d -> d -> d -> d) -> Def d
fromFn5 inlets outlets = D.fromFn5 $ inlets /\ outlets


fromFn5' :: forall d. Shape.Inlets d -> Shape.Outlets d -> (d -> d -> d -> d -> d -> Maybe d) -> Def d
fromFn5' inlets outlets = D.fromFn5' $ inlets /\ outlets