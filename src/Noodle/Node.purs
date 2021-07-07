module Noodle.Node
    ( Node, Link
    , send, connect, disconnect
    , make
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , (<|), (|>), (<~>), (<+), (+>)
    , consumer
    )
    where

import Prelude

import Data.Array (mapMaybe) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry, curry, fst, snd, Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Traversable (traverse_, sequence)
import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Noodle.Node.Define (Def(..))
import Noodle.Node.Define as Def
import Noodle.Node.Shape (Shape)

import Signal (Signal, (~>))
import Signal (foldp, runSignal, filter) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch
import Signal.Channel.Extra as Ch



{- Node stores incoming and outgoing channels (`Signal.Channel`, not `Noodle.Channel`) of data of type `d` + any additional data -}
data Node d
    = Node
        (Shape d)
        (Channel (String /\ d) /\ Channel (String /\ d))


newtype Link = Link (Ref Boolean)


consumer :: String
consumer = "consume_"


make
    :: forall d
     . d
    -> Def d
    -> Effect (Node d)
make default (Def shape fn) = do
    inlets_chan <- Ch.channel (consumer /\ default) -- TODO: `isHot` etc.
    outlets_chan <- Ch.channel (consumer /\ default) -- TODO: `isHot` etc.
    let
        inlets = Ch.subscribe inlets_chan
        node = Node shape (inlets_chan /\ outlets_chan)
        store ( inlet /\ d ) ( _ /\ map ) = inlet /\ (map # Map.insert inlet d)
        maps = inlets # Signal.foldp store (consumer /\ Map.empty)
        toReceive (last /\ fromInlets) = Def.Receive { last, fromInlets }
        fn_signal :: Signal (Effect (Def.Pass d))
        fn_signal = maps ~> toReceive ~> fn
        passFx :: Signal (Effect Unit)
        passFx = ((=<<) $ distribute outlets_chan) <$> fn_signal
    _ <- Signal.runSignal passFx
    pure node


-- TODO: makeFixedPoint --forall i o. (Emitter i -> { input :: Emitter i, output :: Emitter o }) -> Emitter o



infixl 5 Def.receive as <+
infixl 5 send as +>
infixl 4 connect as <~>
infixl 4 inlet as |>
infixl 4 outletFlipped as <|


-- fromFn' :: (d -> d) -> Node''' d

distribute :: forall d. Channel (String /\ d) -> Def.Pass d -> Effect Unit
distribute passTo (Def.Pass { toOutlets }) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable toOutlets :: Array (String /\ d))


send :: forall d. Node d -> (String /\ d) -> Effect Unit
send node (inlet /\ d) =
    Ch.send (getInletsChannel node) $ inlet /\ d


connect :: forall d. (Node d /\ String) -> (Node d /\ String) -> Effect Link
connect (srcNode /\ srcOutlet) (dstNode /\ dstInlet) =
    let inlets_chan = getInletsChannel dstNode
    in do
        ref <- Ref.new true
        _ <- Signal.runSignal
                $ outlet srcNode srcOutlet
                ~> Tuple dstInlet
                -- ~> Ch.send inlets_chan
                ~> Ch.sendIfRef inlets_chan ref
        pure $ Link ref


disconnect :: Link -> Effect Unit
disconnect (Link ref) =
    ref # Ref.write false


attach :: forall d. Signal d -> String -> Node d -> Effect (Node d)
attach signal inlet node = pure node -- FIXME: TODO


getInletsChannel :: forall d. Node d -> Channel (String /\ d)
getInletsChannel (Node _ (inlets_chan /\ _)) = inlets_chan


getOutletsChannel :: forall d. Node d -> Channel (String /\ d)
getOutletsChannel (Node _ (_ /\ outlets_chan)) = outlets_chan


inlets :: forall d. Node d -> Signal (String /\ d)
inlets =
    Ch.subscribe <<< getInletsChannel


outlets :: forall d. Node d -> Signal (String /\ d)
outlets =
    Ch.subscribe <<< getOutletsChannel


inlet :: forall d. Node d -> String -> Signal d
inlet node name =
    Ch.subscribe (getInletsChannel node) ~> snd -- FIXME


outlet :: forall d. Node d -> String -> Signal d
outlet node _ =
    Ch.subscribe (getOutletsChannel node) ~> snd -- FIXME


outletFlipped :: forall d. String -> Node d -> Signal d
outletFlipped = flip outlet
