module Noodle.Node
    ( Node, Link
    , get, set
    , send, connect, disconnect
    , make
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , (<|), (|>), (<~>), (<+), (+>)
    , consumer
    )
    where

import Prelude (bind, const, pure, ($), (#), flip, (<$>), (<*>), (>>>), (<<<), (>>=), (=<<), unit, Unit, identity)

import Data.Array (mapMaybe) as Array
import Data.Maybe (Maybe)
import Data.Tuple (uncurry, curry, snd, Tuple(..))
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

import Signal (Signal, (~>))
import Signal (foldp, runSignal) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch
import Signal.Channel.Extra as Ch



{- Node stores incoming and outgoing channels (`Signal.Channel`, not `Noodle.Channel`) of data of type `d` + any additional data -}
data Node d a
    = Node
        (Channel (String /\ d) /\ Channel (String /\ d))
        a


instance functorNode :: Functor (Node d) where
    map f (Node channels a) = Node channels $ f a


newtype Link = Link (Ref Boolean)


consumer :: String
consumer = "consume_"


make
    :: forall d a
     . d
    -> Def a d
    -> Effect (Node d a)
make default (Def v fn) = do
    inlets_chan <- Ch.channel (consumer /\ default)
    outlets_chan <- Ch.channel (consumer /\ default)
    let
        inlets = Ch.subscribe inlets_chan
        node = Node (inlets_chan /\ outlets_chan) v
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


get :: forall d a. Node d a -> a
get (Node _ a) = a


set :: forall d a. a -> Node d a -> Node d a
set a (Node channels _) = Node channels a


-- fromFn' :: (d -> d) -> Node''' d

distribute :: forall d. Channel (String /\ d) -> Def.Pass d -> Effect Unit
distribute passTo (Def.Pass { toOutlets }) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable toOutlets :: Array (String /\ d))


send :: forall d a. Node d a -> (String /\ d) -> Effect Unit
send node (inlet /\ d) =
    Ch.send (getInletsChannel node) $ inlet /\ d


connect :: forall d a. (Node d a /\ String) -> (Node d a /\ String) -> Effect Link
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


attach :: forall d a. Signal d -> String -> Node d a -> Effect (Node d a)
attach signal inlet node = pure node -- FIXME: TODO


getInletsChannel :: forall d a. Node d a -> Channel (String /\ d)
getInletsChannel (Node (inlets_chan /\ _) _) = inlets_chan


getOutletsChannel :: forall d a. Node d a -> Channel (String /\ d)
getOutletsChannel (Node (_ /\ outlets_chan) _) = outlets_chan


inlets :: forall d a. Node d a -> Signal (String /\ d)
inlets =
    Ch.subscribe <<< getInletsChannel


outlets :: forall d a. Node d a -> Signal (String /\ d)
outlets =
    Ch.subscribe <<< getOutletsChannel


inlet :: forall d a. Node d a -> String -> Signal d
inlet node _ =
    Ch.subscribe (getInletsChannel node) ~> snd -- FIXME


outlet :: forall d a. Node d a -> String -> Signal d
outlet node _ =
    Ch.subscribe (getOutletsChannel node) ~> snd -- FIXME


outletFlipped :: forall d a. String -> Node d a -> Signal d
outletFlipped = flip outlet
