module Node
    ( Node, Receive, Send
    , receive, send, send', sendTo, connect
    , fromFn
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , (<|), (|>), (<~>), (<+), (+>)
    )
    where

import Prelude (bind, pure, ($), (#), flip, (<$>), (>>>), (<<<), (>>=), (=<<), unit, Unit)

import Control.Applicative (class Applicative, class Apply)

import Data.Array ((..))
import Data.Array (mapMaybe) as Array
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap, class Newtype)
import Data.Traversable (traverse, traverse_, sequence)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Signal (Signal, (~>))
import Signal (foldp, unwrap, flatten, constant, runSignal) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch


-- data Node' a m d = Node' (Channel d /\ Channel (m d)) a


-- type Node'' m d = Node' ( Array String /\ Array String ) m ( String /\ d )


-- type Node''' d = Node' Identity Unit

data Node d =
    Node
        (Channel (String /\ d) /\ Channel (String /\ d))


--type Node = NodeM Identity


newtype Receive d = Receive (Map String d)


newtype Send d = Send (Map String d)


fromFn
    :: forall d
     . d
    -> (Receive d -> Effect (Send d))
    -> Effect (Node d)
fromFn def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel ("bang" /\ def)
    let
        inlets = Ch.subscribe inlets_chan
        --outlets = Ch.subscribe outlets_chan
        node = Node (inlets_chan /\ outlets_chan)
        maps :: Signal (Map String d)
        maps = inlets # Signal.foldp (uncurry Map.insert) Map.empty
        fn_signal :: Signal (Effect (Send d))
        fn_signal = (Receive >>> fn) <$> maps
        sendFx :: Signal (Effect Unit)
        sendFx = ((=<<) $ sendAllTo outlets_chan) <$> fn_signal
    _ <- Signal.runSignal sendFx
    pure node


infixl 5 receive as <|
infixl 5 sendTo as |>
infixl 4 connect as <~>
infixl 4 inlet as +>
infixl 4 outletFlipped as <+


-- fromFn' :: (d -> d) -> Node''' d

sendAllTo :: forall d. Channel (String /\ d) -> Send d -> Effect Unit
sendAllTo sendTo (Send map) =
    traverse_ (Ch.send sendTo) $ (Map.toUnfoldable map :: Array (String /\ d))


receive :: forall d. String -> Receive d -> Maybe d
receive label (Receive r) = Map.lookup label r -- unwrap >>> flip Map.lookup


send :: forall d. Array (String /\ d) -> Send d
send = Send <<< Map.fromFoldable


send' :: forall d. Array (String /\ Maybe d) -> Send d
send' = Send <<< Map.fromFoldable <<< Array.mapMaybe sequence


sendTo :: forall d. Node d -> (String /\ d) -> Effect Unit
sendTo (Node (inlets_chan /\ _)) (inlet /\ d) =
    Ch.send inlets_chan $ inlet /\ d


connect :: forall d. (Node d /\ String) -> (Node d /\ String) -> Effect Unit
connect (srcNode /\ outlet) (trgNode /\ inlet) =
    let
        (Node ( _ /\ outlets_chan )) = srcNode
    in pure unit


getInletsChannel :: forall d. Node d -> Channel (String /\ d)
getInletsChannel (Node (inlets_chan /\ _)) = inlets_chan


getOutletsChannel :: forall d. Node d -> Channel (String /\ d)
getOutletsChannel (Node (_ /\ outlets_chan)) = outlets_chan


inlets :: forall d. Node d -> Signal (String /\ d)
inlets (Node (inlets_chan /\ _)) =
    Ch.subscribe inlets_chan


outlets :: forall d. Node d -> Signal (String /\ d)
outlets (Node (_ /\ outlets_chan)) =
    Ch.subscribe outlets_chan


inlet :: forall d. Node d -> String -> Signal d
inlet (Node (inlets_chan /\ _)) _ =
    Ch.subscribe inlets_chan ~> snd -- FIXME


outlet :: forall d. Node d -> String -> Signal d
outlet (Node (_ /\ outlets_chan)) _ =
    Ch.subscribe outlets_chan ~> snd -- FIXME


outletFlipped :: forall d. String -> Node d -> Signal d
outletFlipped = flip outlet
