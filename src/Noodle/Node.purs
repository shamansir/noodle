module Noodle.Node
    ( Node, Receive, Pass, Link
    , receive, pass, pass', send, connect, disconnect
    , make, makeEff
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , (<|), (|>), (<~>), (<+), (+>)
    , withFn1, withFn2, withFn3, withFn4, withFn5
    )
    where

import Prelude (bind, pure, ($), (#), flip, (<$>), (<*>), (>>>), (<<<), (>>=), (=<<), unit, Unit)

import Data.Array (mapMaybe) as Array
import Data.Maybe (Maybe)
import Data.Tuple (uncurry, snd, Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Traversable (traverse_, sequence)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal, (~>))
import Signal (foldp, runSignal) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch


data Node d a
    = Node
        (Channel (String /\ d) /\ Channel (String /\ d)) a


newtype Receive d = Receive (String /-> d)


newtype Pass d = Pass (String /-> d)


newtype Link = Link (Ref Boolean)


makeEff
    :: forall d a
     . a
    -> d
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node d a)
makeEff v def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel ("bang" /\ def)
    let
        inlets = Ch.subscribe inlets_chan
        --outlets = Ch.subscribe outlets_chan
        node = Node (inlets_chan /\ outlets_chan) v
        maps :: Signal (String /-> d)
        maps = inlets # Signal.foldp (uncurry Map.insert) Map.empty
        fn_signal :: Signal (Effect (Pass d))
        fn_signal = (Receive >>> fn) <$> maps
        passFx :: Signal (Effect Unit)
        passFx = ((=<<) $ distribute outlets_chan) <$> fn_signal
    _ <- Signal.runSignal passFx
    pure node


make
    :: forall d a
     . a
    -> d
    -> (Receive d -> Pass d)
    -> Effect (Node d a)
make v def fn = makeEff v def (pure <<< fn)


infixl 5 receive as <+
infixl 5 send as +>
infixl 4 connect as <~>
infixl 4 inlet as |>
infixl 4 outletFlipped as <|


-- fromFn' :: (d -> d) -> Node''' d

distribute :: forall d. Channel (String /\ d) -> Pass d -> Effect Unit
distribute passTo (Pass map) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable map :: Array (String /\ d))


receive :: forall d. String -> Receive d -> Maybe d
receive label (Receive r) = Map.lookup label r -- unwrap >>> flip Map.lookup


pass :: forall d. Array (String /\ d) -> Pass d
pass = Pass <<< Map.fromFoldable


pass' :: forall d. Array (String /\ Maybe d) -> Pass d
pass' = Pass <<< Map.fromFoldable <<< Array.mapMaybe sequence


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
                ~> sendIfRef inlets_chan ref
        pure $ Link ref


sendIfRef :: forall a. Channel a -> Ref Boolean -> a -> Effect Unit
sendIfRef channel ref v =
    Ref.read ref >>= \flag ->
        if flag then Ch.send channel v else pure unit


disconnect :: Link -> Effect Unit
disconnect (Link ref) =
    ref # Ref.write false


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


withFn1 :: forall d. (d -> d) -> String -> Receive d -> Maybe d
withFn1 fn inlet r = fn <$> receive inlet r


withFn2 :: forall d. (d -> d -> d) -> String -> String -> Receive d -> Maybe d
withFn2 fn inletA inletB r = fn <$> receive inletA r <*> receive inletB r


withFn3 :: forall d. (d -> d -> d -> d) -> String -> String -> String -> Receive d -> Maybe d
withFn3 fn inletA inletB inletC r = fn <$> receive inletA r <*> receive inletB r <*> receive inletC r


withFn4 :: forall d. (d -> d -> d -> d -> d) -> String -> String -> String -> String -> Receive d -> Maybe d
withFn4 fn inletA inletB inletC inletD r = fn <$> receive inletA r <*> receive inletB r <*> receive inletC r <*> receive inletD r


withFn5 :: forall d. (d -> d -> d -> d -> d -> d) -> String -> String -> String -> String -> String -> Receive d -> Maybe d
withFn5 fn inletA inletB inletC inletD inletE r =
    fn <$> receive inletA r <*> receive inletB r <*> receive inletC r <*> receive inletD r <*> receive inletE r