module Noodle.Node
    ( Node, Receive, Pass, Link, Node'
    , receive, pass, pass', send, send', connect, connect', disconnect
    , make, makeEff, make', makeEff'
    , inlet, outlet, outletFlipped, inlet', outlet', outletFlipped'
    , inlets, outlets, inlets', outlets'
    , (<|), (|>), (<~>), (<+), (+>)
    , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    , fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    , withFn1, withFn2, withFn3, withFn4, withFn5
    )
    where

import Prelude (bind, pure, ($), (#), flip, (<$>), (<*>), (>>>), (<<<), (>>=), (=<<), unit, Unit, identity)

import Data.Array (mapMaybe) as Array
import Data.Maybe (Maybe)
import Data.Tuple (uncurry, snd, Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse_, sequence)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal, (~>))
import Signal (foldp, runSignal) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch


data Node' d a
    = Node
        (Channel (String /\ d) /\ Channel (String /\ d)) a


type Node d = Node' d Unit


newtype Receive d = Receive (Map String d)


newtype Pass d = Pass (Map String d)


newtype Link = Link (Ref Boolean)


makeEff
    :: forall d
     . d
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node d)
makeEff = makeEff' unit


makeEff'
    :: forall d a
     . a
    -> d
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node' d a)
makeEff' v def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel ("bang" /\ def)
    let
        inlets = Ch.subscribe inlets_chan
        --outlets = Ch.subscribe outlets_chan
        node = Node (inlets_chan /\ outlets_chan) v
        maps :: Signal (Map String d)
        maps = inlets # Signal.foldp (uncurry Map.insert) Map.empty
        fn_signal :: Signal (Effect (Pass d))
        fn_signal = (Receive >>> fn) <$> maps
        passFx :: Signal (Effect Unit)
        passFx = ((=<<) $ distribute outlets_chan) <$> fn_signal
    _ <- Signal.runSignal passFx
    pure node


make
    :: forall d
     . d
    -> (Receive d -> Pass d)
    -> Effect (Node d)
make def fn = makeEff def (pure <<< fn)


make'
    :: forall d a
     . a
    -> d
    -> (Receive d -> Pass d)
    -> Effect (Node' d a)
make' v def fn = makeEff' v def (pure <<< fn)


infixl 5 receive as <+
infixl 5 send' as +>
infixl 4 connect' as <~>
infixl 4 inlet' as |>
infixl 4 outletFlipped' as <|


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


send :: forall d. Node d -> (String /\ d) -> Effect Unit
send = send'


send' :: forall d a. Node' d a -> (String /\ d) -> Effect Unit
send' node (inlet /\ d) =
    Ch.send (getInletsChannel node) $ inlet /\ d


connect :: forall d. (Node d /\ String) -> (Node d /\ String) -> Effect Link
connect = connect'


connect' :: forall d a. (Node' d a /\ String) -> (Node' d a /\ String) -> Effect Link
connect' (srcNode /\ srcOutlet) (dstNode /\ dstInlet) =
    let inlets_chan = getInletsChannel dstNode
    in do
        ref <- Ref.new true
        _ <- Signal.runSignal
                $ outlet' srcNode srcOutlet
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


getInletsChannel :: forall d a. Node' d a -> Channel (String /\ d)
getInletsChannel (Node (inlets_chan /\ _) _) = inlets_chan


getOutletsChannel :: forall d a. Node' d a -> Channel (String /\ d)
getOutletsChannel (Node (_ /\ outlets_chan) _) = outlets_chan


inlets :: forall d. Node d -> Signal (String /\ d)
inlets = inlets'


inlets' :: forall d a. Node' d a -> Signal (String /\ d)
inlets' =
    Ch.subscribe <<< getInletsChannel


outlets :: forall d. Node d -> Signal (String /\ d)
outlets = outlets'


outlets' :: forall d a. Node' d a -> Signal (String /\ d)
outlets' =
    Ch.subscribe <<< getOutletsChannel


inlet :: forall d. Node d -> String -> Signal d
inlet = inlet'


inlet' :: forall d a. Node' d a -> String -> Signal d
inlet' node _ =
    Ch.subscribe (getInletsChannel node) ~> snd -- FIXME


outlet :: forall d. Node d -> String -> Signal d
outlet = inlet'


outlet' :: forall d a. Node' d a -> String -> Signal d
outlet' node _ =
    Ch.subscribe (getOutletsChannel node) ~> snd -- FIXME


outletFlipped :: forall d. String -> Node d -> Signal d
outletFlipped = outletFlipped'


outletFlipped' :: forall d a. String -> Node' d a -> Signal d
outletFlipped' = flip outlet'


fromFn1 :: forall d. d -> (d -> d) -> Effect (Node d)
fromFn1 def fn =
    make def $ \r -> pass' [ "0" /\ (fn <$> receive "0" r) ]


fromFn1' :: forall d. d -> (d -> Maybe d) -> Effect (Node d)
fromFn1' def fn =
    make def $ \r -> pass' [ "0" /\ (fn =<< receive "0" r) ]


fromFn2 :: forall d. d -> (d -> d -> d) -> Effect (Node d)
fromFn2 def fn =
    make def $ \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r) ]


fromFn2' :: forall d. d -> (d -> d -> Maybe d) -> Effect (Node d)
fromFn2' def fn =
    make def $ \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r) ]



fromFn3 :: forall d. d -> (d -> d -> d -> d) -> Effect (Node d)
fromFn3 def fn =
    make def $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn3' :: forall d. d -> (d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn3' def fn =
    make def $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn4 :: forall d. d -> (d -> d -> d -> d -> d) -> Effect (Node d)
fromFn4 def fn =
    make def $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn4' :: forall d. d -> (d -> d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn4' def fn =
    make def $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn5 :: forall d. d -> (d -> d -> d -> d -> d -> d) -> Effect (Node d)
fromFn5 def fn =
    make def $ \r -> pass'
        [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]


fromFn5' :: forall d. d -> (d -> d -> d -> d -> d -> Maybe d) -> Effect (Node d)
fromFn5' def fn =
    make def $ \r -> pass'
        [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]


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