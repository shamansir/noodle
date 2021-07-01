module Noodle.Node
    ( Node, Receive, Pass, Link
    , get, set
    , receive, pass, pass', passNothing, send, connect, disconnect
    , empty, make, makeEff, doNothing
    , inlet, outlet, outletFlipped
    , inlets, outlets
    , (<|), (|>), (<~>), (<+), (+>)
    , withFn1, withFn2, withFn3, withFn4, withFn5
    , fromFn1, fromFn2, fromFn3, fromFn4, fromFn5
    , fromFn1', fromFn2', fromFn3', fromFn4', fromFn5'
    , consumer
    , lastUpdateAt
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

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Signal (Signal, (~>))
import Signal (foldp, runSignal) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch
import Signal.Channel.Extra as Ch


{- Node stores incoming and outgoing channels (`Signal.Channel`, not `Noodle.Channel`) of data of type `d` + any additional data -}
data Node d a
    = Node -- TODO: add name
        (Channel (String /\ d) /\ Channel (String /\ d))
        a


data Receive d = Receive { last :: String, fromInlets :: String /-> d }


newtype Pass d = Pass { toOutlets :: String /-> d }


newtype Link = Link (Ref Boolean)


instance functorNode :: Functor (Node d) where
    map f (Node channels a) = Node channels $ f a


consumer :: String
consumer = "consume_"


empty :: forall d a. a -> d -> Effect (Node d a)
empty v def = make v def $ const $ pass []


make
    :: forall d a
     . a
    -> d
    -> (Receive d -> Pass d)
    -> Effect (Node d a)
make v def fn = makeEff v def (pure <<< fn)


makeEff
    :: forall d a
     . a
    -> d
    -> (Receive d -> Effect (Pass d))
    -> Effect (Node d a)
makeEff v def fn = do
    inlets_chan <- Ch.channel (consumer /\ def)
    outlets_chan <- Ch.channel (consumer /\ def)
    let
        inlets = Ch.subscribe inlets_chan
        node = Node (inlets_chan /\ outlets_chan) v
        store ( inlet /\ d ) ( _ /\ map ) = inlet /\ (map # Map.insert inlet d)
        maps = inlets # Signal.foldp store (consumer /\ Map.empty)
        toReceive (last /\ fromInlets) = Receive { last, fromInlets }
        fn_signal :: Signal (Effect (Pass d))
        fn_signal = maps ~> toReceive ~> fn
        passFx :: Signal (Effect Unit)
        passFx = ((=<<) $ distribute outlets_chan) <$> fn_signal
    _ <- Signal.runSignal passFx
    pure node


-- TODO: `makePastDep` a.k.a. `makeFolding`


infixl 5 receive as <+
infixl 5 send as +>
infixl 4 connect as <~>
infixl 4 inlet as |>
infixl 4 outletFlipped as <|


get :: forall d a. Node d a -> a
get (Node _ a) = a


set :: forall d a. a -> Node d a -> Node d a
set a (Node channels _) = Node channels a


-- fromFn' :: (d -> d) -> Node''' d

distribute :: forall d. Channel (String /\ d) -> Pass d -> Effect Unit
distribute passTo (Pass { toOutlets }) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable toOutlets :: Array (String /\ d))


receive :: forall d. String -> Receive d -> Maybe d
receive label (Receive { fromInlets }) = Map.lookup label fromInlets -- unwrap >>> flip Map.lookup


lastUpdateAt :: forall d. Receive d -> String
lastUpdateAt (Receive { last }) = last


pass :: forall d. Array (String /\ d) -> Pass d
--pass = Pass <<< Map.fromFoldable
pass values = Pass { toOutlets : Map.fromFoldable values }


pass' :: forall d. Array (String /\ Maybe d) -> Pass d
--pass' = Pass <<< Map.fromFoldable <<< Array.mapMaybe sequence
pass' values = Pass { toOutlets : Map.fromFoldable $ Array.mapMaybe sequence $ values }


passNothing :: forall d. Pass d
passNothing = Pass { toOutlets : Map.empty }


doNothing :: forall d. Receive d -> Pass d
doNothing = const $ passNothing


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


fromFn1 :: forall d a. a -> d -> (d -> d) -> Effect (Node d a)
fromFn1 a def fn =
    make a def $ \r -> pass' [ "0" /\ (fn <$> receive "0" r) ]


fromFn1' :: forall d a. a -> d -> (d -> Maybe d) -> Effect (Node d a)
fromFn1' a def fn =
    make a def $ \r -> pass' [ "0" /\ (fn =<< receive "0" r) ]


fromFn2 :: forall d a. a -> d -> (d -> d -> d) -> Effect (Node d a)
fromFn2 a def fn =
    make a def $ \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r) ]


fromFn2' :: forall d a. a -> d -> (d -> d -> Maybe d) -> Effect (Node d a)
fromFn2' a def fn =
    make a def $ \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r) ]



fromFn3 :: forall d a. a -> d -> (d -> d -> d -> d) -> Effect (Node d a)
fromFn3 a def fn =
    make a def $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn3' :: forall d a. a -> d -> (d -> d -> d -> Maybe d) -> Effect (Node d a)
fromFn3' a def fn =
    make a def $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r) ]


fromFn4 :: forall d a. a -> d -> (d -> d -> d -> d -> d) -> Effect (Node d a)
fromFn4 a def fn =
    make a def $
        \r -> pass' [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn4' :: forall d a. a -> d -> (d -> d -> d -> d -> Maybe d) -> Effect (Node d a)
fromFn4' a def fn =
    make a def $
        \r -> pass' [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r) ]


fromFn5 :: forall d a. a -> d -> (d -> d -> d -> d -> d -> d) -> Effect (Node d a)
fromFn5 a def fn =
    make a def $ \r -> pass'
        [ "0" /\ (fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]


fromFn5' :: forall d a. a -> d -> (d -> d -> d -> d -> d -> Maybe d) -> Effect (Node d a)
fromFn5' a def fn =
    make a def $ \r -> pass'
        [ "0" /\ (identity =<< fn <$> receive "0" r <*> receive "1" r <*> receive "2" r <*> receive "3" r <*> receive "4" r) ]