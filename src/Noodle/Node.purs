module Noodle.Node
  ( Family
  , Id
  , InletDef
  , InletId
  , Link(..)
  , LinksCount
  , Node(..)
  , NodeFn
  , OutletDef
  , OutletId
  , connect
  , consumerIn
  , consumerOut
  , default
  , defaultOfInlet
  , defaultOfOutlet
  , dimensions
  , disconnect
  , distribute
  , family
  , get
  , get'
  , getFn
  , getInletsChannel
  , getOutletsChannel
  , getShape
  , getShape'
  , indexOfInlet
  , indexOfOutlet
  , inlet
  , inletSignal
  , inlets
  , inletsSignal
  , inletsSignal'
  , linksAtInlet
  , linksAtOutlet
  , make
  , markFamily
  , move
  , outletSignal
  , outletSignalFlipped
  , outlets
  , outletsSignal
  , outletsSignal'
  , produce
  , send
  )
  where

import Prelude

import Data.Array (mapMaybe, elemIndex) as Array
import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Noodle.Channel as Channel
import Noodle.Fn (Fn)
import Noodle.Fn as Fn

import Signal (Signal, (~>))
import Signal (foldp, runSignal, filter, get) as Signal
import Signal.Channel as Ch
import Signal.Channel as Sig
import Signal.Channel.Extra as Ch


type Id = String

type Family = String


type LinksCount = (InletId /-> Int) /\ (OutletId /-> Int)


type InletId = Fn.InputId


type OutletId = Fn.OutputId


type InletDef d = InletId /\ Channel.Def d -- /\ Signal d


type OutletDef d = OutletId /\ Channel.Def d -- /\ Signal d


type NodeFn state m d = Fn state (InletDef d) (OutletDef d) m d


{- Node stores incoming and outgoing channels (`Signal.Channel`, not `Noodle.Channel`) of data of type `d` + any additional data -}
data Node state m d
    = Node
        d
        (NodeFn state m d)
        (Sig.Channel (InletId /\ d) /\ Sig.Channel (OutletId /\ d))
        -- (Shape d)
        -- (Channel (InletId /\ d) /\ Channel (OutletId /\ d))
        -- (Maybe Family) -- FIXME: make family required?
        -- we can turn these into Signals if we either pass the function needed to send values and forget it,
        -- or create it ourselves and return it to be re-used by outer world.
        -- Signals will give us Functors etc.
        -- see: https://github.com/sharkdp/purescript-flare/blob/master/src/Flare.purs#L156


-- instance invariantNode :: Invariant (Node state m) where
--     imap ()


-- instance functorNode :: Functor (Node state) where
--     map f (Node d fn (isignal /\ osignal) processM) = Node (f d) (f <$> processM)
    -- map f (Node d fn processM) = Node (f d) (f <$> fn) (f <$> processM)


consumerIn :: Fn.InputId
consumerIn = Fn.InputId "consume_"


consumerOut :: Fn.OutputId
consumerOut = Fn.OutputId "consume_"


make
    :: forall state m d
     . MonadEffect m
    => state
    -> d
    -> NodeFn state Aff d
    -> m (Node state Aff d)
make state default fn = do
    inlets_chan <- liftEffect $ Ch.channel (consumerIn /\ default)
    outlets_chan <- liftEffect $ Ch.channel (consumerOut /\ default)
    let
        inlets = Ch.subscribe inlets_chan
        node = Node default fn (inlets_chan /\ outlets_chan)
        store ( inlet /\ d ) ( _ /\ map ) = inlet /\ (map # Map.insert inlet d)
        maps = inlets # Signal.foldp store (consumerIn /\ Map.empty)
        toReceive (last /\ fromInputs) = Fn.Receive { last, fromInputs }
        send :: Fn.Send d
        send = Fn.Send $ Tuple.curry $ Ch.send outlets_chan -- could put the outgoing data in a Map and send once / in packs, see `Pass``
        -- fn_signal :: Signal (Effect (Fn.Pass d))
        fn_signal :: Signal (Effect Unit)
        fn_signal = maps ~> toReceive ~> (\receive -> Fn.runFn receive send default state fn) ~> launchAff_ -- Do not call fn if not the `isHot` inlet triggered the calculation
        -- passFx :: Signal (Effect Unit)
        -- passFx = ((=<<) $ distribute outlets_chan) <$> fn_signal
    _ <- liftEffect $ Signal.runSignal fn_signal
    pure node

{-


-- TODO: makeFixedPoint --forall i o. (Emitter i -> { input :: Emitter i, output :: Emitter o }) -> Emitter o



infixl 5 Def.receive as <+
infixl 5 send as +>
infixl 5 produce as ++>
infixl 4 connect as <~>
infixl 4 inletSignal as |>
infixl 4 outletSignalFlipped as <|


-- fromFn' :: (d -> d) -> Node''' d
-}

distribute :: forall d. Sig.Channel (OutletId /\ d) -> Fn.Pass d -> Effect Unit
distribute passTo (Fn.Pass { toOutlets }) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable toOutlets :: Array (OutletId /\ d))


send :: forall d. Node d -> (InletId /\ d) -> Effect Unit
send node (inlet /\ d) =
    Ch.send (getInletsChannel node) $ inlet /\ d


produce :: forall d. Node d -> (OutletId /\ d) -> Effect Unit
produce node (outlet /\ d) =
    Ch.send (getOutletsChannel node) $ outlet /\ d


-- TODO: sendToOutlet ??


data Link = Link (Ref Boolean)


connect :: forall d. (Node d /\ OutletId) -> (Node d /\ InletId) -> Effect Link
connect (srcNode /\ srcOutlet) (dstNode /\ dstInlet) =
    let inlets_chan = getInletsChannel dstNode
    in do
        ref <- Ref.new true
        _ <- Signal.runSignal
                $ outletSignal srcNode srcOutlet
                ~> Tuple dstInlet
                -- ~> Ch.send inlets_chan
                ~> Ch.sendIfRef inlets_chan ref
        pure $ Link ref


disconnect :: Link -> Effect Unit
disconnect (Link ref) =
    ref # Ref.write false


-- attach :: forall d. Signal d -> InletId -> Node d -> Effect (Node d)
-- attach signal inlet node = pure node -- FIXME: TODO

getFn :: forall state m d. Node state m d -> NodeFn state m d
getFn (Node fn _ _ _) = fn


getInletsChannel :: forall d. Node d -> Ch.Channel (InletId /\ d)
getInletsChannel (Node _ _ (inlets_chan /\ _) _) = inlets_chan


getOutletsChannel :: forall d. Node d -> Ch.Channel (OutletId /\ d)
getOutletsChannel (Node _ _ (_ /\ outlets_chan) _) = outlets_chan


inletsSignal :: forall d. Node d -> Signal (InletId /\ d)
inletsSignal =
    Ch.subscribe <<< getInletsChannel


inletsSignal' :: forall d. Node d -> Signal (InletId /-> d)
inletsSignal' =
    Signal.foldp (Tuple.uncurry Map.insert) Map.empty <<< inletsSignal


outletsSignal :: forall d. Node d -> Signal (OutletId /\ d)
outletsSignal =
    Ch.subscribe <<< getOutletsChannel


outletsSignal' :: forall d. Node d -> Signal (OutletId /-> d)
outletsSignal' =
    Signal.foldp (Tuple.uncurry Map.insert) Map.empty <<< outletsSignal


inletSignal :: forall d. Node d -> InletId -> Signal d
inletSignal node name =
    ( Ch.subscribe (getInletsChannel node)
        # Signal.filter
            (Tuple.fst >>> (==) name)
            (consumerIn /\ default node)
    ) ~> Tuple.snd


outletSignal :: forall state m d. Node state m d -> OutletId -> Signal d
outletSignal node name =
    ( Ch.subscribe (getOutletsChannel node)
        # Signal.filter
            (Tuple.fst >>> (==) name)
            (consumerOut /\ default node)
    ) ~> Tuple.snd


outletSignalFlipped :: forall state m d. OutletId -> Node d -> Signal d
outletSignalFlipped = flip outletSignal


get :: forall state m d. Node state m d -> InletId -> Effect d
get node name = inletSignal node name # Signal.get


get' :: forall d. Node d -> OutletId -> Effect d
get' node name = outletSignal node name # Signal.get


getShape :: forall d. Node d -> Array (InletDef d) /\ Array (OutletDef d)
getShape = getFn <<< Fn.shapeOf


getShape' :: forall d. Node d -> (InletId /-> Channel.Def d) /\ (OutletId /-> Channel.Def d)
getShape' = getFn <<< Fn.shapeOf <<< foldToMaps
    where foldToMaps = identity


inlet :: forall d. InletId -> Node d -> Maybe (InletDef d)
inlet name = getFn >>> Fn.findInput (Tuple.fst >>> (==) name)


outlet :: forall d. OutletId -> Node d -> Maybe (OutletDef d)
outlet name = getFn >>> Fn.findOutput (Tuple.fst >>> (==) name)


inlets :: forall d. Node d -> Array (InletDef d)
inlets = getShape >>> Tuple.fst


{-
inletsBy :: forall d. (InletDef d -> Boolean) -> Node d -> Array (InletDef d)
inletsBy pred = getShape' >>> Shape.inletsBy pred
-}


outlets :: forall d. Node d -> Array (OutletDef d)
outlets = getShape >>> Tuple.snd


{-
outletsBy :: forall d. (Channel.Shape d d -> Boolean) -> Node d -> Array (InletId /\ Channel.Shape d d)
outletsBy pred = getShape' >>> Shape.outletsBy pred
-}


dimensions :: forall d. Node d -> Int /\ Int
dimensions = getFn >>> Fn.dimensions


{-
dimensionsBy :: forall d. (Channel.Shape d d -> Boolean) -> Node d -> Int /\ Int
dimensionsBy pred (Node _ shape _ _) = Shape.dimensionsBy pred shape
-}


indexOfInlet :: forall d. InletId -> Node d -> Maybe Int
indexOfInlet inletName node =
    Array.elemIndex inletName $ Tuple.fst <$> inlets node


indexOfOutlet :: forall d. OutletId -> Node d -> Maybe Int
indexOfOutlet outletName node =
    Array.elemIndex outletName $ Tuple.fst <$> outlets node


defaultOfInlet :: forall d. InletId -> Node d -> Maybe d
defaultOfInlet name node = inlet name node <#> Channel.default


defaultOfOutlet :: forall d. OutletId -> Node d -> Maybe d
defaultOfOutlet name node = outlet name node <#> Channel.default


default :: forall d. Node d -> d
default (Node d _ _ _) = d


markFamily :: forall d. Family -> Node d -> Node d
markFamily family (Node default shape channels _) =
    Node default shape channels $ Just family


family :: forall d. Node d -> Maybe Family
family (Node _ _ _ f) = f


move :: forall a b. (a -> b) -> (b -> a) -> Node a -> Effect (Node b)
move f g (Node default shape (inChannel /\ outChannel) nodeFamily) =
    let
        movedShape = imap f g shape
        nextDefault = f default
    in do
        newInChannel <- Ch.channel (consumerIn /\ nextDefault)
        newOutChannel <- Ch.channel (consumerOut /\ nextDefault)
        _ <- Signal.runSignal $ (Ch.subscribe inChannel ~> ((<$>) f) ~> Ch.send newInChannel)
        _ <- Signal.runSignal $ (Ch.subscribe outChannel ~> ((<$>) f) ~> Ch.send newOutChannel)
        pure $ Node nextDefault movedShape (newInChannel /\ newOutChannel) nodeFamily


linksAtInlet :: InletId -> LinksCount -> Int
linksAtInlet inlet = fromMaybe 0 <<< Map.lookup inlet <<< Tuple.fst


linksAtOutlet :: OutletId -> LinksCount -> Int
linksAtOutlet outlet = fromMaybe 0 <<< Map.lookup outlet <<< Tuple.snd