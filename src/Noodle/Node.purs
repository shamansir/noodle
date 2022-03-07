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
  , dimensions, dimensionsBy, dimensionsBy'
  , disconnect
  , distribute
  , family
  , getI, getO
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
--   , markFamily
  , move
  , outletSignal
  , outletSignalFlipped
  , outlets
  , outletsSignal
  , outletsSignal'
  , produce
  , send
  , in_, out_, _in, _out
  , inletsBy, outletsBy
  )
  where

import Prelude

import Data.Array (mapMaybe, elemIndex, filter) as Array
import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Bifunctor (bimap)
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


in_ :: String -> InletId
in_ = Fn.in_


out_ :: String -> OutletId
out_ = Fn.out_


_in :: InletId -> String
_in = Fn._in


_out :: OutletId -> String
_out = Fn._out


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
--     map f (Node state m d fn (isignal /\ osignal) processM) = Node (f d) (f <$> processM)
    -- map f (Node state m d fn processM) = Node (f d) (f <$> fn) (f <$> processM)


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

distribute :: forall state m d. Sig.Channel (OutletId /\ d) -> Fn.Pass d -> Effect Unit
distribute passTo (Fn.Pass { toOutlets }) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable toOutlets :: Array (OutletId /\ d))


send :: forall state m d. Node state m d -> (InletId /\ d) -> Effect Unit
send node (inlet /\ d) =
    Ch.send (getInletsChannel node) $ inlet /\ d


produce :: forall state m d. Node state m d -> (OutletId /\ d) -> Effect Unit
produce node (outlet /\ d) =
    Ch.send (getOutletsChannel node) $ outlet /\ d


-- TODO: sendToOutlet ??


data Link = Link (Ref Boolean)


connect :: forall state m d. (Node state m d /\ OutletId) -> (Node state m d /\ InletId) -> Effect Link
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


-- attach :: forall state m d. Signal d -> InletId -> Node state m d -> Effect (Node state m d)
-- attach signal inlet node = pure node -- FIXME: TODO

getFn :: forall state m d. Node state m d -> NodeFn state m d
getFn (Node _ fn _) = fn


getInletsChannel :: forall state m d. Node state m d -> Ch.Channel (InletId /\ d)
getInletsChannel (Node _ _ (inlets_chan /\ _)) = inlets_chan


getOutletsChannel :: forall state m d. Node state m d -> Ch.Channel (OutletId /\ d)
getOutletsChannel (Node _ _ (_ /\ outlets_chan)) = outlets_chan


inletsSignal :: forall state m d. Node state m d -> Signal (InletId /\ d)
inletsSignal =
    Ch.subscribe <<< getInletsChannel


inletsSignal' :: forall state m d. Node state m d -> Signal (InletId /-> d)
inletsSignal' =
    Signal.foldp (Tuple.uncurry Map.insert) Map.empty <<< inletsSignal


outletsSignal :: forall state m d. Node state m d -> Signal (OutletId /\ d)
outletsSignal =
    Ch.subscribe <<< getOutletsChannel


outletsSignal' :: forall state m d. Node state m d -> Signal (OutletId /-> d)
outletsSignal' =
    Signal.foldp (Tuple.uncurry Map.insert) Map.empty <<< outletsSignal


inletSignal :: forall state m d. Node state m d -> InletId -> Signal d
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


outletSignalFlipped :: forall state m d. OutletId -> Node state m d -> Signal d
outletSignalFlipped = flip outletSignal


getI :: forall state m d. Node state m d -> InletId -> Effect d
getI node name = inletSignal node name # Signal.get


getO :: forall state m d. Node state m d -> OutletId -> Effect d
getO node name = outletSignal node name # Signal.get


getShape :: forall state m d. Node state m d -> Array (InletDef d) /\ Array (OutletDef d)
getShape = getFn >>> Fn.shapeOf


getShape' :: forall state m d. Node state m d -> (InletId /-> Channel.Def d) /\ (OutletId /-> Channel.Def d)
getShape' = getShape >>> bimap Map.fromFoldable Map.fromFoldable


inlet :: forall state m d. InletId -> Node state m d -> Maybe (InletDef d)
inlet name = getFn >>> Fn.findInput (Tuple.fst >>> (==) name)


outlet :: forall state m d. OutletId -> Node state m d -> Maybe (OutletDef d)
outlet name = getFn >>> Fn.findOutput (Tuple.fst >>> (==) name)


inlets :: forall state m d. Node state m d -> Array (InletDef d)
inlets = getShape >>> Tuple.fst


inletsBy :: forall state m d. (Channel.Def d -> Boolean) -> Node state m d -> Array (InletDef d)
inletsBy pred = inlets >>> Array.filter (Tuple.snd >>> pred)


outlets :: forall state m d. Node state m d -> Array (OutletDef d)
outlets = getShape >>> Tuple.snd


outletsBy :: forall state m d. (Channel.Def d -> Boolean) -> Node state m d -> Array (OutletDef d)
outletsBy pred = outlets >>> Array.filter (Tuple.snd >>> pred)


dimensions :: forall state m d. Node state m d -> Int /\ Int
dimensions = getFn >>> Fn.dimensions


dimensionsBy :: forall state m d. (InletDef d -> Boolean) -> (OutletDef d -> Boolean) -> Node state m d -> Int /\ Int
dimensionsBy iPred oPred = getFn >>> Fn.dimensionsBy iPred oPred


dimensionsBy' :: forall state m d. (Channel.Def d -> Boolean) -> Node state m d -> Int /\ Int
dimensionsBy' pred = dimensionsBy (Tuple.snd >>> pred) (Tuple.snd >>> pred)


indexOfInlet :: forall state m d. InletId -> Node state m d -> Maybe Int
indexOfInlet inletName node =
    Array.elemIndex inletName $ Tuple.fst <$> inlets node


indexOfOutlet :: forall state m d. OutletId -> Node state m d -> Maybe Int
indexOfOutlet outletName node =
    Array.elemIndex outletName $ Tuple.fst <$> outlets node


defaultOfInlet :: forall state m d. InletId -> Node state m d -> Maybe d
defaultOfInlet name node = inlet name node <#> Tuple.snd <#> Channel.default


defaultOfOutlet :: forall state m d. OutletId -> Node state m d -> Maybe d
defaultOfOutlet name node = outlet name node <#> Tuple.snd <#> Channel.default


default :: forall state m d. Node state m d -> d
default (Node d _ _) = d


{- markFamily :: forall state m d. Family -> Node state m d -> Node state m d
markFamily family (Node state m default shape channels _) =
    Node state m default shape channels $ Just family -}


family :: forall state m d. Node state m d -> Family
family = getFn >>> Fn.name


move :: forall state m d d'. (d -> d') -> (d' -> d) -> Node state m d -> Effect (Node state m d')
move f g (Node default fn (inChannel /\ outChannel)) =
    let
        movedFn = imap f g $ Fn.mapInputsAndOutputs (map $ map f) (map $ map f) fn
        nextDefault = f default
    in do
        newInChannel <- Ch.channel (consumerIn /\ nextDefault)
        newOutChannel <- Ch.channel (consumerOut /\ nextDefault)
        _ <- Signal.runSignal $ (Ch.subscribe inChannel ~> map f ~> Ch.send newInChannel)
        _ <- Signal.runSignal $ (Ch.subscribe outChannel ~> map f ~> Ch.send newOutChannel)
        pure $ Node nextDefault movedFn (newInChannel /\ newOutChannel)


linksAtInlet :: InletId -> LinksCount -> Int
linksAtInlet inlet = fromMaybe 0 <<< Map.lookup inlet <<< Tuple.fst


linksAtOutlet :: OutletId -> LinksCount -> Int
linksAtOutlet outlet = fromMaybe 0 <<< Map.lookup outlet <<< Tuple.snd