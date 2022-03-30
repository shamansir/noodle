module Noodle.Node
  ( Family
  , Id
  , InletDef
  , InletId
  , Link(..)
  , LinksCount
  , Node(..)
  , NodeFn, NodeProcess
  , OutletDef
  , OutletId
  , ChannelDefs
  , connect
  , consumerIn
  , consumerOut
  , default
  , defaultOfInlet
  , defaultOfOutlet
  , dimensions--, dimensionsBy, dimensionsBy'
  , disconnect
  , family
  , getI, getO
  , getFn
  , getInletsChannel, getOutletsChannel
--   , getShape
--   , getShape'
  , indexOfInlet
  , indexOfOutlet
  , inlet, inletSignal
  , inlets, inletsSignal, inletsSignal'
  , linksAtInlet, linksAtOutlet
  , make, make', run
--   , markFamily
  , move
  , outletSignal
  , outletSignalFlipped
  , outlets
  , outletsSignal, outletsSignal'
  --, subscribeInlet, subscribeOutlet
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
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Noodle.Channel as Channel
import Noodle.Fn (Fn)
import Noodle.Fn
            ( run, make
            , InputId, OutputId
            , in_, out_, _in, _out
            , dimensions, name, findInput, findOutput, shapeOf
            , mapInputsAndOutputs
            ) as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)

import Control.Monad.Rec.Class (class MonadRec)

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


-- type ChannelDefs d = (InletId /-> Channel.Def d) /\ (OutletId /-> Channel.Def d)
type ChannelDefs d = Array (InletDef d) /\ Array (OutletDef d)


type NodeFn state d = Fn InletId (Channel.Def d) OutletId (Channel.Def d) state Aff d


type NodeProcess state d = Fn.ProcessM InletId OutletId state d Aff Unit


-- type NodeFn state m d = Fn InletId OutletId state m d


{- Node stores incoming and outgoing channels (`Signal.Channel`, not `Noodle.Channel`) of data of type `d` + any additional data -}
data Node state d
    = Node
        -- state
        d
        (NodeFn state d)
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
--     map f (Node m d fn (isignal /\ osignal) processM) = Node (f d) (f <$> processM)
    -- map f (Node m d fn processM) = Node (f d) (f <$> fn) (f <$> processM)


consumerIn :: Fn.InputId
consumerIn = Fn.in_ "consume_"


consumerOut :: Fn.OutputId
consumerOut = Fn.out_ "consume_"


make
    :: forall state d
     . Family
    -> d
    -> Array (InletId /\ Channel.Def d)
    -> Array (OutletId /\ Channel.Def d)
    -> NodeProcess state d
    -> Effect (Node state d)
make family default inlets outlets =
    make' default <<< Fn.make family inlets outlets


make'
    :: forall state m d
     . MonadEffect m
    => d
    -> NodeFn state d
    -> m (Node state d)
make' default fn = do
    inlets_chan <- liftEffect $ Ch.channel (consumerIn /\ default)
    outlets_chan <- liftEffect $ Ch.channel (consumerOut /\ default)
    pure $ Node default fn (inlets_chan /\ outlets_chan)


run :: forall state m d. MonadEffect m => Node state d -> state -> m Unit
run (Node default fn (inlets_chan /\ outlets_chan)) state =
    let
        inlets = Ch.subscribe inlets_chan
        outlets = Ch.subscribe outlets_chan
        store ( inlet /\ d ) ( _ /\ map ) = Just inlet /\ (map # Map.insert inlet d)
        maps = inlets # Signal.foldp store (Just consumerIn /\ Map.empty)
        toReceive (last /\ inputs) = { last, inputs }
        -- send :: Fn.Send OutletId d
        -- send = Fn.Send $ Tuple.curry $ Ch.send outlets_chan -- could put the outgoing data in a Map and send once / in packs, see `Pass``
        -- fn_signal :: Signal (Effect (Fn.Pass d))
        protocol :: (Maybe InletId /\ (InletId /-> d)) -> Protocol InletId OutletId d
        protocol (last /\ inletsMap) =
            { last : const $ pure last
            , receive : pure <<< flip Map.lookup inletsMap
            , send : \outlet d -> Ch.send outlets_chan (outlet /\ d)
            , sendIn : \inlet d -> Ch.send inlets_chan (inlet /\ d)
            }
        fn_signal :: Signal (Effect Unit)
        fn_signal = maps
                        ~> (\(last /\ inputMap) ->
                                Fn.run default state (protocol (last /\ inputMap)) fn
                            )
                        ~> launchAff_ -- Do not call fn if not the `isHot` inlet triggered the calculation
        -- passFx :: Signal (Effect Unit)
        -- passFx = ((=<<) $ distribute outlets_chan) <$> fn_signal
    in liftEffect $ Signal.runSignal fn_signal



-- mkRun ::


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

{-
distribute :: forall state d. Sig.Channel (OutletId /\ d) -> Fn.Pass OutletId d -> Effect Unit
distribute passTo (Fn.Pass { toOutputs }) =
    traverse_ (Ch.send passTo) $ (Map.toUnfoldable toOutputs :: Array (OutletId /\ d))
-}


send :: forall state d. Node state d -> (InletId /\ d) -> Effect Unit
send node (inlet /\ d) =
    Ch.send (getInletsChannel node) $ inlet /\ d


produce :: forall state d. Node state d -> (OutletId /\ d) -> Effect Unit
produce node (outlet /\ d) =
    Ch.send (getOutletsChannel node) $ outlet /\ d


-- TODO: sendToOutlet ??


data Link = Link (Ref Boolean)


connect :: forall state d. (Node state d /\ OutletId) -> (Node state d /\ InletId) -> Effect Link
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


-- attach :: forall state d. Signal d -> InletId -> Node m d -> Effect (Node m d)
-- attach signal inlet node = pure node -- FIXME: TODO

getFn :: forall state d. Node state d -> NodeFn state d
getFn (Node _ fn _) = fn


getInletsChannel :: forall state d. Node state d -> Ch.Channel (InletId /\ d)
getInletsChannel (Node _ _ (inlets_chan /\ _)) = inlets_chan


getOutletsChannel :: forall state d. Node state d -> Ch.Channel (OutletId /\ d)
getOutletsChannel (Node _ _ (_ /\ outlets_chan)) = outlets_chan


-- subscribeInlet inletId = inletsSignal >>> Signal.filter


inletsSignal :: forall state d. Node state d -> Signal (InletId /\ d)
inletsSignal =
    Ch.subscribe <<< getInletsChannel


inletsSignal' :: forall state d. Node state d -> Signal (InletId /-> d)
inletsSignal' =
    Signal.foldp (Tuple.uncurry Map.insert) Map.empty <<< inletsSignal


outletsSignal :: forall state d. Node state d -> Signal (OutletId /\ d)
outletsSignal =
    Ch.subscribe <<< getOutletsChannel


outletsSignal' :: forall state d. Node state d -> Signal (OutletId /-> d)
outletsSignal' =
    Signal.foldp (Tuple.uncurry Map.insert) Map.empty <<< outletsSignal


inletSignal :: forall state d. Node state d -> InletId -> Signal d
inletSignal node name =
    ( Ch.subscribe (getInletsChannel node)
        # Signal.filter
            (Tuple.fst >>> (==) name)
            (consumerIn /\ default node)
    ) ~> Tuple.snd


outletSignal :: forall state d. Node state d -> OutletId -> Signal d
outletSignal node name =
    ( Ch.subscribe (getOutletsChannel node)
        # Signal.filter
            (Tuple.fst >>> (==) name)
            (consumerOut /\ default node)
    ) ~> Tuple.snd


outletSignalFlipped :: forall state d. OutletId -> Node state d -> Signal d
outletSignalFlipped = flip outletSignal


getI :: forall state d. Node state d -> InletId -> Effect d
getI node name = inletSignal node name # Signal.get


getO :: forall state d. Node state d -> OutletId -> Effect d
getO node name = outletSignal node name # Signal.get


getShape :: forall state d. Node state d -> Array (InletDef d) /\ Array (OutletDef d)
getShape = getFn >>> Fn.shapeOf


getShape' :: forall state d. Node state d -> (InletId /-> Channel.Def d) /\ (OutletId /-> Channel.Def d)
getShape' = getShape >>> bimap Map.fromFoldable Map.fromFoldable


inlet :: forall state d. InletId -> Node state d -> Maybe (InletDef d)
inlet name = getFn >>> Fn.findInput ((==) name)


outlet :: forall state d. OutletId -> Node state d -> Maybe (OutletDef d)
outlet name = getFn >>> Fn.findOutput ((==) name)


inlets :: forall state d. Node state d -> Array (InletDef d)
inlets = getShape >>> Tuple.fst


inletsBy :: forall state d. (Channel.Def d -> Boolean) -> Node state d -> Array (InletDef d)
inletsBy pred = inlets >>> Array.filter (Tuple.snd >>> pred)


outlets :: forall state d. Node state d -> Array (OutletDef d)
outlets = getShape >>> Tuple.snd


outletsBy :: forall state d. (Channel.Def d -> Boolean) -> Node state d -> Array (OutletDef d)
outletsBy pred = outlets >>> Array.filter (Tuple.snd >>> pred)


dimensions :: forall state d. Node state d -> Int /\ Int
dimensions = getFn >>> Fn.dimensions


{- dimensionsBy :: forall state d. (InletDef d -> Boolean) -> (OutletDef d -> Boolean) -> Node m d -> Int /\ Int
dimensionsBy iPred oPred = getFn >>> Fn.dimensionsBy iPred oPred


dimensionsBy' :: forall state d. (Channel.Def d -> Boolean) -> Node m d -> Int /\ Int
dimensionsBy' pred = dimensionsBy (Tuple.snd >>> pred) (Tuple.snd >>> pred) -}


indexOfInlet :: forall state d. InletId -> Node state d -> Maybe Int
indexOfInlet inletName node =
    Array.elemIndex inletName $ Tuple.fst <$> inlets node


indexOfOutlet :: forall state d. OutletId -> Node state d -> Maybe Int
indexOfOutlet outletName node =
    Array.elemIndex outletName $ Tuple.fst <$> outlets node


defaultOfInlet :: forall state d. InletId -> Node state d -> Maybe d
defaultOfInlet name node = inlet name node <#> Tuple.snd <#> Channel.default


defaultOfOutlet :: forall state d. OutletId -> Node state d -> Maybe d
defaultOfOutlet name node = outlet name node <#> Tuple.snd <#> Channel.default


default :: forall state d. Node state d -> d
default (Node d _ _) = d


{- markFamily :: forall state d. Family -> Node m d -> Node m d
markFamily family (Node m default shape channels _) =
    Node m default shape channels $ Just family -}


family :: forall state d. Node state d -> Family
family = getFn >>> Fn.name


move :: forall state d d'. (d -> d') -> (d' -> d) -> Node state d -> Effect (Node state d')
move f g (Node default fn (inChannel /\ outChannel)) =
    let
        movedFn = imap f g $ Fn.mapInputsAndOutputs (map f) (map f) fn
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


-- subscribeInlet :: InletId -> (d -> Effect Unit) -> Effect Unit
-- subscribeInlet = TODO


-- changeProcess :: forall state d. Node state d -> NodeProcess state d -> Node state d
-- changeProcess (Node )


-- with :: forall state d. Node state d -> NodeProcess state d ->