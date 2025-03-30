module Web.Components.PatchArea where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Extra (whenJust)

import Signal (Signal, (~>))
import Signal (runSignal) as Signal

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, lookup, insert, toUnfoldable, size) as Map
import Data.Map.Extra (update') as MapX
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (sortWith) as Array
import Data.Array ((:))
import Data.Int (toNumber) as Int
import Data.Bifunctor (lmap)
import Data.Functor.Extra ((<$$>), (<##>))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Elements as HS
import Halogen.Subscription as HSS

import Web.UIEvent.MouseEvent (clientX, clientY) as Mouse

import Noodle.Wiring (class Wiring)
import Noodle.Id (PatchR, FamilyR, NodeR, unsafeFamilyR) as Id
import Noodle.Patch (Patch)
import Noodle.Patch (getState) as Patch
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies, class FromPatchState, spawnAnyRaw, loadFromPatch) as Toolkit
import Noodle.Network (toolkit, addPatch, patches) as Network
import Noodle.Patch (make, id, name, registerRawNode, mapAllNodes) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (run, _runOnInletUpdates, NodeChanges, id, setState, subscribeChanges) as RawNode
import Noodle.Repr.Tagged (class ValueTagged)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Web.Bounds (Bounds)
import Web.Bounds (getPosition) as Bounds
import Web.Components.NodeBox as NodeBox
import Web.Class.WebRenderer (class WebLocator, ConstantShift, firstLocation, locateNext)
import Web.Class.WebRenderer (locateNext) as Web


newtype NodeZIndex = ZIndex Int
derive newtype instance Eq NodeZIndex
derive newtype instance Ord NodeZIndex
instance Bounded NodeZIndex where
    top = ZIndex 1000
    bottom = ZIndex 0


type Locator = ConstantShift -- TODO: move to some root App config?


type Slots sr cr =
    ( nodeBox :: H.Slot (NodeBox.Query sr cr) NodeBox.Output Id.NodeR
    )


_nodeBox = Proxy :: _ "nodeBox"


defaultPosition = { left : 0.0, top : 0.0 }


type State loc tk ps fs sr cr m =
    { lastLocation :: loc
    , toolkit :: Toolkit tk fs sr cr m
    , patch :: Patch ps fs sr cr m
    , nodesBounds :: Map Id.NodeR (Bounds /\ NodeZIndex)
    , draggingNode :: Maybe Id.NodeR
    }


type Input tk ps fs sr cr m =
    { state :: ps
    , patch :: Patch ps fs sr cr m
    , toolkit :: Toolkit tk fs sr cr m
    }


data Action sr cr
    = Initialize
    | SpawnNode Id.FamilyR
    | PassUpdate Id.NodeR (RawNode.NodeChanges sr cr)
    | PatchAreaMouseMove { x :: Number, y :: Number }
    | PatchAreaClick
    | FromNodeBox Id.NodeR NodeBox.Output


data Output ps fs sr cr m
    = UpdatePatch (Patch ps fs sr cr m)


data Query a
    = QuerySpawnNode Id.FamilyR a


component
    :: forall loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => WriteChannelRepr cr
    => Toolkit.HoldsFamilies sr cr m fs
    => ValueTagged cr
    => Proxy loc
    -> H.Component Query (Input tk ps fs sr cr m) (Output ps fs sr cr m) m
component ploc =
    H.mkComponent
        { initialState : initialState ploc
        , render : render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }


initialState :: forall loc tk fs ps sr cr m. WebLocator loc => Proxy loc -> Input tk ps fs sr cr m -> State loc tk ps fs sr cr m
initialState _ { patch, toolkit } =
    { lastLocation : firstLocation
    , patch
    , toolkit
    , nodesBounds : Map.empty
    , draggingNode : Nothing
    }


render
    :: forall loc tk ps fs sr cr m
     . MonadEffect m
    => Toolkit.HoldsFamilies sr cr m fs
    => WriteChannelRepr cr
    => State loc tk ps fs sr cr m
    -> H.ComponentHTML (Action sr cr) (Slots sr cr) m
render state =
    HS.g
        []
        [ HS.rect
            [ HSA.width 1000.0, HSA.height 1000.0
            , HSA.fill $ Just $ P.hColorOf $ Palette.black
            , HE.onClick $ const PatchAreaClick
            , HE.onMouseMove \mevt -> PatchAreaMouseMove
                { x : (Int.toNumber $ Mouse.clientX mevt)
                , y : (Int.toNumber $ Mouse.clientY mevt)
                }
            ]
        , HS.g
            [ HE.onClick $ const PatchAreaClick
            ]
            $ nodeBoxesSlots
        ]
    where
        nodeBoxesSlots = state.patch # Patch.mapAllNodes findRenderData # Array.sortWith _.zIndex <#> nodeBoxSlot
        findRenderData rawNode =
            let
                nodeR = RawNode.id rawNode
                (position /\ zIndex) =
                    fromMaybe (defaultPosition /\ top)
                         $ lmap Bounds.getPosition
                        <$> findBounds nodeR state
            in
                { rawNode, position, zIndex }
        nodeBoxSlot { rawNode, position } =
            let
                nodeR = RawNode.id rawNode
            in HH.slot _nodeBox nodeR NodeBox.component
                { node : rawNode
                , position
                }
                $ FromNodeBox nodeR


handleAction
    :: forall loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => ValueTagged cr
    => Action sr cr
    -> H.HalogenM (State loc tk ps fs sr cr m) (Action sr cr) (Slots sr cr) (Output ps fs sr cr m) m Unit
handleAction = case _ of
    Initialize -> pure unit
    SpawnNode familyR -> do
        state <- State.get
        (mbRawNode :: Maybe (Raw.Node sr cr m)) <- H.lift $ Toolkit.spawnAnyRaw familyR state.toolkit
        whenJust mbRawNode \rawNode -> do
            let
                nodeR = RawNode.id rawNode
                nodeRect = { width : 300.0, height : 70.0 }
                nextLoc /\ nodePos = Web.locateNext state.lastLocation nodeRect
            H.lift $ RawNode._runOnInletUpdates rawNode
            (patchState :: ps) <- (_.patch >>> Patch.getState) =<< State.get
            let (mbNodeState :: Maybe sr) = Toolkit.loadFromPatch (Proxy :: _ tk) familyR patchState
            whenJust mbNodeState
                \nextState -> rawNode # RawNode.setState nextState

            _ <- H.subscribe =<< do
                { emitter, listener } <- H.liftEffect HSS.create
                H.liftEffect
                    $  Signal.runSignal
                    $  RawNode.subscribeChanges rawNode
                    ~> PassUpdate nodeR
                    ~> HSS.notify listener
                pure emitter

            H.modify_
                $   (\s -> s { patch = Patch.registerRawNode rawNode s.patch })
                >>> storeBounds nodeR
                        { left : nodePos.left, top : nodePos.top
                        , width : nodeRect.width, height : nodeRect.height
                        }
                >>> _ { lastLocation = nextLoc }

            H.lift $ RawNode.run rawNode

            State.get <#> _.patch >>= (H.raise <<< UpdatePatch)
    PatchAreaMouseMove { x, y } -> do
        state <- State.get
        whenJust state.draggingNode
            \nodeR -> do
                H.modify_ $ updatePosition nodeR { left : x, top : y }
    PatchAreaClick -> do
        state <- State.get
        whenJust state.draggingNode
            \nodeR -> do
                State.put $ state { draggingNode = Nothing }
                H.tell _nodeBox nodeR NodeBox.QueryDragEnd
    FromNodeBox nodeR NodeBox.HeaderWasClicked -> do
        state <- State.get
        case state.draggingNode of
            Nothing -> do
                State.put $ state { draggingNode = Just nodeR }
                H.tell _nodeBox nodeR NodeBox.QueryDragStart
            Just otherNodeR -> do
                State.put $ state { draggingNode = Nothing }
                H.tell _nodeBox otherNodeR NodeBox.QueryDragEnd
    FromNodeBox nodeR (NodeBox.InletWasClicked inletDef) -> do
        pure unit
    FromNodeBox nodeR (NodeBox.OutletWasClicked outletDef) -> do
        pure unit
    PassUpdate nodeR update ->
        H.tell _nodeBox nodeR $ NodeBox.QueryUpdate update


handleQuery
    :: forall loc tk ps fs sr cr m a
     . Wiring m
    => WebLocator loc
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => ValueTagged cr
    => Query a
    -> H.HalogenM (State loc tk ps fs sr cr m) (Action sr cr) (Slots sr cr) (Output ps fs sr cr m) m (Maybe a)
handleQuery = case _ of
    QuerySpawnNode familyR a -> do
        handleAction $ SpawnNode familyR
        pure $ Just a


findBounds :: forall loc tk ps fs sr cr m. Id.NodeR -> State loc tk ps fs sr cr m -> Maybe (Bounds /\ NodeZIndex)
findBounds nodeR = _.nodesBounds >>> Map.lookup nodeR


storeBounds :: forall loc tk ps fs sr cr m. Id.NodeR -> Bounds -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
storeBounds nodeR bounds s = s
    { nodesBounds
        = s.nodesBounds
            # Map.insert nodeR
                (bounds /\ (ZIndex $ Map.size s.nodesBounds))
    }


updatePosition :: forall loc tk ps fs sr cr m. Id.NodeR -> { left :: Number, top :: Number } -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
updatePosition nodeR { left, top } s = s
    { nodesBounds
        = s.nodesBounds
            # MapX.update' (lmap $ _ { left = left, top = top }) nodeR
    }