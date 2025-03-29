module Web.Components.MainScreen where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Rec.Class (class MonadRec)

import Signal (Signal, (~>))
import Signal (runSignal) as Signal

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (toUnfoldable) as Map
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (sortWith) as Array
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

import Web.Bounds (getPosition) as Bounds
import Web.State (State)
import Web.State
    ( init
    , spawnPatch, registerPatch, indexOfPatch, currentPatch, currentPatchState, withCurrentPatch
    , defaultPosition, findBounds, storeBounds, updatePosition
    ) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.NodeBox as NodeBox
import Web.Class.WebRenderer (class WebLocator, ConstantShift)
import Web.Class.WebRenderer (locateNext) as Web


type Slots sr cr =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output Unit
    , nodeBox :: H.Slot (NodeBox.Query sr cr) NodeBox.Output Id.NodeR
    )


_patchesBar = Proxy :: _ "patchesBar"
_library = Proxy :: _ "library"
_nodeBox = Proxy :: _ "nodeBox"


type Locator = ConstantShift -- TODO: move to some root App config?


data Action sr cr
    = Initialize
    | SelectPatch Id.PatchR
    | CreatePatch
    | SpawnNode Id.FamilyR
    | PassUpdate Id.NodeR (RawNode.NodeChanges sr cr)
    | PatchAreaMouseMove { x :: Number, y :: Number }
    | PatchAreaClick
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromNodeBox Id.NodeR NodeBox.Output


component
    :: forall query input output tk ps fs sr cr m
     . Wiring m
    => HasFallback cr
    => WriteChannelRepr cr
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.FromPatchState tk ps sr
    => ValueTagged cr
    => ps
    -> Toolkit tk fs sr cr m
    -> H.Component query input output m
component pstate toolkit =
    H.mkComponent
        { initialState : initialState pstate toolkit
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction pstate
            , initialize = Just Initialize
            }
        }


initialState :: forall input tk ps fs sr cr m. ps -> Toolkit tk fs sr cr m -> input -> State Locator tk ps fs sr cr m
initialState pstate toolkit _ = CState.init pstate toolkit


render
    :: forall tk ps fs sr cr m
     . MonadEffect m
    => Toolkit.HoldsFamilies sr cr m fs
    => WriteChannelRepr cr
    => State _ tk ps fs sr cr m
    -> H.ComponentHTML (Action sr cr) (Slots sr cr) m
render state =
    HH.div_
        [ HS.svg [ HSA.width 1000.0, HSA.height 1000.0 ]
            [ HS.g
                []
                (
                    [ HS.rect
                        [ HSA.width 1000.0, HSA.height 1000.0
                        , HSA.fill $ Just $ P.hColorOf $ Palette.black
                        , HE.onClick $ const PatchAreaClick
                        , HE.onMouseMove \mevt -> PatchAreaMouseMove
                            { x : (Int.toNumber $ Mouse.clientX mevt) - patchAreaX
                            , y : (Int.toNumber $ Mouse.clientY mevt) - patchAreaY
                            }
                        ]
                    , HH.slot _patchesBar unit PatchesBar.component
                        { patches : map Patch.name <$> (Map.toUnfoldable $ Network.patches state.network)
                        , selected : _.id <$> state.currentPatch
                        }
                        FromPatchesBar
                    , HH.slot _library unit Library.component
                        { families : Toolkit.families $ Network.toolkit state.network }
                        FromLibrary
                    , HS.g
                        [ HSA.transform [ HSA.Translate patchAreaX patchAreaY ]
                        , HE.onClick $ const PatchAreaClick
                        ]
                        nodeBoxesSlots
                    ]
                )
            ]
        ]
    where
        patchAreaX = Library.width + 20.0
        patchAreaY = PatchesBar.height + 15.0
        nodeBoxesSlots = (CState.currentPatch state <#> Patch.mapAllNodes findRenderData <#> Array.sortWith _.zIndex <##> nodeBoxSlot) # fromMaybe []
        findRenderData rawNode =
            let
                nodeR = RawNode.id rawNode
                (position /\ zIndex) =
                    fromMaybe (CState.defaultPosition /\ top)
                         $ lmap Bounds.getPosition
                        <$> CState.findBounds nodeR state
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
    :: forall output loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => ValueTagged cr
    => ps
    -> Action sr cr
    -> H.HalogenM (State loc tk ps fs sr cr m) (Action sr cr) (Slots sr cr) output m Unit
handleAction pstate = case _ of
    Initialize -> do
        firstPatch <- H.lift $ Patch.make "Patch 1" pstate
        State.modify_ $ CState.registerPatch firstPatch
    CreatePatch -> do
        state <- State.get
        newPatch <- H.lift $ CState.spawnPatch state
        State.modify_ $ CState.registerPatch newPatch
        handleAction pstate $ FromPatchesBar $ PatchesBar.SelectPatch $ Patch.id newPatch
    SelectPatch patchR -> do
        state <- State.get
        H.modify_ _
            { currentPatch =
                CState.indexOfPatch patchR state
                    <#> (\pIndex -> { id : patchR, index : pIndex })
            }
    SpawnNode familyR -> do
        state <- State.get
        let toolkit = Network.toolkit state.network
        (mbRawNode :: Maybe (Raw.Node sr cr m)) <- H.lift $ Toolkit.spawnAnyRaw familyR toolkit
        case mbRawNode of
            Just rawNode -> do
                let
                    nodeR = RawNode.id rawNode
                    nodeRect = { width : 300.0, height : 70.0 }
                    nextLoc /\ nodePos = Web.locateNext state.lastLocation nodeRect
                H.lift $ RawNode._runOnInletUpdates rawNode
                (mbPatchState :: Maybe ps) <- CState.currentPatchState =<< State.get
                let (mbNodeState :: Maybe sr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) familyR
                case mbNodeState of
                    Just nextState -> rawNode # RawNode.setState nextState
                    Nothing -> pure unit

                _ <- H.subscribe =<< do
                    { emitter, listener } <- H.liftEffect HSS.create
                    H.liftEffect
                        $  Signal.runSignal
                        $  RawNode.subscribeChanges rawNode
                        ~> PassUpdate nodeR
                        ~> HSS.notify listener
                    pure emitter
                H.modify_
                    $   (CState.withCurrentPatch $ Patch.registerRawNode rawNode)
                    >>> CState.storeBounds nodeR
                            { left : nodePos.left, top : nodePos.top
                            , width : nodeRect.width, height : nodeRect.height
                            }
                    >>> _ { lastLocation = nextLoc }
                H.lift $ RawNode.run rawNode
            Nothing -> pure unit
    PatchAreaMouseMove { x, y } -> do
        state <- State.get
        case state.draggingNode of
            Just nodeR -> do
                H.modify_ $ CState.updatePosition nodeR { left : x, top : y }
            Nothing -> pure unit
    PatchAreaClick -> do
        state <- State.get
        case state.draggingNode of
            Just nodeR -> do
                State.put $ state { draggingNode = Nothing }
                H.tell _nodeBox nodeR NodeBox.QueryDragEnd
            Nothing -> pure unit
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction pstate $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction pstate $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) -> do
        handleAction pstate $ SpawnNode familyR
    FromNodeBox nodeR NodeBox.HeaderWasClicked -> do
        state <- State.get
        case state.draggingNode of
            Nothing -> do
                State.put $ state { draggingNode = Just nodeR }
                H.tell _nodeBox nodeR NodeBox.QueryDragStart
            Just otherNodeR -> do
                State.put $ state { draggingNode = Nothing }
                H.tell _nodeBox otherNodeR NodeBox.QueryDragEnd
    PassUpdate nodeR update ->
        H.tell _nodeBox nodeR $ NodeBox.QueryUpdate update
