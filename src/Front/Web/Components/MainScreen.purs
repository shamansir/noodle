module Web.Components.MainScreen where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Control.Monad.State (get, put, modify, modify_) as State

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (toUnfoldable) as Map
import Data.Tuple.Nested ((/\), type (/\))

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

import Noodle.Id (PatchR, FamilyR, NodeR, unsafeFamilyR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies, class FromPatchState, spawnAnyRaw, loadFromPatch) as Toolkit
import Noodle.Network (toolkit, addPatch, patches) as Network
import Noodle.Patch (make, id, name, registerRawNode, mapAllNodes) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, setState) as RawNode
import Noodle.Repr.Tagged (class ValueTagged)

import Web.State (State)
import Web.State (init, spawnPatch, registerPatch, lastPatchIndex, currentPatch, currentPatchState, withCurrentPatch) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.NodeBox as NodeBox
import Web.Class.WebRenderer (class WebLocator, ConstantShift)


type Slots =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output Unit
    , nodeBox :: forall q. H.Slot q NodeBox.Output Id.NodeR
    )


_patchesBar = Proxy :: _ "patchesBar"
_library = Proxy :: _ "library"
_nodeBox = Proxy :: _ "nodeBox"


type Locator = ConstantShift -- TODO: move to some root App config?


data Action
    = Initialize
    | SelectPatch Id.PatchR
    | CreatePatch
    | SpawnNode Id.FamilyR
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromNodeBox Id.NodeR NodeBox.Output


component
    :: forall query input output tk ps fs sr cr mi m
     . MonadEffect m
    => Toolkit.HoldsFamilies sr cr mi fs
    => Toolkit.FromPatchState tk ps sr
    => ValueTagged cr
    => ps
    -> Toolkit tk fs sr cr mi
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


initialState :: forall input tk ps fs sr cr mi. ps -> Toolkit tk fs sr cr mi -> input -> State Locator tk ps fs sr cr mi
initialState pstate toolkit _ = CState.init pstate toolkit


render
    :: forall tk ps fs sr cr mi m
     . Toolkit.HoldsFamilies sr cr mi fs
    => State _ tk ps fs sr cr mi
    -> H.ComponentHTML Action Slots m
render state =
    HH.div_
        [ HS.svg [ HSA.width 1000.0, HSA.height 1000.0 ]
            [ HS.g
                []
                (
                    [ HS.rect
                        [ HSA.width 1000.0, HSA.height 1000.0
                        , HSA.fill $ Just $ HC.RGB 16 15 15
                        , HSA.stroke $ Just $ HC.RGB 100 100 100
                        ]
                    , HH.slot _patchesBar unit PatchesBar.component
                        { patches : map Patch.name <$> (Map.toUnfoldable $ Network.patches state.network)
                        , selected : _.id <$> state.currentPatch
                        }
                        FromPatchesBar
                    , HH.slot _library unit Library.component
                        { families : Toolkit.families $ Network.toolkit state.network }
                        FromLibrary
                    ]
                    <> nodeBoxesSlots
                )
            ]
        ]
    where
        nodeBoxesSlots = (CState.currentPatch state <#> Patch.mapAllNodes nodeBoxSlot) # fromMaybe []
        nodeBoxSlot rawNode =
            HH.slot _nodeBox (RawNode.id rawNode) NodeBox.component
                { node : rawNode }
                $ FromNodeBox
                $ RawNode.id rawNode


handleAction
    :: forall output tk ps fs sr cr mi m
     . MonadEffect m
    => Toolkit.FromPatchState tk ps sr
    => ValueTagged cr
    => ps
    -> Action
    -> H.HalogenM (State _ tk ps fs sr cr mi) Action Slots output m Unit
handleAction pstate = case _ of
    Initialize -> do
        firstPatch <- H.lift $ Patch.make "Patch 1" pstate
        State.modify_ $ CState.registerPatch firstPatch
        handleAction pstate $ SpawnNode $ Id.unsafeFamilyR "voronoi"
    CreatePatch -> do
        state <- State.get
        newPatch <- H.lift $ CState.spawnPatch state
        State.modify_ $ CState.registerPatch newPatch
        handleAction pstate $ FromPatchesBar $ PatchesBar.SelectPatch $ Patch.id newPatch
    SelectPatch patchR -> do
        state <- State.get
        H.modify_ _ { currentPatch = Just { id : patchR, index : CState.lastPatchIndex state + 1 } }
    SpawnNode familyR -> do
        state <- State.get
        let toolkit = Network.toolkit state.network
        (mbRawNode :: Maybe (Raw.Node sr cr mi)) <- H.lift $ Toolkit.spawnAnyRaw familyR toolkit
        case mbRawNode of
            Just rawNode -> do
                (mbPatchState :: Maybe ps) <- CState.currentPatchState =<< State.get
                let (mbNodeState :: Maybe sr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) familyR
                case mbNodeState of
                    Just nextState -> rawNode # RawNode.setState nextState
                    Nothing -> pure unit
                H.modify_ $ CState.withCurrentPatch $ Patch.registerRawNode rawNode
                pure unit
            Nothing -> pure unit
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction pstate $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction pstate $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) -> do
        handleAction pstate $ SpawnNode familyR
    FromNodeBox nodeR NodeBox.Output ->
        pure unit
