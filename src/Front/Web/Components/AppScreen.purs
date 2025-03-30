module Web.Components.AppScreen where

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
import Halogen.Svg.Elements.Extra as HSX
import Halogen.Subscription as HSS

import Web.UIEvent.MouseEvent (clientX, clientY) as Mouse

import Noodle.Wiring (class Wiring)
import Noodle.Id (PatchR, FamilyR, NodeR, unsafeFamilyR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies, class FromPatchState, spawnAnyRaw, loadFromPatch) as Toolkit
import Noodle.Network (toolkit, addPatch, patches) as Network
import Noodle.Patch (make, id, name, registerRawNode, mapAllNodes, nodesCount) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (run, _runOnInletUpdates, NodeChanges, id, setState, subscribeChanges) as RawNode
import Noodle.Repr.Tagged (class ValueTagged)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Web.Bounds (getPosition) as Bounds
import Web.Components.AppScreen.State (State)
import Web.Components.AppScreen.State
    ( init
    , spawnPatch, registerPatch, indexOfPatch, currentPatch, currentPatchState, withCurrentPatch
    ) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library
import Web.Components.PatchArea as PatchArea
import Web.Components.NodeBox as NodeBox
import Web.Class.WebRenderer (class WebLocator, ConstantShift)
import Web.Class.WebRenderer (locateNext) as Web


type Slots ps fs sr cr m =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output Unit
    , patchArea :: H.Slot PatchArea.Query (PatchArea.Output ps fs sr cr m) Unit
    )


_library = Proxy :: _ "library"
_patchesBar = Proxy :: _ "patchesBar"
_patchArea = Proxy :: _ "patchArea"


data Action ps fs sr cr m
    = Initialize
    | SelectPatch Id.PatchR
    | CreatePatch
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output
    | FromPatchArea (PatchArea.Output ps fs sr cr m)


component
    :: forall query input output loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => HasFallback cr
    => WriteChannelRepr cr
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.FromPatchState tk ps sr
    => ValueTagged cr
    => Proxy loc
    -> ps
    -> Toolkit tk fs sr cr m
    -> H.Component query input output m
component ploc pstate toolkit =
    H.mkComponent
        { initialState : initialState pstate toolkit
        , render : render ploc
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction pstate
            , initialize = Just Initialize
            }
        }


initialState :: forall input tk ps fs sr cr m. ps -> Toolkit tk fs sr cr m -> input -> State tk ps fs sr cr m
initialState pstate toolkit _ = CState.init pstate toolkit


render
    :: forall loc tk ps fs sr cr m
     . Wiring m
    => WebLocator loc
    => Toolkit.HoldsFamilies sr cr m fs
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => ValueTagged cr
    => WriteChannelRepr cr
    => Proxy loc
    -> State tk ps fs sr cr m
    -> H.ComponentHTML (Action ps fs sr cr m) (Slots ps fs sr cr m) m
render ploc state =
    HH.div_
        [ HS.svg [ HSA.width 1000.0, HSA.height 1000.0 ]
            [ HS.g
                []
                (
                    [ HS.rect
                        [ HSA.width 1000.0, HSA.height 1000.0
                        , HSA.fill $ Just $ P.hColorOf $ Palette.black
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
                        ]
                        [ HH.slot _patchArea unit (PatchArea.component ploc)
                            { state : state.initPatchesFrom
                            , toolkit : Network.toolkit state.network
                            , mbPatch : CState.currentPatch state
                            }
                            FromPatchArea
                        ]
                    ]
                )
            ]
        ]
        where
            patchAreaX = Library.width + 20.0
            patchAreaY = PatchesBar.height + 15.0


handleAction
    :: forall output tk ps fs sr cr m
     . Wiring m
    => Toolkit.FromPatchState tk ps sr
    => HasFallback cr
    => ValueTagged cr
    => ps
    -> Action ps fs sr cr m
    -> H.HalogenM (State tk ps fs sr cr m) (Action ps fs sr cr m) (Slots ps fs sr cr m) output m Unit
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
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction pstate $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction pstate $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) ->
        H.tell _patchArea unit $ PatchArea.QuerySpawnNode familyR
    FromPatchArea (PatchArea.UpdatePatch nextPatch) ->
        H.modify_ $ CState.withCurrentPatch $ const nextPatch
