module Web.Components.MainScreen where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Control.Monad.State (get, put, modify, modify_) as State

import Data.Maybe (Maybe(..))
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

import Noodle.Id (PatchR, FamilyR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Network (toolkit, addPatch, patches) as Network
import Noodle.Patch (make, id, name) as Patch

import Web.State (State)
import Web.State (empty, spawnPatch, registerPatch, lastPatchIndex) as CState
import Web.Components.PatchesBar as PatchesBar
import Web.Components.Library as Library

type Slots =
    ( patchesBar :: forall q. H.Slot q PatchesBar.Output Unit
    , library :: forall q. H.Slot q Library.Output Unit
    )

_patchesBar = Proxy :: _ "patchesBar"
_library = Proxy :: _ "library"

data Action
    = Initialize
    | SelectPatch Id.PatchR
    | CreatePatch
    | SpawnNode Id.FamilyR
    | FromPatchesBar PatchesBar.Output
    | FromLibrary Library.Output


component
    :: forall query input output ps tk fs sr cr mi m
     . MonadEffect m
    => Toolkit.HoldsFamilies sr cr mi fs
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


initialState :: forall input tk ps fs sr cr mi. ps -> Toolkit tk fs sr cr mi -> input -> State _ tk ps fs sr cr mi
initialState pstate toolkit _ = CState.empty pstate toolkit


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
            ]
        ]


handleAction :: forall output tk ps fs sr cr mi m. MonadEffect m => ps -> Action -> H.HalogenM (State _ tk ps fs sr cr mi) Action Slots output m Unit
handleAction pstate = case _ of
    Initialize -> do
        firstPatch <- H.lift $ Patch.make "Patch 1" pstate
        State.modify_ $ \s -> s { network = s.network # Network.addPatch firstPatch }
    CreatePatch -> do
        state <- State.get
        newPatch <- H.lift $ CState.spawnPatch state
        State.modify_ $ CState.registerPatch newPatch
        handleAction pstate $ FromPatchesBar $ PatchesBar.SelectPatch $ Patch.id newPatch
    SelectPatch patchR -> do
        state <- State.get
        H.modify_ _ { currentPatch = Just { id : patchR, index : CState.lastPatchIndex state + 1 } }
    SpawnNode familyR -> do
        pure unit -- TODO
    FromPatchesBar (PatchesBar.SelectPatch patchR) -> do
        handleAction pstate $ SelectPatch patchR
    FromPatchesBar PatchesBar.CreatePatch -> do
        handleAction pstate $ CreatePatch
    FromLibrary (Library.SelectFamily familyR) -> do
        handleAction pstate $ SpawnNode familyR
