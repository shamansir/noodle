module Web.Components.AppScreen.State where

import Prelude

import Effect.Class (class MonadEffect)

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Traversable (traverse)
import Data.Text.Format (Tag) as T

import Noodle.Id (PatchR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (class InitPatchState, initPatch) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (id, make, getState) as Patch
import Noodle.Network (Network)
import Noodle.Network (init, patchesCount, patch, addPatch, withPatch) as Network

import HydraTk.Lang.Program (Program) as Hydra


type State (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { size :: Maybe { width :: Number, height :: Number }
    , zoom :: Number
    , bgOpacity :: Number
    , shiftPressed :: Boolean
    , network :: Network tk ps fs sr cr m
    , patchIdToIndex :: Map Id.PatchR PatchIndex
    , currentPatch :: Maybe { index :: PatchIndex, id :: Id.PatchR }
    , currentPatchState :: Maybe ps -- FIXME: it's data duplication, but we store it here, because getting it from Network is effectful and we need it on `render` for the user's Patch component
    , statusBarContent :: Maybe T.Tag
    , hydraProgram :: Maybe Hydra.Program -- FIXME : should be created by Hydra itself
    }


newtype PatchIndex = PatchIndex Int


init :: forall tk ps fs sr cr m. Toolkit tk fs sr cr m -> State tk ps fs sr cr m
init toolkit =
    { size : Nothing
    , zoom : 1.0
    , bgOpacity : 0.1
    , shiftPressed : false
    , network : Network.init toolkit
    , patchIdToIndex : Map.empty
    , currentPatch : Nothing
    , currentPatchState : Nothing
    , statusBarContent : Nothing
    , hydraProgram : Nothing
    }


spawnPatch :: forall tk ps fs sr cr mp m. MonadEffect m => Toolkit.InitPatchState tk ps m => State tk ps fs sr cr mp -> m { state :: ps, patch :: Patch ps fs sr cr mp }
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    patchState <- Toolkit.initPatch (Proxy :: _ tk)
    Patch.make ("Patch " <> show nextPatchIndex) patchState
        <#> \newPatch -> { patch : newPatch, state : patchState }


registerPatch :: forall tk ps fs sr cr m. ps -> Patch ps fs sr cr m -> State tk ps fs sr cr m -> State tk ps fs sr cr m
registerPatch patchState newPatch s =
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = PatchIndex $ patchesCount + 1
        nextNW = s.network # Network.addPatch newPatch
    in
        s
            { currentPatch = Just { index : nextPatchIndex, id : Patch.id newPatch } -- FIXME: make patch current in a separate function
            , currentPatchState = Just patchState
            , patchIdToIndex = s.patchIdToIndex # Map.insert (Patch.id newPatch) nextPatchIndex
            , network = nextNW
            }


lastPatchIndex :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> PatchIndex
lastPatchIndex s = PatchIndex $ Network.patchesCount s.network


indexOfPatch :: forall tk ps fs sr cr m. Id.PatchR -> State tk ps fs sr cr m -> Maybe PatchIndex
indexOfPatch patchR = _.patchIdToIndex >>> Map.lookup patchR


patch :: forall tk ps fs sr cr m. Id.PatchR -> State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.currentPatch <#> _.id >>= flip patch s


currentPatchId :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe Id.PatchR
currentPatchId s = s.currentPatch <#> _.id


currentPatchState :: forall tk ps fs sr cr mp m. MonadEffect m => State tk ps fs sr cr mp -> m (Maybe ps)
currentPatchState = traverse Patch.getState <<< currentPatch


currentPatchState' :: forall tk ps fs sr cr m. State tk ps fs sr cr m -> Maybe ps
currentPatchState' = _.currentPatchState


withPatch :: forall tk ps fs sr cr m. Id.PatchR -> (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


replacePatch :: forall tk ps fs sr cr m. Id.PatchR -> Patch ps fs sr cr m -> State tk ps fs sr cr m -> State tk ps fs sr cr m
replacePatch patchR = withPatch patchR <<< const


withCurrentPatch :: forall tk ps fs sr cr m. (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State tk ps fs sr cr m -> State tk ps fs sr cr m
withCurrentPatch f s = case s.currentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s