module Web.State where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))
import Data.Bounded (class Bounded)

import Data.Map (Map)
import Data.Map (empty, insert, lookup, size) as Map
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (PatchR, NodeR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (id, make, getState) as Patch
import Noodle.Network (Network)
import Noodle.Network (init, patchesCount, patch, addPatch, withPatch) as Network

import Web.Class.WebRenderer (class WebLocator, firstLocation)
import Web.Bounds (Bounds)


type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    , initPatchesFrom :: ps
    , patchIdToIndex :: Map Id.PatchR PatchIndex
    , lastLocation :: loc
    , currentPatch :: Maybe { index :: PatchIndex, id :: Id.PatchR }
    , nodesBounds :: Map Id.NodeR (Bounds /\ ZIndex)
    }


newtype PatchIndex = PatchIndex Int
newtype ZIndex = ZIndex Int


derive newtype instance Eq ZIndex
derive newtype instance Ord ZIndex


instance Bounded ZIndex where
    top = ZIndex 1000
    bottom = ZIndex 0


init :: forall loc tk ps fs sr cr m. WebLocator loc => ps -> Toolkit tk fs sr cr m -> State loc tk ps fs sr cr m
init patchState toolkit =
    { network : Network.init toolkit
    , initPatchesFrom : patchState
    , patchIdToIndex : Map.empty
    , currentPatch : Nothing
    , lastLocation : firstLocation
    , nodesBounds : Map.empty
    }


spawnPatch :: forall loc tk ps fs sr cr mp m. MonadEffect m => State loc tk ps fs sr cr mp -> m (Patch ps fs sr cr mp)
spawnPatch s = do
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = patchesCount + 1
    Patch.make ("Patch " <> show nextPatchIndex) s.initPatchesFrom


registerPatch :: forall loc tk ps fs sr cr m. Patch ps fs sr cr m -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
registerPatch newPatch s =
    let
        patchesCount = s.network # Network.patchesCount
        nextPatchIndex = PatchIndex $ patchesCount + 1
        nextNW = s.network # Network.addPatch newPatch
    in
        s
            { currentPatch = Just { index : nextPatchIndex, id : Patch.id newPatch } -- FIXME: make patch current in a separate function
            , patchIdToIndex = s.patchIdToIndex # Map.insert (Patch.id newPatch) nextPatchIndex
            , network = nextNW
            }


lastPatchIndex :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> PatchIndex
lastPatchIndex s = PatchIndex $ Network.patchesCount s.network


indexOfPatch :: forall loc tk ps fs sr cr m. Id.PatchR -> State loc tk ps fs sr cr m -> Maybe PatchIndex
indexOfPatch patchR = _.patchIdToIndex >>> Map.lookup patchR


patch :: forall loc tk ps fs sr cr m. Id.PatchR -> State loc tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
patch patchR = _.network >>> Network.patch patchR


currentPatch :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Maybe (Patch ps fs sr cr m)
currentPatch s = s.currentPatch <#> _.id >>= flip patch s


currentPatchId :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Maybe Id.PatchR
currentPatchId s = s.currentPatch <#> _.id


currentPatchState :: forall loc tk ps fs sr cr mp m. MonadEffect m => State loc tk ps fs sr cr mp -> m (Maybe ps)
currentPatchState = traverse Patch.getState <<< currentPatch


withPatch :: forall loc tk ps fs sr cr m. Id.PatchR -> (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
withPatch patchR f s = s { network = Network.withPatch patchR f s.network }


withCurrentPatch :: forall loc tk ps fs sr cr m. (Patch ps fs sr cr m -> Patch ps fs sr cr m) -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
withCurrentPatch f s = case s.currentPatch <#> _.id of
    Just curPatchR -> withPatch curPatchR f s
    Nothing -> s


defaultPosition = { left : 0.0, top : 0.0 }


findBounds :: forall loc tk ps fs sr cr m. Id.NodeR -> State loc tk ps fs sr cr m -> Maybe (Bounds /\ ZIndex)
findBounds nodeR = _.nodesBounds >>> Map.lookup nodeR


storeBounds :: forall loc tk ps fs sr cr m. Id.NodeR -> Bounds -> State loc tk ps fs sr cr m -> State loc tk ps fs sr cr m
storeBounds nodeR bounds s = s
    { nodesBounds
        = s.nodesBounds
            # Map.insert nodeR
                (bounds /\ (ZIndex $ Map.size s.nodesBounds))
    }


-- storeBounds =