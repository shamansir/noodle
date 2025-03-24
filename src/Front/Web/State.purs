module Web.State where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))

import Data.Map (Map)
import Data.Map (empty, insert) as Map

import Noodle.Id (PatchR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit.Families (Families)
import Noodle.Patch (Patch)
import Noodle.Patch (id, make) as Patch
import Noodle.Network (Network)
import Noodle.Network (init, patchesCount, addPatch) as Network


type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    , initPatchesFrom :: ps
    , patchIdToIndex :: Map Id.PatchR Int
    , currentPatch :: Maybe { index :: Int, id :: Id.PatchR }
    }


empty :: forall loc tk ps fs sr cr m. ps -> Toolkit tk fs sr cr m -> State loc tk ps fs sr cr m
empty fromState toolkit =
    { network : Network.init toolkit
    , initPatchesFrom : fromState
    , patchIdToIndex : Map.empty
    , currentPatch : Nothing
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
        nextPatchIndex = patchesCount + 1
        nextNW = s.network # Network.addPatch newPatch
    in
        s
            { currentPatch = Just { index : nextPatchIndex, id : Patch.id newPatch }
            , patchIdToIndex = s.patchIdToIndex # Map.insert (Patch.id newPatch) nextPatchIndex
            , network = nextNW
            }


lastPatchIndex :: forall loc tk ps fs sr cr m. State loc tk ps fs sr cr m -> Int
lastPatchIndex s = Network.patchesCount s.network