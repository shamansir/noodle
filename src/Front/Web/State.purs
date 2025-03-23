module Front.Web.State where

import Prelude

import Effect.Class (class MonadEffect)

import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Network (Network)
import Noodle.Network (init, patch, addPatch, withPatch, patchesCount, toolkit) as Network
import Noodle.Patch (make, id, registerRawNode, registerRawNode', getState) as Patch


type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    }


empty :: forall loc tk ps fs sr cr m. MonadEffect m => Toolkit tk fs sr cr m -> State loc tk ps fs sr cr m
empty toolkit = { network : Network.init toolkit }


init :: forall loc tk ps fs sr cr m. MonadEffect m => ps -> Toolkit tk fs sr cr m -> m (State loc tk ps fs sr cr m)
init state toolkit = do
    firstPatch <- Patch.make "Patch 1" state
    pure
        { network : Network.init toolkit # Network.addPatch firstPatch
        }