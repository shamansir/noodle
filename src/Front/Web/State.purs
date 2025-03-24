module Web.State where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))

import Noodle.Id (PatchR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit.Families (Families)
import Noodle.Network (Network)
import Noodle.Network (init) as Network


type State loc (tk :: ToolkitKey) ps (fs :: Families) sr cr m =
    { network :: Network tk ps fs sr cr m
    , currentPatch :: Maybe Id.PatchR
    }


empty :: forall loc tk ps fs sr cr m. Toolkit tk fs sr cr m -> State loc tk ps fs sr cr m
empty toolkit =
    { network : Network.init toolkit
    , currentPatch : Nothing
    }