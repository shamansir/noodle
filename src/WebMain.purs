module WebMain where


import Prelude (Unit, (<#>), (>>=), ($), pure, unit)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Web.App2 as App

import Noodle.Network2 (Network)
import Noodle.Network2 as Network
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit

import Toolkit.Test (toolkit)
--import Hydra.Network (network)


--app :: Network Hydra -> App' UI.Action UI.State Unit Hydra
--app :: Network Hydra -> App UI.State Hydra Hydra
app gstate nw =
    { toolkit
    -- , components
    , currentPatch : Nothing -- Just "hydra"
    , network : nw
    , patchState : gstate
    }


main :: Effect Unit
main =
    App.run $ app unit $ Network.init toolkit


--network :: Toolkit Queue Hydra -> Effect (Network Hydra)
--network toolkit =
