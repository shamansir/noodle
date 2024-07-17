module WebMain where


import Prelude (Unit, (<#>), (>>=), ($), pure, unit)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Web.App3 as App

import Noodle.Network (Network)
import Noodle.Network as Network
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Patch (Patch)

import Toolkit.Test (toolkit)
import Toolkit.Test (TestToolkit)
import Toolkit.Test (Instances, Families) as TestToolkit
--import Hydra.Network (network)


type AppState gstate repr m =
  { toolkit :: TestToolkit repr m
  , currentPatch :: Maybe (Patch gstate (TestToolkit.Instances repr m))
  , network :: Network gstate (TestToolkit.Families repr m) (TestToolkit.Instances repr m)
  , patchState :: gstate
  }


--app :: Network Hydra -> App' UI.Action UI.State Unit Hydra
--app :: Network Hydra -> App UI.State Hydra Hydra
app :: forall repr. Unit -> _ -> AppState Unit repr Effect
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
