module Hydra.Main where


import Prelude (Unit, (<#>), (>>=))

import Effect (Effect)
import Data.Maybe (Maybe(..))

import App (run) as App
import App (App')
import App.Style (Style, NodeFlow(..))
import App.Style.Hydra as Hydra

import Color.Extra (transparent) as C

import Noodle.Network (Network)

import Hydra (Hydra)
--import Hydra.Engine (init) as HydraE
import Hydra.Toolkit (toolkit)
import Hydra.Toolkit.UI (components, markings, getFlags)
import Hydra.Toolkit.UI.State (State, init) as UI
import Hydra.Toolkit.UI.Action (Action) as UI
import Hydra.Network (network)


style :: Style
style =
    Hydra.style
        { bg
            { fill = C.transparent }
        }


flow :: NodeFlow
flow = Vertical


app :: Network Hydra -> App' UI.Action UI.State Hydra
app nw =
    { style
    , flow
    , toolkit
    , components
    , markings
    , getFlags
    , currentPatch : Just "hydra"
    , network : nw
    , patchState : UI.init
    }


main :: Effect Unit
main =
    network toolkit
        <#> app
        >>= App.run