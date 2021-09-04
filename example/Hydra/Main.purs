module Hydra.Main where


import Prelude (Unit, (<#>), (>>=), discard)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import App (run) as App
import App (App)
import App.Style (Style, NodeFlow(..))
import App.Style.Hydra as Hydra

import Color as C
import Color.Extra as C

import Noodle.Network (Network)

import Hydra (Hydra)
import Hydra.Engine (init) as HydraE
import Hydra.Toolkit (toolkit)
import Hydra.Toolkit.UI (ui)
import Hydra.Network (network)
import Hydra.Component.State as App


style :: Style
style =
    Hydra.style
        { bg
            { fill = C.transparent }
        }


flow :: NodeFlow
flow = Vertical


app :: Network Hydra -> App App.State Hydra
app nw =
    { style
    , flow
    , toolkit
    , ui
    , currentPatch : Just "hydra"
    , network : nw
    }


main :: Effect Unit
main =
    network toolkit
        <#> app
        >>= App.run