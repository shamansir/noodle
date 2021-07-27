module Hydra.Main where


import Prelude ((#), Unit)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Data.Tuple.Nested ((/\))

import App (run) as App
import App (App)
import App.Style (Style, NodeFlow(..))
import App.Style.Quartz as Quartz

import Noodle.Network as Network
import Noodle.Network (Network)
import Noodle.Patch as Patch

import Hydra (Hydra)
import Hydra.Toolkit as Toolkit
import Hydra.UI (ui)


style :: Style
style = Quartz.style


flow :: NodeFlow
flow = Vertical


network :: Network Hydra
network =
    Network.empty
        # Network.addPatch ( "hydra" /\ Patch.empty )


app :: App Hydra
app =
    { style
    , flow
    , network
    , toolkit : Toolkit.toolkit
    , currentPatch : Just "hydra"
    , ui
    }


main :: Effect Unit
main = App.run app