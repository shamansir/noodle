module JustSum.Network where


import Prelude ((#), Unit)

import Effect (Effect)

import Data.Tuple.Nested ((/\))

import App (run) as App
import App (App)
import App.Style (Style, NodeFlow(..))
import App.Style.Quartz as Quartz

import Noodle.Network as Network
import Noodle.Network (Network)
import Noodle.Patch as Patch

import JustSum.Toolkit as Toolkit


style :: Style
style = Quartz.style


flow :: NodeFlow
flow = Vertical


network :: Network Toolkit.Data
network = Network.empty # Network.addPatch ( "base" /\ Patch.empty )


app :: App Toolkit.Data
app =
    { style
    , flow
    , network
    , toolkit : Toolkit.toolkit
    }


main :: Effect Unit
main = App.run app