module WebMain where


import Prelude (Unit, (<#>), (>>=), ($), pure)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import App (run) as App
import App (App')
import App.Style (Style, NodeFlow(..))
import App.Style.Hydra as Hydra

import Color.Extra (transparent) as C

import Noodle.Network (Network)
import Noodle.Network as Network
import Noodle.Toolkit (Toolkit)

-- import Hydra (Hydra)
-- import Toolkit.Hydra (toolkit)
-- import Hydra.Toolkit.UI (components, markings, getFlags)
-- import Hydra.Toolkit.UI.State (State, init) as UI
-- import Hydra.Toolkit.UI.Action (Action) as UI
-- import Hydra.Network (network)


import Toolkit.Hydra.Op (Hydra)
import Toolkit.Hydra (toolkit)
import Toolkit.Hydra.UI (components, markings, getFlags)
import Toolkit.Hydra.UI.State (State, init) as UI
import Toolkit.Hydra.UI.Action (Action) as UI
--import Hydra.Network (network)


style :: Style
style =
    Hydra.style
        { bg
            { fill = C.transparent }
        }


flow :: NodeFlow
flow = Vertical


app :: Network Unit Hydra -> App' UI.Action UI.State Unit Hydra
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


network :: Toolkit Unit Hydra -> Effect (Network Unit Hydra)
network toolkit = do
    pure $ Network.empty

{-
        # Network.addPatch ( "hydra" /\ Patch.empty )
        # Network.withPatch' "hydra" (\patch -> do
            patch
                # Patch.addNodesFrom toolkit
                    [ "num" /\ "freq"
                    , "num" /\ "sync"
                    , "num" /\ "offset"
                    , "osc" /\ "osc"
                    , "out" /\ "out"
                    ]
                >>= ("freq" /\ "num")   <~> ("osc" /\ "freq")
                >>= ("sync" /\ "num")   <~> ("osc" /\ "sync")
                >>= ("offset" /\ "num") <~> ("osc" /\ "offset")
                --- >>= ("osc" /\ "osc")    <~> ("out" /\ "src")
-}

                {-
                >>= ("freq" /\ "num")   +>  Hydra.num 4.0 -- doesn't update the node :(
                >>= ("sync" /\ "num")   +>  Hydra.num 0.1 -- doesn't update the node :(
                >>= ("offset" /\ "num") +>  Hydra.num 1.2 -- doesn't update the node :(
                -}
{-
                >>= ("freq" /\ "num")   +>  Hydra.num 60.0 -- doesn't update the node :(
                >>= ("sync" /\ "num")   +>  Hydra.num 0.1 -- doesn't update the node :(
                >>= ("offset" /\ "num") +>  Hydra.num 0.0 -- doesn't update the node :(
-}
