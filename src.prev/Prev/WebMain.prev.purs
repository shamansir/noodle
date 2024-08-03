module Prev.WebMain where


import Prelude (Unit, (<#>), (>>=), ($), pure, unit)

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Prev.Web.App (run) as App
import Prev.Web.App (App')
import Prev.Web.App.Style (Style, NodeFlow(..))
import Prev.Web.App.Style.Hydra as Hydra

import Color.Extra (transparent) as C

import Prev.Noodle.Network (Network)
import Prev.Noodle.Network as Network
import Prev.Noodle.Toolkit (Toolkit)

-- import Hydra (Hydra)
-- import Toolkit.Hydra (toolkit)
-- import Hydra.Toolkit.UI (components, markings, getFlags)
-- import Hydra.Toolkit.UI.State (State, init) as UI
-- import Hydra.Toolkit.UI.Action (Action) as UI
-- import Hydra.Network (network)


import Prev.Toolkit.Hydra.Op (Hydra)
import Prev.Toolkit.Hydra (toolkit)
import Prev.Toolkit.Hydra.Queue (Queue)
import Prev.Toolkit.Hydra.UI (components, markings, getFlags)
import Prev.Toolkit.Hydra.UI.State (State, init) as UI
import Prev.Toolkit.Hydra.UI.Action (Action) as UI
--import Hydra.Network (network)


style :: Style
style =
    Hydra.style
        { bg
            { fill = C.transparent }
        }


flow :: NodeFlow
flow = Vertical


--app :: Network Hydra -> App' UI.Action UI.State Unit Hydra
app :: Network Hydra -> App' UI.State Hydra Hydra
app nw =
    { toolkit
    -- , components
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


network :: Toolkit Queue Hydra -> Effect (Network Hydra)
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
