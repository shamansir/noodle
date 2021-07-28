module Hydra.Network where


import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\))
import Data.Traversable (traverse, traverse_)

import Noodle.Network (Network)
import Noodle.Network as Network
import Noodle.Patch as Patch
import Noodle.Patch ((<~>), (+>))
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit


import Hydra (Hydra)
import Hydra as Hydra


network :: Toolkit Hydra -> Effect (Network Hydra)
network toolkit = do
    Network.empty
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
                >>= ("osc" /\ "osc")    <~> ("out" /\ "src")
                >>= ("freq" /\ "num")   +>  Hydra.num 4.0
                >>= ("sync" /\ "num")   +>  Hydra.num 0.1
                >>= ("offset" /\ "num") +>  Hydra.num 1.2
        )
