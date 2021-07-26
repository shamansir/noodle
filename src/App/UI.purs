module App.UI where


import Data.Maybe (Maybe)

import Noodle.Node (Node)
import Noodle.Network (Network)


import Halogen as H


type BgInput d = Network d


type NodeInput d = Node d


type UI d =
    { background :: forall m query output. Maybe (H.Component query (BgInput d) output m)
    , node :: forall m query output. String -> Maybe (H.Component query (NodeInput d) output m)
    }