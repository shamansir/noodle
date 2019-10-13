module Example.Network
    ( recipe
    ) where

import Prelude

import Rpd.Network (empty) as Network
import Rpd.Network as R
import Rpd.Path as R
import Rpd.Toolkit as R

-- import Rpd.API as Rpd
import Rpd.API.Action.Apply as Rpd
import Rpd.API.Action.Sequence as Actions
import Rpd.API.Action.Sequence ((</>))
-- import Rpd.API ((</>))

import Example.Toolkit.Nodes (Node(..))
import Example.Toolkit.Value (Value(..))
import Example.Toolkit.Channel (Channel(..))
import Example.Toolkit (toolkit)


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "test"
        </> Actions.addNode (R.toPatch "test") "random" RandomNode
        </> Actions.addInlet (R.toNode "test" "random") "min" NumericalChannel
        </> Actions.addNode (R.toPatch "test") "buttons" ButtonsNode

        -- FIXME: this block produces an error
        -- </> Rpd.addNode (R.toPatch "test") "random"
        -- </> Rpd.addInlet (R.toNode "test" "random") "min"
        -- </> Rpd.addInlet (R.toNode "test" "random") "max"
        -- </> Rpd.addInlet (R.toNode "test" "random") "bang"
        -- </> Rpd.addOutlet (R.toNode "test" "random") "random"

        -- </> Rpd.addToolkitNode (R.toPatch "test") "random" toolkit RandomNode

        -- </> Rpd.sendToInlet (R.toInlet "test" "random" "min") (Number' 10.0)
        -- </> Rpd.sendToInlet (R.toInlet "test" "random" "max") (Number' 20.0)
        -- </> Rpd.sendToInlet (R.toInlet "test" "random" "bang") Bang
