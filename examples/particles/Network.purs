module Example.Network
    ( recipe
    ) where

import Prelude

import Noodle.Network (empty) as Network
import Noodle.Network as R
import Noodle.Path as R
import Noodle.Toolkit as R

-- import Noodle.API as Noodle
import Noodle.API.Action.Apply as Noodle
import Noodle.API.Action.Sequence as Actions
import Noodle.API.Action.Sequence ((</>))
-- import Noodle.API ((</>))

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
        </> Actions.addNode (R.toPatch "test") "list" NodeListNode

        -- FIXME: this block produces an error
        -- </> Noodle.addNode (R.toPatch "test") "random"
        -- </> Noodle.addInlet (R.toNode "test" "random") "min"
        -- </> Noodle.addInlet (R.toNode "test" "random") "max"
        -- </> Noodle.addInlet (R.toNode "test" "random") "bang"
        -- </> Noodle.addOutlet (R.toNode "test" "random") "random"

        -- </> Noodle.addToolkitNode (R.toPatch "test") "random" toolkit RandomNode

        -- </> Noodle.sendToInlet (R.toInlet "test" "random" "min") (Number' 10.0)
        -- </> Noodle.sendToInlet (R.toInlet "test" "random" "max") (Number' 20.0)
        -- </> Noodle.sendToInlet (R.toInlet "test" "random" "bang") Bang
