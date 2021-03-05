module TensorFlow.Network
    ( recipe
    ) where

import Noodle.Path as R

import Noodle.API.Action.Sequence as Actions
import Noodle.API.Action.Sequence ((</>))

import TensorFlow.Toolkit.Node (Node(..))
import TensorFlow.Toolkit.Value (Value)
import TensorFlow.Toolkit.Channel (Channel)


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "tensforflow"
        </> Actions.addNode (R.toPatch "tensforflow") "bang" BangNode
        </> Actions.addNode (R.toPatch "tensforflow") "list" NodeListNode
        </> Actions.addNode (R.toPatch "tensforflow") "add" AddNode
        </> Actions.addNode (R.toPatch "tensforflow") "layer-1" LayerNode
        </> Actions.addNode (R.toPatch "tensforflow") "layer-2" LayerNode
