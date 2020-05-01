module Xodus.Network
    ( recipe
    ) where

import Noodle.Path as R

import Noodle.API.Action.Sequence as Actions
import Noodle.API.Action.Sequence ((</>))

import Xodus.Toolkit.Node (Node(..))
import Xodus.Toolkit.Value (Value)
import Xodus.Toolkit.Channel (Channel(..))


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "test"
        </> Actions.addNode (R.toPatch "test") "bang" BangNode
        </> Actions.addInlet (R.toNode "test" "bang") "ch" Channel
        </> Actions.addNode (R.toPatch "test") "list" NodeListNode
