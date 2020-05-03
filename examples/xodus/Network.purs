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
        </> Actions.addPatch "xodus-dnq"
        </> Actions.addNode (R.toPatch "xodus-dnq") "list" NodeListNode
        </> Actions.addNode (R.toPatch "xodus-dnq") "connect" ConnectNode
        </> Actions.addNode (R.toPatch "xodus-dnq") "databases" DatabasesNode

