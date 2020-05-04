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
        </> Actions.addNode (R.toPatch "xodus-dnq") "source" SourceNode
        </> Actions.addNode (R.toPatch "xodus-dnq") "all-of" AllOfNode
        </> Actions.addNode (R.toPatch "xodus-dnq") "select" SelectNode
        </> Actions.connect
                (R.toOutlet "xodus-dnq" "connect" "databases")
                (R.toInlet "xodus-dnq" "source" "databases")
        </> Actions.connect
                (R.toOutlet "xodus-dnq" "source" "source")
                (R.toInlet "xodus-dnq" "all-of" "source")
        </> Actions.connect
                (R.toOutlet "xodus-dnq" "all-of" "query")
                (R.toInlet "xodus-dnq" "select" "query")

