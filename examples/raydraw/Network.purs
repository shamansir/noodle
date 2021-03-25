module RayDraw.Network
    ( recipe
    ) where

import Noodle.Path as R

import Noodle.API.Action.Sequence as Actions
import Noodle.API.Action.Sequence ((</>))

import RayDraw.Toolkit.Node (Node(..))
import RayDraw.Toolkit.Value (Value)
import RayDraw.Toolkit.Channel (Channel)


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "raydraw-dnq"
        </> Actions.addNode (R.toPatch "raydraw-dnq") "list" NodeListNode


