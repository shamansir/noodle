module RayDraw.Network
    ( recipe
    ) where

import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence as Actions
import Noodle.Path as R
import RayDraw.Toolkit.Channel (Channel)
import RayDraw.Toolkit.Node (Node(..))
import RayDraw.Toolkit.Value (Value(..))


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "raydraw-dnq"
        </> Actions.addNode (R.toPatch "raydraw-dnq") "list" NodeListNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "bang" BangNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "palette1" PaletteNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "palette2" PaletteNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "palette3" PaletteNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "preview" PreviewNode
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "bang" "bang")
                    (R.toInlet "raydraw-dnq" "preview" "bang")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "palette1" "color")
                    (R.toInlet "raydraw-dnq" "preview" "color1")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "palette2" "color")
                    (R.toInlet "raydraw-dnq" "preview" "color2")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "palette3" "color")
                    (R.toInlet "raydraw-dnq" "preview" "color3")
        </> Actions.sendToInlet (R.toInlet "raydraw-dnq" "bang" "bang") Bang

