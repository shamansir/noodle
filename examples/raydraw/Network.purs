module RayDraw.Network
    ( recipe
    ) where

import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence as Actions
import Noodle.Path as R
import RayDraw.Toolkit.Channel (Channel)
import RayDraw.Toolkit.Node (Node(..))
import RayDraw.Toolkit.Value (RgbaColor(..), Value(..))


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "raydraw-dnq"
        </> Actions.addNode (R.toPatch "raydraw-dnq") "list" NodeListNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "bang" BangNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "color1" ColorNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "color2" ColorNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "color3" ColorNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "preview" PreviewNode
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "bang" "bang")
                    (R.toInlet "raydraw-dnq" "preview" "bang")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "color1" "color")
                    (R.toInlet "raydraw-dnq" "preview" "color1")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "color2" "color")
                    (R.toInlet "raydraw-dnq" "preview" "color2")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "color3" "color")
                    (R.toInlet "raydraw-dnq" "preview" "color3")
        </> Actions.sendToInlet (R.toInlet "raydraw-dnq" "bang" "bang") Bang
        </> Actions.sendToOutlet (R.toOutlet "raydraw-dnq" "color1" "color") (Color (RgbaColor {r: 1.0, g : 0.5, b : 0.0, a: 1.0}))
        </> Actions.sendToOutlet (R.toOutlet "raydraw-dnq" "color2" "color") (Color (RgbaColor {r: 0.8, g : 1.0, b : 0.2, a: 1.0}))
        </> Actions.sendToOutlet (R.toOutlet "raydraw-dnq" "color3" "color") (Color (RgbaColor {r: 0.25, g : 0.5, b : 1.0, a: 1.0}))

