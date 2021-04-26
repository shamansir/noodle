module RayDraw.Network
    ( recipe
    ) where

import Prelude
import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence as Actions
import Noodle.Path as R
import RayDraw.Toolkit.Channel (Channel)
import RayDraw.Toolkit.Node (Node(..))
import RayDraw.Toolkit.Value (Product(..), Value(..), getPalette, rayPoints)


recipe :: Actions.ActionList Value Channel Node
recipe =
    Actions.init
        </> Actions.addPatch "raydraw-dnq"
        </> Actions.addNode (R.toPatch "raydraw-dnq") "list" NodeListNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "bang" BangNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "palette" ProductPaletteNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "preview" PreviewNode
        </> Actions.addNode (R.toPatch "raydraw-dnq") "points" RayPointsNode
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "bang" "bang")
                    (R.toInlet "raydraw-dnq" "preview" "bang")
        </> Actions.connect 
                    (R.toOutlet "raydraw-dnq" "palette" "palette")
                    (R.toInlet "raydraw-dnq" "preview" "palette")
        </> Actions.sendToInlet (R.toInlet "raydraw-dnq" "bang" "bang") Bang
        </> Actions.sendToOutlet (R.toOutlet "raydraw-dnq" "palette" "palette") (Palette $ getPalette JetBrains)
        </> Actions.sendToInlet (R.toInlet "raydraw-dnq" "points" "points") (Points $ rayPoints [{x : 1.0, y : 3.0}, {x: 3.0, y : 10.0}, {x : 5.0, y: 10.0}])

