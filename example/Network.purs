module Example.Network
    ( network
    ) where

import Prelude

import Rpd.Network (empty) as Network
import Rpd.Network as R
import Rpd.Path as R
import Rpd.Toolkit as R

import Rpd.API (Rpd) as R
import Rpd.API as Rpd
import Rpd.API ((</>))

import Example.Toolkit.Value (Value(..))
import Example.Toolkit (toolkit)

network :: R.Rpd (R.Network Value)
network =
    Rpd.init "foo"
        </> Rpd.addPatch "test"

        -- FIXME: this block produces an error
        -- </> Rpd.addNode (R.toPatch "test") "random"
        -- </> Rpd.addInlet (R.toNode "test" "random") "min"
        -- </> Rpd.addInlet (R.toNode "test" "random") "max"
        -- </> Rpd.addInlet (R.toNode "test" "random") "bang"
        -- </> Rpd.addOutlet (R.toNode "test" "random") "random"

        </> Rpd.addToolkitNode (R.toPatch "test") "random" toolkit (R.NodeDefAlias "random")

        </> Rpd.sendToInlet (R.toInlet "test" "random" "min") (Number' 10.0)
        </> Rpd.sendToInlet (R.toInlet "test" "random" "max") (Number' 20.0)
        </> Rpd.sendToInlet (R.toInlet "test" "random" "bang") Bang
