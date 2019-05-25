module Example.Network
    ( network
    ) where

import Prelude

import Rpd.Network (empty) as Network
import Rpd.Network as R
import Rpd.Path as R

import Rpd.API (Rpd) as R
import Rpd.API as Rpd
import Rpd.API ((</>))

import Example.Toolkit (patch, colorNode, Value(..))

network :: R.Rpd (R.Network Value)
network =
    Rpd.init "foo"
        </> Rpd.addPatch' patch
        </> Rpd.addNode' (R.patchPath 0) colorNode
        </> Rpd.sendToInlet (R.inletPath 0 0 0) (Number' 10.0)
        </> Rpd.sendToInlet (R.inletPath 0 0 1) (Number' 20.0)
        </> Rpd.sendToInlet (R.inletPath 0 0 2) (Number' 30.0)
