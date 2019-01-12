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

import Example.Toolkit (patch, colorNode, Value)

network :: R.Rpd (R.Network Value)
network =
    Rpd.init "foo"
        </> Rpd.addPatch' patch
        </> Rpd.addNode' (R.patchId 0) colorNode
