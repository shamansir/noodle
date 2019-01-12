module Example.Toolkit
    ( patch
    , colorNode
    , Value(..)
    , ParticleShape(..)
    ) where


import Data.List as List

import Rpd.Def as Rpd
import Rpd.Process (ProcessF(..)) as R


data ParticleShape
    = Circle
    | Cross
    | Square
    | Diamond


data Value
    = Bang
    | Color Number Number Number
    | Shape ParticleShape
    | Random Number
    | Number' Number
    | IsEnabled Boolean
    | Period Number
    | Magic Number Number


patch :: Rpd.PatchDef Value
patch =
    { name : "particles"
    , nodeDefs : List.Nil
    }


colorNode :: Rpd.NodeDef Value
colorNode =
    { name : "color"
    , inletDefs : List.Nil
    , outletDefs : List.Nil
    , process : R.Withhold
    }


-- TODO: metro, color, random, shape, magic, wind...
