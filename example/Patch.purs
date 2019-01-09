module Example.Patch
    ( testPatch
    , testNode
    ) where

import Data.List as List

import Rpd.Def as Rpd
import Rpd.Process (ProcessF(..)) as R


testPatch :: forall d. Rpd.PatchDef d
testPatch =
    { name : "foo"
    , nodeDefs : List.Nil
    }


testNode :: forall d. Rpd.NodeDef d
testNode =
    { name : "a"
    , inletDefs : List.Nil
    , outletDefs : List.Nil
    , process : R.Withhold
    }
