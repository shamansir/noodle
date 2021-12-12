module Noodle.NoodleM
    ( NoodleF
    ) where


import Prelude (Unit)


import Data.Tuple.Nested (type (/\), (/\))


import Noodle.Patch as Patch
import Noodle.Patch (Patch)
import Noodle.Node as Node
import Noodle.Node (Node)
import Noodle.Node.Shape (InletId, OutletId)
import Noodle.Node.Define as Def
import Noodle.Node.Define (Def)

import Halogen (HalogenM)


data NoodleF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | AddPatch Patch.Id a
    | AddNode Patch.Id Node.Id a
    | AddNodeFromDef Patch.Id Node.Id d (Def d) a
    | RemovePatch Patch.Id a
    -- | WithPatch Patch.Id (Patch d -> m (a /\ Patch d))
    -- | WithNode Patch.Id Node.Id (Node d -> m (a /\ Node d))
    | AddInlet Patch.Id Node.Id InletId d a
    | AddOutlet Patch.Id Node.Id OutletId d a
    | Connect Patch.Id Patch.OutletPath Patch.InletPath (Unit -> m (a /\ Node.Link))
    | Disconnect Patch.Id Patch.OutletPath Patch.InletPath a
    | DisconnectLink Node.Link
    | Send Patch.Id Node.Id InletId d a
    | Produce Patch.Id Node.Id OutletId d a
    | GetAtInlet Patch.Id Node.Id InletId (Unit -> m (a /\ d))
    | GetAtOutlet Patch.Id Node.Id OutletId (Unit -> m (a /\ d))
