module Rpd.Command where

import Prelude (class Eq, class Show, show, (<>))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow

import Rpd.Path as P
import Rpd.Def as D

data Command d
    = Bang
    | AddPatch (D.PatchDef d)
    | AddNode P.PatchId (D.NodeDef d)
    | AddInlet P.NodePath (D.InletDef d)
    | AddOutlet P.NodePath (D.OutletDef d)
    | Connect P.OutletPath P.InletPath
    | Disconnect P.OutletPath P.InletPath
    -- | DeleteNode
    -- | DeleteInlet
    -- | DeleteOutlet

data StringCommand
    = Bang'
    | AddPatch' String
    | AddNode' P.PatchId String
    | AddInlet' P.NodePath String
    | AddOutlet' P.NodePath String
    | Connect' P.OutletPath P.InletPath
    | Disconnect' P.OutletPath P.InletPath


derive instance genericStringCommand :: Generic StringCommand _
instance eqStringCommand :: Eq StringCommand where
  eq = GEq.genericEq
instance showStringCommand :: Show StringCommand where
  show = GShow.genericShow

-- derive instance genericCommand :: Generic (Command d) _
-- instance eqCommand :: Eq d => Eq (Command d) where
--   eq = GEq.genericEq
-- instance showCommand :: Show d => Show (Command d) where
--   show = GShow.genericShow
