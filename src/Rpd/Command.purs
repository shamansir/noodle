module Rpd.Command where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow

import Rpd.Path as P
import Rpd.UUID as UUID
import Rpd.Def as D


data Command d
    = Bang
    | AddPatch P.Alias (D.PatchDef d)
    | AddNode P.PatchId P.Alias (D.NodeDef d)
    | AddInlet P.NodePath P.Alias (D.InletDef d)
    | AddOutlet P.NodePath P.Alias (D.OutletDef d)
    | Connect P.OutletPath P.InletPath
    | Disconnect P.OutletPath P.InletPath
    | GotInletData P.InletPath d
    | GotOutletData P.OutletPath d
    | SendToInlet P.InletPath d
    | SendToOutlet P.OutletPath d
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


-- instance showCommand :: Show d => Show (Command d) where
instance showCommand :: Show (Command d) where
    show Bang = "Bang"
    show (AddPatch alias _) = "AddPatch " <> show alias
    show (AddNode path alias _) = "AddNode " <> show path <> " " <> show alias
    show (AddInlet path alias _) = "AddInlet " <> show path <> " " <> show alias
    show (AddOutlet path alias _) = "AddOutlet " <> show path <> " " <> show alias
    show (Connect oPath iPath) = "Connect " <> show oPath <> " " <> show iPath
    show (Disconnect oPath iPath) = "Disconnect " <> show oPath <> " " <> show iPath
    show (GotInletData iPath _) = "GotInletData " <> show iPath <> " TODO"
    show (GotOutletData oPath _) = "GotutletData " <> show oPath <> " TODO"
    show (SendToInlet iPath _) = "SendToInlet " <> show iPath <> " TODO"
    show (SendToOutlet oPath _) = "SendToOutlet " <> show oPath <> " TODO"
    -- show (GotInletData iPath d) = "GotInletData " <> show iPath <> " " <> show d
    -- show (GotOutletData oPath d) = "GotOutletData " <> show oPath <> " " <> show d


-- instance eqCommand :: Eq d => Eq (Command d) where
--     eq Bang Bang = true
--     eq (AddPatch lDef) (AddPatch rDef) = D.comparePDefs lDef rDef
--     eq (AddNode lId lDef) (AddNode rId rDef) =
--       (lId == rId) && D.compareNDefs lDef rDef
--     eq (AddInlet lPath lDef) (AddInlet rPath rDef) =
--       (lPath == rPath) && D.compareIDefs lDef rDef
--     eq (AddOutlet lPath lDef) (AddOutlet rPath rDef) =
--       (lPath == rPath) && D.compareODefs lDef rDef
--     eq (Connect lOPath lIPath) (Connect rOPath rIpath) =
--       (lOPath == rOPath) && (lIPath == rIpath)
--     eq (Disconnect lOPath lIPath) (Disconnect rOPath rIpath) =
--       (lOPath == rOPath) && (lIPath == rIpath)
--     eq _ _ = false
