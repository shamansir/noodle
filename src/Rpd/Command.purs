module Rpd.Command where

import Prelude

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
    -- | SendToInlet P.InletPath d
    -- | SendToOutlet P.OutletPath d
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
    show (AddPatch { name }) = "AddPatch " <> name
    show (AddNode path { name }) = "AddNode " <> show path <> " " <> name
    show (AddInlet path { label }) = "AddInlet " <> show path <> " " <> label
    show (AddOutlet path { label }) = "AddOutlet " <> show path <> " " <> label
    show (Connect oPath iPath) = "Connect " <> show oPath <> " " <> show iPath
    show (Disconnect oPath iPath) = "Disconnect " <> show oPath <> " " <> show iPath
    -- show (GotInletData iPath d) = "GotInletData " <> show iPath <> " " <> show d
    -- show (GotOutletData oPath d) = "GotOutletData " <> show oPath <> " " <> show d


instance eqCommand :: Eq d => Eq (Command d) where
    eq Bang Bang = true
    eq (AddPatch lDef) (AddPatch rDef) = D.comparePDefs lDef rDef
    eq (AddNode lId lDef) (AddNode rId rDef) =
      (lId == rId) && D.compareNDefs lDef rDef
    eq (AddInlet lPath lDef) (AddInlet rPath rDef) =
      (lPath == rPath) && D.compareIDefs lDef rDef
    eq (AddOutlet lPath lDef) (AddOutlet rPath rDef) =
      (lPath == rPath) && D.compareODefs lDef rDef
    eq (Connect lOPath lIPath) (Connect rOPath rIpath) =
      (lOPath == rOPath) && (lIPath == rIpath)
    eq (Disconnect lOPath lIPath) (Disconnect rOPath rIpath) =
      (lOPath == rOPath) && (lIPath == rIpath)
    eq _ _ = false
