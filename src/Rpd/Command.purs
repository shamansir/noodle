module Rpd.Command where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow

import Rpd.Toolkit (class Channels, NodeDefAlias, ChannelDefAlias)
import Rpd.Path as Path
import Rpd.UUID


data Command d c n
    = Bang
    | AddPatch Path.Alias
    | AddNode Path.ToPatch Path.Alias n
    | AddInlet Path.ToNode Path.Alias c
    | AddOutlet Path.ToNode Path.Alias c
    -- | AddInlet Path (forall c. Show c => Channels d c => c)
    -- | AddOutlet Path (forall c. Show c => Channels d c => c)
    | Connect { outlet :: Path.ToOutlet, inlet :: Path.ToInlet }
    | Disconnect { outlet :: Path.ToOutlet, inlet :: Path.ToInlet }
    | GotInletData Path.ToInlet d
    | GotOutletData Path.ToOutlet d
    | SendToInlet Path.ToInlet d
    | SendToOutlet Path.ToOutlet d
    -- | DeleteNode
    -- | DeleteInlet
    -- | DeleteOutlet


-- derive instance genericStringCommand :: Generic StringCommand _
-- instance eqStringCommand :: Eq StringCommand where
--   eq = GEq.genericEq
-- instance showStringCommand :: Show StringCommand where
--   show = GShow.genericShow


-- instance showCommand :: Show d => Show (Command d) where
instance showCommand :: (Show d, Show c, Show n) => Show (Command d c n) where
    show Bang = "Bang"
    show (AddPatch alias) = "AddPatch " <> show (Path.toPatch alias)
    show (AddNode patchPath alias n) =
        "AddNode " <> show (Path.nodeInPatch patchPath alias) <> " " <> show n
    show (AddInlet nodePath alias c) =
        "AddInlet " <> show (Path.inletInNode nodePath alias) <> " " <> show c
    show (AddOutlet nodePath alias c)  =
        "AddOutlet " <> show (Path.outletInNode nodePath alias) <> " " <> show c
    show (Connect { outlet : oPath, inlet : iPath }) =
        "Connect " <> show oPath <> " " <> show iPath
    show (Disconnect { outlet : oPath, inlet : iPath }) =
        "Disconnect " <> show oPath <> " " <> show iPath
    show (GotInletData iPath d) = "GotInletData " <> show iPath <> " " <> show d
    show (GotOutletData oPath d) = "GotutletData " <> show oPath <> " " <> show d
    show (SendToInlet iPath d) = "SendToInlet " <> show iPath <> " " <> show d
    show (SendToOutlet oPath d) = "SendToOutlet " <> show oPath <> " " <> show d
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
