module Rpd.Command where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow

import Rpd.Channel (class Channel)
import Rpd.Path
import Rpd.UUID



data Command d c
    = Bang
    | AddPatch Path
    | AddNode Path
    | AddInlet Path (Channel c d => c)
    | AddOutlet Path (Channel c d => c)
    -- | AddInlet Path (forall c. Show c => Channel c d => c)
    -- | AddOutlet Path (forall c. Show c => Channel c d => c)
    | Connect { outlet :: Path, inlet :: Path }
    | Disconnect { outlet :: Path, inlet :: Path }
    | GotInletData Path d
    | GotOutletData Path d
    | SendToInlet Path d
    | SendToOutlet Path d
    -- | DeleteNode
    -- | DeleteInlet
    -- | DeleteOutlet


-- derive instance genericStringCommand :: Generic StringCommand _
-- instance eqStringCommand :: Eq StringCommand where
--   eq = GEq.genericEq
-- instance showStringCommand :: Show StringCommand where
--   show = GShow.genericShow


-- instance showCommand :: Show d => Show (Command d) where
instance showCommand :: Channel c d => Show (Command d c) where
    show Bang = "Bang"
    show (AddPatch path) = "AddPatch " <> show path
    show (AddNode path) = "AddNode " <> show path
    show (AddInlet path channel) = "AddInlet " <> show path
    show (AddOutlet path channel) = "AddOutlet " <> show path <> " " <> show channel
    show (Connect { outlet : oPath, inlet : iPath }) =
        "Connect " <> show oPath <> " " <> show iPath
    show (Disconnect { outlet : oPath, inlet : iPath }) =
        "Disconnect " <> show oPath <> " " <> show iPath
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
