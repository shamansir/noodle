module Rpd.API.Command where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow

import Rpd.Toolkit (class Channels, NodeDefAlias, ChannelDefAlias)
import Rpd.Path as Path
import Rpd.Network
import Rpd.Process
import Rpd.Util (Canceler)
import Rpd.UUID



data RequestCommand d c n
    = ToAddPatch Path.Alias
    | ToAddNode Path.ToPatch Path.Alias n
    | ToAddOutlet Path.ToNode Path.Alias c
    | ToAddInlet Path.ToNode Path.Alias c
    | ToConnect Path.ToOutlet Path.ToInlet


data BuildCommand d c n
    = AddPatch (Patch d c n)
    | AddNode (Node d n)
    -- TODO: Toolkit nodes
    | AddInlet (Inlet d c)
    | AddLink Link
    | AddOutlet (Outlet d c)
    | ProcessWith (Node d n) (ProcessF d)


data InnerCommand d c n
    = StoreNodeCanceler (Node d n) Canceler
    | ClearNodeCancelers (Node d n)
    | StoreInletCanceler (Inlet d c) Canceler
    | StoreOutletCanceler (Outlet d c) Canceler
    | StoreLinkCanceler Link Canceler


data DataCommand d c
    = Bang
    | GotInletData (Inlet d c) d -- TODO: implement and use
    | GotOutletData (Outlet d c) d -- TODO: implement and use
    | SendToInlet Path.ToInlet d
    | SendToOutlet Path.ToOutlet d


data Command d c n
    = NoOp
    | Inner (InnerCommand d c n)
    | Request (RequestCommand d c n)
    | Build (BuildCommand d c n)
    | Data (DataCommand d c)


data RpdEffect d c n
    = AddPatchE Path.Alias
    | AddNodeE Path.ToPatch Path.Alias n
    | AddInletE Path.ToNode Path.Alias c
    | AddOutletE Path.ToNode Path.Alias c
    | AddLinkE (Outlet d c) (Inlet d c)
    | SubscribeNodeProcess (Node d n)
    | InformNodeOnInletUpdates (Inlet d c) (Node d n)
    | InformNodeOnOutletUpdates (Outlet d c) (Node d n)
    | CancelNodeSubscriptions (Node d n)
    | SubscribeNodeUpdates (Node d n)


-- derive instance genericStringCommand :: Generic StringCommand _
-- instance eqStringCommand :: Eq StringCommand where
--   eq = GEq.genericEq
-- instance showStringCommand :: Show StringCommand where
--   show = GShow.genericShow


-- instance showCommand :: Show d => Show (Command d) where
-- instance showCommand :: (Show d, Show c, Show n) => Show (Command d c n) where
--     show Bang = "Bang"
--     show (AddPatch alias) = "AddPatch " <> show (Path.toPatch alias)
--     show (AddNode patchPath alias n) =
--         "AddNode " <> show (Path.nodeInPatch patchPath alias) <> " " <> show n
--     show (AddInlet nodePath alias c) =
--         "AddInlet " <> show (Path.inletInNode nodePath alias) <> " " <> show c
--     show (AddOutlet nodePath alias c)  =
--         "AddOutlet " <> show (Path.outletInNode nodePath alias) <> " " <> show c
--     show (Connect { outlet : oPath, inlet : iPath }) =
--         "Connect " <> show oPath <> " " <> show iPath
--     show (Disconnect { outlet : oPath, inlet : iPath }) =
--         "Disconnect " <> show oPath <> " " <> show iPath
--     show (GotInletData iPath d) = "GotInletData " <> show iPath <> " " <> show d
--     show (GotOutletData oPath d) = "GotutletData " <> show oPath <> " " <> show d
--     show (SendToInlet iPath d) = "SendToInlet " <> show iPath <> " " <> show d
--     show (SendToOutlet oPath d) = "SendToOutlet " <> show oPath <> " " <> show d
    -- show (GotInletData iPath d) = "GotInletData " <> show iPath <> " " <> show d
    -- show (GotOutletData oPath d) = "GotOutletData " <> show oPath <> " " <> show d


-- instance eqCommand :: (Eq c, Eq n) => Eq (Command d c n) where
--     eq Bang Bang = true
--     eq (AddPatch lAlias) (AddPatch rAlias) = lAlias == rAlias
--     eq (AddNode lPatch lNode lNodeType) (AddNode rPath rNode rNodeType) =
--       (lPatch == rPath) && (lNode == lNode) && (lNodeType == rNodeType)
--     eq (AddInlet lNode lInlet lChannel) (AddInlet rNode rInlet rChannel) =
--       (lNode == rNode) && (lInlet == rInlet) && (lChannel == rChannel)
--     eq (AddOutlet lNode lOutlet lChannel) (AddOutlet rNode rOutlet rChannel) =
--       (lNode == rNode) && (lOutlet == rOutlet) && (lChannel == rChannel)
--     eq (Connect { outlet : lOutlet, inlet : lInlet })
--        (Connect { outlet : rOutlet, inlet : rInlet }) =
--       (lOutlet == rOutlet) && (lInlet == rInlet)
--     eq (Disconnect { outlet : lOutlet, inlet : lInlet })
--        (Disconnect { outlet : rOutlet, inlet : rInlet }) =
--       (lOutlet == rOutlet) && (lInlet == rInlet)
--     eq _ _ = false
