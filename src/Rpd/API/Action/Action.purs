module Rpd.API.Action where

-- import Data.Generic.Rep (class Generic)
-- import Data.Generic.Rep.Eq as GEq
-- import Data.Generic.Rep.Show as GShow

import Prelude (class Show, show, (<>), Unit)

import FRP.Event (Event)

import Effect (Effect)

import Rpd.Path as Path
import Rpd.Network
import Rpd.Process (ProcessF)
import Rpd.Util (Canceler)
import Rpd.Toolkit (NodeDef)


data Action d c n
    = NoOp
    | Inner (InnerAction d c n)
    | Request (RequestAction d c n)
    | Build (BuildAction d c n)
    | Data (DataAction d c)


data RequestAction d c n
    = ToAddPatch Path.Alias
    | ToAddNode Path.ToPatch Path.Alias n
    | ToAddNodeByDef Path.ToPatch Path.Alias n (NodeDef d c)
    | ToAddOutlet Path.ToNode Path.Alias c
    | ToAddInlet Path.ToNode Path.Alias c
    | ToConnect Path.ToOutlet Path.ToInlet
    | ToDisconnect Path.ToOutlet Path.ToInlet
    | ToSendToInlet Path.ToInlet d
    | ToSendToOutlet Path.ToOutlet d
    | ToSendPeriodicallyToInlet Path.ToInlet Int (Int -> d)
    | ToStreamToInlet Path.ToInlet (Event d)
    | ToStreamToOutlet Path.ToOutlet (Event d)
    | ToSubscribeToInlet Path.ToInlet (d -> Effect Unit)
    | ToSubscribeToOutlet Path.ToOutlet (d -> Effect Unit)
    -- | ToSendPeriodicallyToInlet Path.ToInlet Int (Int -> d)


data BuildAction d c n
    = AddPatch (Patch d c n)
    | AddNode (Node d n)
    -- TODO: Toolkit nodes
    | AddInlet (Inlet d c)
    | AddOutlet (Outlet d c)
    | AddLink Link
    | ProcessWith (Node d n) (ProcessF d)


data InnerAction d c n -- FIXME: InnerActions should not be exposed
    = Do (Network d c n -> Effect Unit)
    | StoreNodeCanceler (Node d n) Canceler
    | ClearNodeCancelers (Node d n)
    | StoreInletCanceler (Inlet d c) Canceler
    | StoreOutletCanceler (Outlet d c) Canceler
    | StoreLinkCanceler Link Canceler


data DataAction d c
    = Bang
    | SendToInlet (Inlet d c) d -- FIXME: either implement or get rid of
    | SendToOutlet (Outlet d c) d -- FIXME: either implement or get rid of
    -- | SendPeriodicallyToInlet (Inlet d c) Int (Int -> d)
    | GotInletData (Inlet d c) d
    | GotOutletData (Outlet d c) d -- TODO: implement and use


data RpdEffect d c n -- TODO: move to a separate module
    = DoE (Network d c n -> Effect Unit)
    | AddPatchE Path.Alias
    | AddNodeE Path.ToPatch Path.Alias n (NodeDef d c)
    | AddInletE Path.ToNode Path.Alias c
    | AddOutletE Path.ToNode Path.Alias c
    | AddLinkE (Outlet d c) (Inlet d c)
    | SubscribeNodeProcess (Node d n)
    | InformNodeOnInletUpdates (Inlet d c) (Node d n)
    | InformNodeOnOutletUpdates (Outlet d c) (Node d n)
    | CancelNodeSubscriptions (Node d n)
    | SubscribeNodeUpdates (Node d n)
    | SendToInletE (PushToInlet d) d
    | SendToOutletE (PushToOutlet d) d
    | SendActionOnInletUpdatesE (Inlet d c)
    | SendActionOnOutletUpdatesE (Outlet d c)
    | SendPeriodicallyToInletE (PushToInlet d) Int (Int -> d)
    | SendPeriodicallyToOutletE (PushToOutlet d) Int (Int -> d)
    | StreamToInletE (PushToInlet d) (Event d)
    | StreamToOutletE (PushToOutlet d) (Event d)
    | SubscribeToInletE (InletFlow d) (d -> Effect Unit) -- FIXME: should pass the canceler to the user
    | SubscribeToOutletE (OutletFlow d) (d -> Effect Unit) -- FIXME: should pass the canceler to the user


-- derive instance genericStringAction :: Generic StringAction _
-- instance eqStringAction :: Eq StringAction where
--   eq = GEq.genericEq
-- instance showStringAction :: Show StringAction where
--   show = GShow.genericShow

instance showAction :: (Show d, Show c, Show n) => Show (Action d c n) where
    show NoOp = "NoOp"
    show (Inner innerAction) = "I: " <> show innerAction
    show (Request requestAction) = "R: " <> show requestAction
    show (Build buildAction) = "B: " <> show buildAction
    show (Data dataAction) = "D: " <> show dataAction


instance showDataAction :: (Show d, Show c) => Show (DataAction d c) where
    show Bang = "Bang"
    show (GotInletData inlet d) = "GotInletData " <> show inlet <> " " <> show d
    show (GotOutletData outlet d) = "GotOutletData " <> show outlet <> " " <> show d
    show (SendToInlet inlet d) = "SendToInlet " <> show inlet <> " " <> show d
    show (SendToOutlet outlet d) = "SendToOutlet " <> show outlet <> " " <> show d


instance showInnerAction :: Show (InnerAction d c n) where
    show _ = "<Inner>"


instance showBuildAction :: (Show d, Show c, Show n) => Show (BuildAction d c n) where
    show (AddPatch patch) = "AddPatch " <> show patch
    show (AddNode node) = "AddNode " <> show node
    show (AddOutlet outlet) = "AddOutlet " <> show outlet
    show (AddInlet inlet) = "AddInlet " <> show inlet
    show (AddLink link) = "AddLink "
    show (ProcessWith node _) = "ProcessWith " <> show node


instance showRequestAction :: Show (RequestAction d c n) where
    show _ = "<Request>"


-- instance showAction :: Show d => Show (Action d) where
-- instance showAction :: (Show d, Show c, Show n) => Show (Action d c n) where
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


-- instance eqAction :: (Eq c, Eq n) => Eq (Action d c n) where
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
