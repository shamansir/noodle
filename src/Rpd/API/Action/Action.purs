module Rpd.API.Action where

-- import Data.Generic.Rep (class Generic)
-- import Data.Generic.Rep.Eq as GEq
-- import Data.Generic.Rep.Show as GShow

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List)
import Data.List (toUnfoldable, List(..)) as List
import Data.String (joinWith) as Str

import FRP.Event (Event)

import Effect (Effect)


import Rpd.API
    ( InletSubscription
    , OutletSubscription
    , InletPeriodSubscription
    , OutletPeriodSubscription
    , NodeInletsSubscription
    , NodeOutletsSubscription
    )
import Rpd.Path as Path
import Rpd.Network
import Rpd.Process (ProcessF, InletAlias, OutletAlias)
import Rpd.Util (Canceler)
import Rpd.Toolkit (NodeDef)
import Rpd.UUID as UUID


type Perform d c n = (Network d c n -> Effect Unit)


data Action d c n
    = NoOp
    | Inner (InnerAction d c n)
    | Request (RequestAction d c n)
    | Build (BuildAction d c n)
    | Data (DataAction d c)


data RequestAction d c n
    = ToAddPatch Path.Alias
    | ToAddNode Path.ToPatch Path.Alias n
    | ToAddNextNode Path.ToPatch n
    | ToAddNodeByDef Path.ToPatch Path.Alias n (NodeDef d c)
    | ToAddNextNodeByDef Path.ToPatch n (NodeDef d c)
    | ToRemoveNode Path.ToNode
    | ToAddInlet Path.ToNode Path.Alias c
    | ToAddOutlet Path.ToNode Path.Alias c
    | ToRemoveInlet Path.ToInlet
    | ToRemoveOutlet Path.ToOutlet
    | ToProcessWith Path.ToNode (ProcessF d)
    | ToConnect Path.ToOutlet Path.ToInlet
    | ToDisconnect Path.ToOutlet Path.ToInlet
    | ToSendToInlet Path.ToInlet d
    | ToSendToOutlet Path.ToOutlet d
    | ToSendPeriodicallyToInlet Path.ToInlet Int (InletPeriodSubscription d)
    | ToStreamToInlet Path.ToInlet (Event d)
    | ToStreamToOutlet Path.ToOutlet (Event d)
    | ToSubscribeToInlet Path.ToInlet (InletSubscription d)
    | ToSubscribeToOutlet Path.ToOutlet (OutletSubscription d)
    | ToSubscribeToNode Path.ToNode (NodeInletsSubscription d) (NodeOutletsSubscription d)


data BuildAction d c n
    = AddPatch (Patch d c n)
    | AddNode (Node d n)
    | RemoveNode (Node d n)
    -- TODO: Toolkit nodes
    | AddInlet (Inlet d c)
    | AddOutlet (Outlet d c)
    | RemoveInlet (Inlet d c)
    | RemoveOutlet (Outlet d c)
    | AddLink Link
    | Connect (Outlet d c) (Inlet d c)
    | ProcessWith (Node d n) (ProcessF d)


data InnerAction d c n -- FIXME: InnerActions should not be exposed
    = Do (Perform d c n) -- FIXME: We don't need it nowadays
    | StoreNodeCanceler (Node d n) Canceler
    | ClearNodeCancelers (Node d n)
    | StoreInletCanceler (Inlet d c) Canceler
    | ClearInletCancelers (Inlet d c)
    | StoreOutletCanceler (Outlet d c) Canceler
    | ClearOutletCancelers (Outlet d c)
    | StoreLinkCanceler Link Canceler
    | ClearLinkCancelers Link


data DataAction d c
    = Bang
    | SendToInlet (Inlet d c) d
    | SendToOutlet (Outlet d c) d
    -- | SendPeriodicallyToInlet (Inlet d c) Int (Int -> d)
    | GotInletData (Inlet d c) d
    | GotOutletData (Outlet d c) d
    | StreamToInlet (Inlet d c) (Event d)
    | StreamToOutlet (Outlet d c) (Event d)
    | SendPeriodicallyToInlet (Inlet d c) Int (InletPeriodSubscription d)


{-
data RpdEffect d c n -- TODO: move to a separate module
    = DoE (Perform d c n)
    | AddPatchE Path.Alias
    | AddNodeE Path.ToPatch Path.Alias n (NodeDef d c)
    | AddNextNodeE Path.ToPatch n (NodeDef d c)
    | AddInletE Path.ToNode Path.Alias c
    | AddOutletE Path.ToNode Path.Alias c
    | AddLinkE (Outlet d c) (Inlet d c)
    | ProcessWithE (Node d n) (ProcessF d)
    | SubscribeNodeProcess (Node d n)
    | InformNodeOnInletUpdates (Inlet d c) (Node d n)
    | InformNodeOnOutletUpdates (Outlet d c) (Node d n)
    | CancelNodeSubscriptions (Node d n)
    | CancelInletSubscriptions (Inlet d c)
    | CancelOutletSubscriptions (Outlet d c)
    | CancelLinkSubscriptions Link
    | SubscribeNodeUpdates (Node d n)
    | SendToInletE (Inlet d c) d
    | SendToOutletE (Outlet d c) d
    | SendActionOnInletUpdatesE (Inlet d c)
    | SendActionOnOutletUpdatesE (Outlet d c)
    | SendPeriodicallyToInletE (Inlet d c) Int (InletPeriodSubscription d)
    | SendPeriodicallyToOutletE (Outlet d c) Int (OutletPeriodSubscription d)
    | StreamToInletE (Inlet d c) (Event d)
    | StreamToOutletE (Outlet d c) (Event d)
    | SubscribeToInletE (Inlet d c) (InletSubscription d) -- TODO: should pass the canceler to the user
    | SubscribeToOutletE (Outlet d c) (OutletSubscription d) -- TODO: should pass the canceler to the user
    | SubscribeToNodeE
            (Node d n)
            (NodeInletsSubscription d)
            (NodeOutletsSubscription d)
-}

-- derive instance genericStringAction :: Generic StringAction _
-- instance eqStringAction :: Eq StringAction where
--   eq = GEq.genericEq
-- instance showStringAction :: Show StringAction where
--   show = GShow.genericShow



showKind :: forall d c n. Action d c n -> String
showKind NoOp = "NoOp"
showKind (Inner _) = "Inner"
showKind (Request _) = "Request"
showKind (Build _) = "Build"
showKind (Data _) = "Data"


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
    show (StreamToInlet inlet _) = "StreamToInlet " <> show inlet
    show (StreamToOutlet outlet _) = "StreamToOutlet " <> show outlet
    show (SendPeriodicallyToInlet inlet period _) =
        "SendPeriodicallyToInlet " <> show inlet <> " " <> show period


instance showInnerAction :: Show (InnerAction d c n) where
    show _ = "<Inner>"


instance showBuildAction :: (Show d, Show c, Show n) => Show (BuildAction d c n) where
    show (AddPatch patch) = "AddPatch " <> show patch
    show (AddNode node) = "AddNode " <> show node
    show (RemoveNode node) = "RemoveNode " <> show node
    show (AddInlet inlet) = "AddInlet " <> show inlet
    show (AddOutlet outlet) = "AddOutlet " <> show outlet
    show (RemoveInlet inlet) = "RemoveInlet " <> show inlet
    show (RemoveOutlet outlet) = "RemoveOutlet " <> show outlet
    show (Connect outlet inlet) = "Connect " <> show inlet <> " " <> show outlet
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
