module Rpd.Render
    ( UI(..)
    , UIState
    , Push
    , Message(..), Interaction(..), Selection(..), Subject(..)
    , isPatchSelected, isNodeSelected, isInletSelected, isOutletSelected
    , init, update, updateAndLog, subscribeData
    , interactionToMessage -- FIXME: Do not expose
    ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing)
import Data.Set (Set(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\), type (/\))
import Rpd as R
-- import Signal.Channel as SC


-- newtype UIState d =
--     UIState
type UIState d =
    { selection :: Selection
    , dragging :: Maybe R.NodePath
    , connecting :: Maybe R.OutletPath
    , lastInletData :: Map R.InletPath d
    , lastOutletData :: Map R.OutletPath d
    , areLinksChanged :: Boolean
    -- TODO: lastConnection: Maybe Link
    -- , prevCanceller :: Maybe (R.Canceller e)
    , lastMessages :: Array (Message d) -- FIXME: remove, make some friendly debugger or History plugin to track it
    , lastInteractions :: Array (Interaction d) -- FIXME: remove, make some friendly debugger or History plugin to track it
    -- , friendlyLog :: String -- FIXME: remove, as well as the above
    }


data Message d
    = NoOp
    | AffectSelection Subject
    | ConnectFrom R.OutletPath
    | ConnectTo R.InletPath
    | DisconnectAt R.InletPath
    | SubscribeData R.InletPath
    | SubscribeAllData
    | OpenPatch R.PatchId
    | ClosePatch R.PatchId
    | SendDataToInlet R.InletPath d
    | SendDataToOutlet R.OutletPath d
    | Batch (Array (Message d))


data Interaction d
    = Init
    | Click Subject
    | DataAtInlet R.InletPath d
    | DataAtOutlet R.OutletPath d


data Selection -- TODO: allow multiple selections
    = SNone
    | SNetwork
    | SPatch R.PatchId
    | SNodes (Set R.NodePath)
    | SInlets (Set R.InletPath)
    | SOutlets (Set R.OutletPath)
    | SLinks (Set R.LinkId)


data Subject
    = CSNetwork -- a.k.a. None / Background ?
    | CSPatch R.PatchId
    | CSNode R.NodePath
    | CSNodeHandle R.NodePath
    | CSInlet R.InletPath
    | CSInletConnector R.InletPath
    | CSOutlet R.OutletPath
    | CSOutletConnector R.OutletPath
    | CSLink R.LinkId


data SelectionMode
    = Multiple
    | Single

data UI d = UI (UIState d) (R.Network d)


type Push d e = Interaction d -> R.RpdEff e Unit


init :: forall d. UIState d
init =
    { selection : SNone
    , dragging : Nothing
    , connecting : Nothing
    , lastInletData : Map.empty
    , lastOutletData : Map.empty
    , areLinksChanged : false
    , lastMessages : []
    , lastInteractions : []
    -- , friendlyLog : ""
    }


update :: forall d. Message d -> UI d -> UI d
update msg (UI state network) =
    update' msg (UI state' network)
    where
        --msg = interactionToMessage interaction state
        state' = state
            { areLinksChanged = false
            --, friendlyLog = ""
            }


update' :: forall d. Message d -> UI d -> UI d
update' (SendDataToInlet inletPath d) (UI state network) =
    UI state' network
    where
        state' =
            state
                { lastInletData =
                    Map.insert inletPath d state.lastInletData
                }
update' (SendDataToOutlet outletPath d) (UI state network) =
    UI state' network
    where
        state' =
            state
                { lastOutletData =
                    Map.insert outletPath d state.lastOutletData
                }
update' (ConnectFrom outletPath) (UI state network) =
    UI state' network
    where
        state' = state
            { connecting = Just outletPath
            --, friendlyLog = "connect from " <> show outletPath
            }
update' (ConnectTo inletPath) (UI state network)
    | isJust state.connecting =
    UI state' network'
    where
        state' =
            state
                { connecting = Nothing
                , areLinksChanged = true
                --, friendlyLog = "connect " <> maybe "?" show state.connecting
                --        <> " to " <> show inletPath
                }
        network'=
            case state.connecting of
                Just outletPath ->
                    fromMaybe network $ R.connect' outletPath inletPath network
                Nothing -> network
update' (DisconnectAt inletPath) (UI state network)
    | isNothing state.connecting =
    UI state' network'
    where
        network' = fromMaybe network $ R.disconnectLast inletPath network
        state' = state
            { areLinksChanged = true
            --, friendlyLog = "disconnect last at " <> show inletPath
            }
update' (AffectSelection subject) (UI state network)
    | affectsSelection subject =
    UI state' network
    where
        newSelection = affectSelection state.selection Single subject
        state' =
            state
                { selection = newSelection
                --, friendlyLog = "select " <> show newSelection
                }
update' _ ui = ui


-- updateAndLog :: forall d e. Event d -> UI d -> String /\ UI d

shouldLogMessage :: forall d. Message d -> Boolean
shouldLogMessage (SendDataToInlet _ _) = false
shouldLogMessage (SendDataToOutlet _ _) = false
shouldLogMessage _ = true

-- TODO: use Writer monad for logging
updateAndLog :: forall d. Message d -> UI d -> UI d
updateAndLog msg ui =
    let
        UI state network = update msg ui
        state' =
            if shouldLogMessage msg then
                state { lastMessages = Array.take 5 $ msg : state.lastMessages }
            else
                state
    in
        UI state' network


subscribeData
    :: forall d e
     . (d -> R.InletPath -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
    -> R.Network d
    -> R.Subscriber e
subscribeData inletHandler outletHandler network = do
    log "aaa"
    R.subscribeDataFlow inletHandler outletHandler network


-- TODO:
-- addLog :: forall d x. (Message d -> UI d -> x) -> Writer (Array (Message d)) x
-- addLog f =
--     \msg ui -> do
--         tell msg
--         pure $ f msg ui


-- areLinksChanged :: forall d. Message d -> Boolean
-- areLinksChanged (ConnectTo _) = true
-- areLinksChanged _ = false


-- TODO: rename to `subscribe` and `Sub`s, like in Elm
-- https://github.com/elm-lang/mouse/blob/master/src/Mouse.elm
interactionToMessage :: forall d. Interaction d -> UIState d -> Message d
interactionToMessage (Click (CSInletConnector inlet)) state
    | isJust state.connecting = ConnectTo inlet
    | otherwise = DisconnectAt inlet
interactionToMessage (Click (CSOutletConnector outlet)) state = ConnectFrom outlet
interactionToMessage (Click subj) _ | affectsSelection subj = AffectSelection subj
interactionToMessage (DataAtInlet inlet d) _ = SendDataToInlet inlet d
interactionToMessage (DataAtOutlet outlet d) _ = SendDataToOutlet outlet d
interactionToMessage _ _ = NoOp



affectsSelection :: Subject -> Boolean
affectsSelection (CSInletConnector _) = false
affectsSelection (CSOutletConnector _) = false
affectsSelection _ = true

{-
if patch was clicked, we select it, deselect and close all the others, and open it, unless it was already opened.
if node title was clicked, we expand or collapse it
if node was clicked, we select it and only it. if it was clicked with modifier, we add or remove it to/from selection.
same for inlets, outlets, and links
-}
affectSelection :: Selection -> SelectionMode -> Subject -> Selection
affectSelection _ _ CSNetwork = SNetwork
affectSelection prevSelection Single (CSPatch newPatch)
    | isPatchSelected prevSelection newPatch = SNone
    | otherwise = SPatch newPatch
affectSelection prevSelection Single (CSNode newNode)
    | isNodeSelected prevSelection newNode = SNone
    | otherwise = SNodes $ Set.singleton newNode
affectSelection prevSelection Single (CSInlet newInlet)
    | isInletSelected prevSelection newInlet = SNone
    | otherwise = SInlets $ Set.singleton newInlet
affectSelection prevSelection Single (CSOutlet newOutlet)
    | isOutletSelected prevSelection newOutlet = SNone
    | otherwise = SOutlets $ Set.singleton newOutlet
affectSelection prevSelection Single (CSLink newLink) = SLinks $ Set.singleton newLink
affectSelection prevSelection _ _ = prevSelection


-- select :: forall d. Selection -> Selection -> Maybe Selection
-- select newSelection SNone = Just newSelection
-- select (SPatch newPatch) prevSelection   | isPatchSelected prevSelection newPatch = Just SNone
--                                          | otherwise = Just (SPatch newPatch)
-- select (SNode newNode) prevSelection     | isNodeSelected prevSelection newNode =
--                                                 Just (SPatch $ R.getPatchOfNode newNode)
--                                          | otherwise = Just (SNode newNode)
-- select (SInlet newInlet) prevSelection   | isInletSelected prevSelection newInlet =
--                                                 Just (SNode $ R.getNodeOfInlet newInlet)
--                                          | otherwise = Just (SInlet newInlet)
-- select (SOutlet newOutlet) prevSelection | isOutletSelected prevSelection newOutlet =
--                                                 Just (SNode $ R.getNodeOfOutlet newOutlet)
--                                          | otherwise = Just (SOutlet newOutlet)
-- select SNone _ = Just SNone
-- select _ _ = Nothing


someSatisfy :: forall a. (a -> Boolean) -> Set a -> Boolean
someSatisfy predicate set =
    foldr (\elm res -> res || predicate elm) false set


isPatchSelected :: Selection -> R.PatchId -> Boolean
isPatchSelected (SPatch selectedPatchId) patchId = selectedPatchId == patchId
isPatchSelected (SNodes nodePaths) patchId =
    someSatisfy (flip R.isNodeInPatch $ patchId) nodePaths
isPatchSelected (SInlets inletPaths) patchId =
    someSatisfy (flip R.isInletInPatch $ patchId) inletPaths
isPatchSelected (SOutlets outletPaths) patchId =
    someSatisfy (flip R.isOutletInPatch $ patchId) outletPaths
isPatchSelected _ _ = false


isNodeSelected :: Selection -> R.NodePath -> Boolean
isNodeSelected (SNodes nodePaths) nodePath = someSatisfy ((==) nodePath) nodePaths
isNodeSelected (SInlets inletPaths) nodePath =
    someSatisfy (flip R.isInletInNode $ nodePath) inletPaths
isNodeSelected (SOutlets outletPaths) nodePath =
    someSatisfy (flip R.isOutletInNode $ nodePath) outletPaths
isNodeSelected _ _ = false


isInletSelected :: forall d. Selection -> R.InletPath -> Boolean
isInletSelected (SInlets inletPaths) inletPath = someSatisfy ((==) inletPath) inletPaths
isInletSelected _ _ = false


isOutletSelected :: forall d. Selection -> R.OutletPath -> Boolean
isOutletSelected (SOutlets outletPaths) outletPath = someSatisfy ((==) outletPath) outletPaths
isOutletSelected _ _ = false


instance showSelection :: Show Selection where
    show SNone = "Nothing"
    show SNetwork = "Network"
    show (SPatch patchId) = show patchId
    show (SNodes nodePaths) = show nodePaths
    show (SInlets inletPaths) = show inletPaths
    show (SOutlets outletPaths) = show outletPaths
    show (SLinks linkIds) = show linkIds


instance showSubject :: Show Subject where
    show CSNetwork = "Network"
    show (CSPatch patchId) = "Patch: " <> show patchId
    show (CSNode nodePath) = "Node: " <> show nodePath
    show (CSNodeHandle nodePath) = "Node title: " <> show nodePath
    show (CSInlet inletPath) = "Inlet: " <> show inletPath
    show (CSOutlet outletPath) = "Outlet: " <> show outletPath
    show (CSLink linkId) = "Link: " <> show linkId
    show (CSInletConnector inletPath) = "InletCon: " <> show inletPath
    show (CSOutletConnector outletPath) = "OutletCon: " <> show outletPath


instance showUI :: (Show d) => Show (UI d) where
    show (UI s _)
        = "Selection: " <> show s.selection <>
        ", Dragging: " <> show s.dragging <>
        ", Connecting: " <> show s.connecting <>
        ", Inlets: " <> show s.lastInletData <>
        ", Outlets: " <> show s.lastOutletData <>
        ", Last events: " <> show (Array.reverse s.lastMessages) <>
        ", Last interactions: " <> show (Array.reverse s.lastInteractions)
        --", Friendly log: " <> s.friendlyLog


instance showMessage :: (Show d) => Show (Message d) where
    show NoOp = "NoOp"
    show (AffectSelection subject) = "Affect selection " <> show subject
    show (ConnectFrom outletPath) = "Connect from " <> show outletPath
    show (ConnectTo inletPath) = "Connect to " <> show inletPath
    show (DisconnectAt inletPath) = "Disconnect at " <> show inletPath
    show (SubscribeData inletPath) = "Subscribe data from " <> show inletPath
    show SubscribeAllData = "Subscribe all data"
    show (OpenPatch patchId) = "Open patch " <> show patchId
    show (ClosePatch patchId) = "Close patch " <> show patchId
    show (SendDataToInlet inlet d) = "Data at inlet " <> show inlet <> " " <> show d
    show (SendDataToOutlet outlet d) = "Data at outlet " <> show outlet <> " " <> show d
    show (Batch messages) = "Batch: " <> show messages

instance showInteraction :: (Show d) => Show (Interaction d) where
    show Init = "Init"
    show (Click subject) = "Click " <> show subject
    show (DataAtInlet inlet d) = "Data at inlet " <> show inlet <> " " <> show d
    show (DataAtOutlet outlet d) = "Data at outlet " <> show outlet <> " " <> show d
