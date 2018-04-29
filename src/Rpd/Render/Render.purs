module Rpd.Render
    ( UI(..)
    , UIState
    , Push
    , Message(..), Selection(..)
    , isPatchSelected, isNodeSelected, isInletSelected, isOutletSelected
    , init, update, subscribeData, areLinksChanged
    ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing)
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
    , lastMessages :: Array (Message d) -- FIXME: remove
    }


-- data Message d
--     = Init
--     | Select Selection -- TrySelecting Selection
--     | ConnectFrom R.OutletPath
--     | ConnectTo R.InletPath
--     | DisconnectAt R.InletPath
--     | DataAtInlet R.InletPath d
--     | DataAtOutlet R.OutletPath d


data Message d
    = Init
    | Click ClickSubject
    | DataAtInlet R.InletPath d
    | DataAtOutlet R.OutletPath d


data Selection
    = SNone
    | SNetwork -- a.k.a. None ?
    | SPatch R.PatchId
    | SNode R.NodePath
    | SInlet R.InletPath
    | SOutlet R.OutletPath
    | SLink R.LinkId


data ClickSubject
    = CSNetwork -- a.k.a. None / Background ?
    | CSPatch R.PatchId
    | CSNode R.NodePath
    | CSInlet R.InletPath
    | CSInletConnector R.InletPath
    | CSOutlet R.OutletPath
    | CSOutletConnector R.OutletPath
    | CSLink R.LinkId


data UI d = UI (UIState d) (R.Network d)


type Push d e = Message d -> R.RpdEff e Unit


init :: forall d. UIState d
init =
    { selection : SNone
    , dragging : Nothing
    , connecting : Nothing
    , lastInletData : Map.empty
    , lastOutletData : Map.empty
    , areLinksChanged : false
    , lastMessages : []
    }


update :: forall d. Message d -> UI d -> UI d
update msg (UI state network) =
    update' msg (UI state' network)
    where
        state' = state { areLinksChanged = false }


update' :: forall d. Message d -> UI d -> UI d
update' (DataAtInlet inletPath d) (UI state network) =
    UI state' network
    where
        state' =
            state
                { lastInletData =
                    Map.insert inletPath d state.lastInletData
                }
update' (DataAtOutlet outletPath d) (UI state network) =
    UI state' network
    where
        state' =
            state
                { lastOutletData =
                    Map.insert outletPath d state.lastOutletData
                }
update' (Click (CSOutletConnector outletPath)) (UI state network) =
    UI state' network
    where
        state' = state { connecting = Just outletPath }
update' (Click (CSInletConnector inletPath)) (UI state network)
    | isJust state.connecting =
    UI state' network'
    where
        state' =
            state { connecting = Nothing, areLinksChanged = true }
        network'=
            case state.connecting of
                Just outletPath ->
                    fromMaybe network $ R.connect' outletPath inletPath network
                Nothing -> network
update' (Click (CSInletConnector inletPath)) (UI state network)
    | isNothing state.connecting =
    UI state' network'
    where
        network' = fromMaybe network $ R.disconnectLast inletPath network
        state' = state { areLinksChanged = true }
update' (Click subject) (UI state network)
    | affectsSelection subject =
    UI state' network
    where
        selection = fromMaybe state.selection $ subjectToSelection subject
        state' =
            case select selection state.selection of
                Just newSelection -> state { selection = newSelection }
                Nothing -> state -- SNone?


-- affectsSelection :: ClickSubject -> Boolean


-- subjectToSelection :: ClickSubject -> Selection


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


areLinksChanged :: forall d. Message d -> Boolean
areLinksChanged (ConnectTo _) = true
areLinksChanged _ = false


select :: forall d. Selection -> Selection -> Maybe Selection
select newSelection SNone = Just newSelection
select (SPatch newPatch) prevSelection   | isPatchSelected prevSelection newPatch = Just SNone
                                         | otherwise = Just (SPatch newPatch)
select (SNode newNode) prevSelection     | isNodeSelected prevSelection newNode =
                                                Just (SPatch $ R.getPatchOfNode newNode)
                                         | otherwise = Just (SNode newNode)
select (SInlet newInlet) prevSelection   | isInletSelected prevSelection newInlet =
                                                Just (SNode $ R.getNodeOfInlet newInlet)
                                         | otherwise = Just (SInlet newInlet)
select (SOutlet newOutlet) prevSelection | isOutletSelected prevSelection newOutlet =
                                                Just (SNode $ R.getNodeOfOutlet newOutlet)
                                         | otherwise = Just (SOutlet newOutlet)
select SNone _ = Just SNone
select _ _ = Nothing


isPatchSelected :: Selection -> R.PatchId -> Boolean
isPatchSelected (SPatch selectedPatchId) patchId = selectedPatchId == patchId
isPatchSelected (SNode nodePath) patchId = R.isNodeInPatch nodePath patchId
isPatchSelected (SInlet inletPath) patchId = R.isInletInPatch inletPath patchId
isPatchSelected (SOutlet outletPath) patchId = R.isOutletInPatch outletPath patchId
isPatchSelected _ _ = false


isNodeSelected :: Selection -> R.NodePath -> Boolean
isNodeSelected (SNode selectedNodePath) nodePath = selectedNodePath == nodePath
isNodeSelected (SInlet inletPath) nodePath = R.isInletInNode inletPath nodePath
isNodeSelected (SOutlet outletPath) nodePath = R.isOutletInNode outletPath nodePath
isNodeSelected _ _ = false


isInletSelected :: forall d. Selection -> R.InletPath -> Boolean
isInletSelected (SInlet selectedInletPath) inletPath = selectedInletPath == inletPath
isInletSelected _ _ = false


isOutletSelected :: forall d. Selection -> R.OutletPath -> Boolean
isOutletSelected (SOutlet selectedOutletPath) outletPath = selectedOutletPath == outletPath
isOutletSelected _ _ = false


instance showSelection :: Show Selection where
    show SNone = "Nothing"
    show SNetwork = "Network"
    show (SPatch patchId) = show patchId
    show (SNode nodePath) = show nodePath
    show (SInlet inletPath) = show inletPath
    show (SOutlet outletPath) = show outletPath
    show (SLink linkId) = show linkId


instance showClickSubject :: Show ClickSubject where
    show SNone = "Nothing"
    show SNetwork = "Network"
    show (SPatch patchId) = show patchId
    show (SNode nodePath) = show nodePath
    show (SInlet inletPath) = show inletPath
    show (SOutlet outletPath) = show outletPath
    show (SLink linkId) = show linkId


instance showUI :: (Show d) => Show (UI d) where
    show (UI { selection, dragging, connecting, lastInletData, lastOutletData, lastMessages } _)
        = "Selection: " <> show selection <>
        ", Dragging: " <> show dragging <>
        ", Connecting: " <> show connecting <>
        ", Inlets: " <> show lastInletData <>
        ", Outlets: " <> show lastOutletData <>
        ", Last events: " <> show (Array.reverse lastMessages)


instance showMessage :: (Show d) => Show (Message d) where
    show Init = "Init"
    show (Click clickSubject) = "Start connecting from " <> show clickSubject
    show (ConnectTo inletPath) = "Connect to " <> show inletPath
    show (DisconnectAt inletPath) = "Disconnect at " <> show inletPath
    -- | Drag Int Int
    -- | Data (R.DataMsg d)
    show (Select selection) = "Select " <> show selection
    show (InformDataAtInlet inletPath d) = "InletData " <> show inletPath <> " " <> show d
    show (InformDataAtOutlet outletPath d) = "OutletData " <> show outletPath <> " " <> show d
    show _ = "?"
