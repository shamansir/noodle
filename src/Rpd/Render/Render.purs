module Rpd.Render
    ( UI(..)
    , UIState(..)
    , Push
    , Message(..), Selection(..), getSelection, getConnecting
    , isPatchSelected, isNodeSelected, isInletSelected, isOutletSelected
    , init, update, subscribeData, areLinksChanged
    ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Rpd as R
-- import Signal.Channel as SC


newtype UIState d =
    UIState
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


data Message d
    = Start
    | Skip
    | ConnectFrom R.OutletPath
    | ConnectTo R.InletPath
    | Drag Int Int
    | DataAtInlet R.InletPath d
    | DataAtOutlet R.OutletPath d
    | Select Selection


data Selection
    = SNone
    | SNetwork -- a.k.a. None ?
    | SPatch R.PatchId
    | SNode R.NodePath
    | SInlet R.InletPath
    | SOutlet R.OutletPath
    | SLink R.LinkId


data UI d = UI (UIState d) (R.Network d)


type Push d e = Message d -> R.RpdEff e Unit


init :: forall d. UIState d
init =
    UIState
        { selection : SNone
        , dragging : Nothing
        , connecting : Nothing
        , lastInletData : Map.empty
        , lastOutletData : Map.empty
        , areLinksChanged : false
        , lastMessages : []
        }


update :: forall d. Message d -> UI d -> UI d
update (DataAtInlet inletPath d) (UI (UIState state) network) =
    UI
        (UIState $
            state
                { lastInletData =
                    Map.insert inletPath d state.lastInletData
                })
        network
update (DataAtOutlet outletPath d) (UI (UIState state) network) =
    UI
        (UIState $
            state
                { lastOutletData =
                    Map.insert outletPath d state.lastOutletData
                })
        network
update (ConnectFrom outletPath) (UI (UIState state) network) =
    UI (UIState $ state { connecting = Just outletPath }) network
update (ConnectTo inletPath) (UI (UIState state) network) =
    UI (UIState $ state { connecting = Nothing }) network'
    where
        network' =
            case state.connecting of
                Just outletPath -> fromMaybe network $ R.connect' outletPath inletPath network
                Nothing -> network
update (Select selection) ui =
    case select selection $ getSelection ui of
        Just newSelection -> setSelection newSelection ui
        Nothing -> ui
update _ ui = ui


-- dataSubUnsub
--     :: forall d e
--      . (R.Canceller e -> R.RpdEff e Unit)
--     -> (d -> R.InletPath -> R.RpdEff e Unit)
--     -> (d -> R.OutletPath -> R.RpdEff e Unit)
--     -> R.Network d
--     -> Maybe (R.Canceller e)
--     -> R.RpdEff e Unit
-- dataSubUnsub push inletHandler outletHandler network maybeCancel = do
--     cancelPrev <- fromMaybe (pure $ pure unit) maybeCancel
--     cancelPrev
--     let cancelNext = R.subscribeDataFlow network inletHandler outletHandler
--     push $ cancelNext
--     pure unit


-- dataSubUnsub
--     :: forall d e
--      . (d -> R.InletPath -> R.RpdEff e Unit)
--     -> (d -> R.OutletPath -> R.RpdEff e Unit)
--     -> R.Network d
--     -> Maybe (R.Canceller e)
--     -> Maybe (R.Canceller e) /\ R.Subscriber e
-- dataSubUnsub inletHandler outletHandler network maybeCancel =
--     maybeCancelPrev /\ subscribe
--     where
--         maybeCancelPrev = maybeCancel
--         subscribe = (\_ -> R.subscribeDataFlow network inletHandler outletHandler)


subscribeData
    :: forall d e
     . (d -> R.InletPath -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
    -> R.Network d
    -> (Unit -> R.Canceller e)
subscribeData inletHandler outletHandler network =
    \_ -> R.subscribeDataFlow network inletHandler outletHandler


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


getSelection :: forall d. UI d -> Selection
getSelection (UI (UIState s) _) = s.selection


getConnecting :: forall d. UI d -> Maybe R.OutletPath
getConnecting (UI (UIState s) _) = s.connecting


setSelection :: forall d. Selection -> UI d -> UI d
setSelection newSelection (UI (UIState s) network) =
    UI (UIState $ s { selection = newSelection }) network


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


instance showUIState :: (Show d) => Show (UIState d) where
    show (UIState { selection, dragging, connecting, lastInletData, lastOutletData, lastMessages })
        = "Selection: " <> show selection <>
        ", Dragging: " <> show dragging <>
        ", Connecting: " <> show connecting <>
        ", Inlets: " <> show lastInletData <>
        ", Outlets: " <> show lastOutletData <>
        ", Last events: " <> show (Array.reverse lastMessages)


instance showUI :: (Show d) => Show (UI d) where
    show (UI state _ ) = show state


instance showMessage :: (Show d) => Show (Message d) where
    show Start = "Start"
    show Skip = "Skip"
    show (ConnectFrom outletPath) = "Start connecting from " <> show outletPath
    show (ConnectTo inletPath) = "Finish connecting at " <> show inletPath
    -- | Drag Int Int
    -- | Data (R.DataMsg d)
    show (Select selection) = "Select " <> show selection
    show (DataAtInlet inletPath d) = "InletData " <> show inletPath <> " " <> show d
    show (DataAtOutlet outletPath d) = "OutletData " <> show outletPath <> " " <> show d
    show _ = "?"
