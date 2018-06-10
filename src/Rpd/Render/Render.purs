module Rpd.Render
    ( Renderer, RenderEff
    , UI(..)
    , UIState
    , Push
    , Message(..), Interaction(..), Selection(..), Subject(..)
    , isPatchSelected, isNodeSelected, isInletSelected, isOutletSelected
    , create
    ) where

import Prelude

import Rpd
    ( Network
    , PatchId, NodePath, InletPath, OutletPath, LinkId
    , RpdEff
    , connect'
    , disconnectTop
    , isNodeInPatch, isInletInNode, isInletInPatch, isOutletInPatch, isOutletInNode
    ) as R
import Rpd.Flow
    ( Subscriber, Subscribers, Canceler, Cancelers
    , subscribeAll, subscribeTop
    , initCancelers
    ) as R

import Data.Array ((:), head, tail)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import FRP.Event as Event


type RenderEff e = R.RpdEff e Unit

type Renderer d e = R.Network d -> RenderEff e


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
    | SubscribeAllData
    | AffectSelection Subject
    | ConnectFrom R.OutletPath
    | ConnectTo R.InletPath
    | DisconnectAt R.InletPath
    | OpenPatch R.PatchId
    | ClosePatch R.PatchId
    | SendDataToInlet R.InletPath d
    | SendDataToOutlet R.OutletPath d


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


-- https://dvdsgl.co/2016/a-trello-monad-in-the-dark/
create :: forall d e. (Show d) => (Push d e -> UI d -> RenderEff e) -> Renderer d e
create render = \nw -> do
    { event : interactions, push : pushInteraction } <- Event.create
    --{ flow : subs, push : pushSubEff } <- create
    let
        uiMsgFlow = Event.fold updateUi interactions $ UI init nw /\ NoOp
        uiFlow = map fst uiMsgFlow
        trackTheFlow' =
            trackTheFlow
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        dataFlow = Event.fold trackTheFlow' uiMsgFlow R.initCancelers
    _ <- Event.subscribe dataFlow (\_ -> pure unit) -- TODO: perform eff on the result
    _ <- Event.subscribe uiMsgFlow $ \(ui /\ msg) -> do
        render pushInteraction ui
    pushInteraction Init
    -- TODO: try `sampleOn`, may be it's the more proper thing to use
    --       instead of `fold` in case of data subscriptions/cancels.
    {- The code below should work instead, when
       https://github.com/paf31/purescript-behaviors/issues/27
       is dealt with. Like, folds start fresh on every subscription,
       and it is what breaks the flow.
    -}
    {-
    { flow : interactions, push : pushInteraction } <- create
    let
        uiMsgFlow = fold foldingF interactions $ UI init nw /\ NoOp
        uiFlow = map fst uiMsgFlow
        dataFoldingF' =
            dataFoldingF
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        dataFlow = fold dataFoldingF' uiMsgFlow $ pure (Map.empty /\ Map.empty)
    _ <- subscribe dataFlow id
    _ <- subscribe uiFlow $ \ui -> render pushInteraction ui
    pushInteraction Init
    -}


updateUi :: forall d. Interaction d -> (UI d /\ Message d) -> (UI d /\ Message d)
updateUi interaction (ui@(UI state _) /\ _) =
    updateAndLog msg ui /\ msg
    where msg = interactionToMessage interaction state


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
update' (SubscribeAllData) ui =
    ui -- FIXME: implement
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
        network' = fromMaybe network $ R.disconnectTop inletPath network
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
-- update' (Batch messages) ui =
--     foldr update' ui messages
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


-- subscribeData
--     :: forall d e
--      . (d -> R.InletPath -> R.RpdEff e Unit)
--     -> (d -> R.OutletPath -> R.RpdEff e Unit)
--     -> R.Network d
--     -> R.Subscriber e
-- subscribeData inletHandler outletHandler network = do
--     log "aaa"
--     R.subscribeDataFlow inletHandler outletHandler network


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
interactionToMessage Init _ = SubscribeAllData
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


isInletSelected :: Selection -> R.InletPath -> Boolean
isInletSelected (SInlets inletPaths) inletPath = someSatisfy ((==) inletPath) inletPaths
isInletSelected _ _ = false


isOutletSelected :: Selection -> R.OutletPath -> Boolean
isOutletSelected (SOutlets outletPaths) outletPath = someSatisfy ((==) outletPath) outletPaths
isOutletSelected _ _ = false



trackTheFlow
    :: forall d e
     . (Show d)
    => (d -> R.InletPath -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
    -> (UI d /\ Message d)
    -> R.Cancelers e
    -> R.Cancelers e
trackTheFlow
    inletHandler
    outletHandler
    ((UI _ network) /\ msg)
    (allOutletCancelers /\ allInletCancelers) = do
    case msg of
        {- AddNode -> pure cancelers -- FIXME: implement -}
        SubscribeAllData ->
            let
                ( allOutletSubscribers /\ allInletSubscribers ) = subscribeAll network
                allOutletCancelers' :: Map R.OutletPath (R.Canceler e)
                allOutletCancelers' =
                   -- performSub <$> allOutletSubscribers
                    (\sub -> liftEff $ performSub sub) <$> allOutletSubscribers
                allInletCancelers' :: Map R.InletPath (Array (R.Canceler e))
                allInletCancelers' =
                    map performSub <$> allInletSubscribers
            in allOutletCancelers' /\ allInletCancelers'
            {- subscribe with function below and execute all -}
        ConnectTo inlet -> do
                let
                    maybeCanceler :: Maybe (R.Canceler e)
                    maybeCanceler = connectToInlet inlet network <#> performSub
                case maybeCanceler of
                    Just canceler ->
                        let
                            inletCancelers' :: Array (R.Canceler e)
                            inletCancelers' =
                                case Map.lookup inlet allInletCancelers of
                                    Just inletCancelers -> canceler : inletCancelers
                                    Nothing -> [ canceler ]
                            allInletCancelers' :: Map R.InletPath (Array (R.Canceler e))
                            allInletCancelers' =
                                Map.insert inlet inletCancelers' allInletCancelers
                        in allOutletCancelers /\ allInletCancelers'
                    Nothing -> allOutletCancelers /\ allInletCancelers
            {- subscribe with function below and execute subscriber,
            -- then insert the resulting canceler into the map -}
            -- connectToInlet inlet network
        DisconnectAt inlet -> do
            -- _ <- fromMaybe (pure unit) $ disconnectAtInlet inlet allInletCancelers <#> performCancel
            case disconnectAtInlet inlet allInletCancelers <#> performCancel of
                Just _ -> do
                    let
                        inletCancelers' :: Array (R.Canceler e)
                        inletCancelers' =
                            -- FIMXE: the search is performed second time, first time at disconnectAtInlet
                            case Map.lookup inlet allInletCancelers of
                                Just inletCancelers -> fromMaybe [] $ tail inletCancelers -- tail inletCancelers <|> []
                                Nothing -> [ ]
                        allInletCancelers' :: Map R.InletPath (Array (R.Canceler e))
                        allInletCancelers' =
                            Map.insert inlet inletCancelers' allInletCancelers
                    allOutletCancelers /\ allInletCancelers'
                Nothing -> allOutletCancelers /\ allInletCancelers
            {- execute the canceler returned from function below,
            -- then remove it from the map -}
        _ -> allOutletCancelers /\ allInletCancelers
    where
        performSub :: R.Subscriber e -> R.Canceler e
        performSub = unsafePerformEff -- FIXME: unsafe
        performCancel :: R.Canceler e -> Unit
        performCancel = unsafePerformEff -- FIXME: unsafe
        subscribeAll :: R.Network d -> R.Subscribers e
        subscribeAll =
            R.subscribeAll
                (\inlet _ d -> inletHandler d inlet)
                (\outlet d -> outletHandler d outlet)
        connectToInlet :: R.InletPath -> R.Network d -> Maybe (R.Subscriber e)
        connectToInlet inlet =
            R.subscribeTop (\_ d -> inletHandler d inlet) inlet
        disconnectAtInlet :: R.InletPath -> Map R.InletPath (Array (R.Canceler e)) -> Maybe (R.Canceler e)
        disconnectAtInlet inlet allInletCancelers' =
            Map.lookup inlet allInletCancelers' >>= head


pushInletData
    :: forall d e
     . (Interaction d -> R.RpdEff e Unit)
    -> (d -> R.InletPath -> R.RpdEff e Unit)
pushInletData push =
    (\d inletPath -> do
        -- log $ "Receive from " <> show inletPath
        push $ DataAtInlet inletPath d)


pushOutletData
    :: forall d e
     . (Interaction d -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
pushOutletData push =
    (\d outletPath -> do
        --log $ "Receive from " <> show outletPath
        push $ DataAtOutlet outletPath d)


showCancelers :: forall e. R.Cancelers e -> String
showCancelers (outletCancelers /\ inletCancelers) =
    show $ "Outlets: " <> (show $ Map.keys outletCancelers) <>
           "Inlets: " <> (show $ Map.keys inletCancelers)


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
    -- show Start = "Start"
    show SubscribeAllData = "Subscribe all data"
    show (AffectSelection subject) = "Affect selection " <> show subject
    show (ConnectFrom outletPath) = "Connect from " <> show outletPath
    show (ConnectTo inletPath) = "Connect to " <> show inletPath
    show (DisconnectAt inletPath) = "Disconnect at " <> show inletPath
    show (OpenPatch patchId) = "Open patch " <> show patchId
    show (ClosePatch patchId) = "Close patch " <> show patchId
    show (SendDataToInlet inlet d) = "Data at inlet " <> show inlet <> " " <> show d
    show (SendDataToOutlet outlet d) = "Data at outlet " <> show outlet <> " " <> show d
    -- show (Batch messages) = "Batch: " <> show messages

instance showInteraction :: (Show d) => Show (Interaction d) where
    show Init = "Init"
    show (Click subject) = "Click " <> show subject
    show (DataAtInlet inlet d) = "Data at inlet " <> show inlet <> " " <> show d
    show (DataAtOutlet outlet d) = "Data at outlet " <> show outlet <> " " <> show d
