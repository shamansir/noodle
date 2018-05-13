module Rpd.Render.Create
    ( createRenderer
    ) where

import Prelude

import Rpd as R
import Rpd.Render

import Rpd.Flow (Flow, create, subscribe, fold, sampleOn_)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Array (head, (:))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Filterable (filter)
import Control.Monad.Eff.Console (CONSOLE, log)


createRenderer :: forall d e. (Push d e -> UI d -> R.RenderEff e) -> R.Renderer d e
createRenderer render = (\nw -> do
    { flow : interactions, push : pushInteraction } <- create
    let
        uiMsgFlow = fold foldingF interactions $ UI init nw /\ NoOp
        uiFlow = map fst uiMsgFlow
        dataFoldingF' =
            dataFoldingF
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        dataFlow = fold dataFoldingF' uiMsgFlow $ pure (Map.empty /\ Map.empty)
    { flow : cancelers, push : saveCanceler } <- create
    { flow : cancelerTriggers, push : triggerPrevCanceler } <- create
    -- FIXME: remove logs and CONSOLE effect everywhere
    let
        subscribeData' =
            subscribeData
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        --pastCancelers = map (\{ last } -> last) $ Event.withLast cancelers
        triggeredCancelers = sampleOn_ cancelers cancelerTriggers
        networksBylinksChanged = map (\(UI _ network) -> network)
            $ filter (\(UI state _) -> state.areLinksChanged) uiFlow
    _ <- subscribe triggeredCancelers $ \cancel -> do
        log $ "cancel called."
        _ <- cancel
        pure unit
    _ <- subscribe networksBylinksChanged $ \nw -> do
        log "trigger prev cancel"
        triggerPrevCanceler unit
        log "subscribe"
        subscriber <- subscribeData' nw
        cancelNext <- subscriber
        log "save canceler"
        _ <- saveCanceler cancelNext
        pure unit
    _ <- do
        log "first subscription"
        subscriber <- subscribeData' nw
        cancelNext <- subscriber
        _ <- saveCanceler cancelNext
        pure unit
    _ <- subscribe uiFlow $ \ui -> render pushInteraction ui
    pushInteraction Init
)

foldingF :: forall d. Interaction d -> (UI d /\ Message d) -> (UI d /\ Message d)
foldingF interaction (ui@(UI state _) /\ _) =
    updateAndLog msg ui /\ msg
    where msg = interactionToMessage interaction state


dataFoldingF
    :: forall d e
     . (d -> R.InletPath -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
    -> (UI d /\ Message d)
    -> R.RpdEff e (R.Cancelers e)
    -> R.RpdEff e (R.Cancelers e)
dataFoldingF inletHandler outletHandler ((UI _ network) /\ msg) cancelersEff = do
    (allOutletCancelers /\ allInletCancelers) <- cancelersEff
    {- pure $ -}
    case msg of
        -- AddNode -> pure cancelers -- FIXME: implement
        SubscribeAllData -> do
            -- TODO: subscribe to all inlets, outlets and their sources
            -- subscriber <- subscribeData
            --     (pushInletData pushInteraction)
            --     (pushOutletData pushInteraction) network
            pure $ R.subscribeAll
                (\inlet _ d -> inletHandler d inlet)
                (\outlet d -> outletHandler d outlet)
                network
        ConnectTo inlet ->
            pure $ let
                canceler = do
                    c <- R.subscribeTop (\_ d -> inletHandler d inlet) inlet network
                    pure c
                allInletCancelers' = do
                    inletCancelers <- Map.lookup inlet allInletCancelers
                    canceler' <- canceler
                    let inletCancelers' = canceler' : inletCancelers
                        cancelers' = Map.insert inlet inletCancelers' allInletCancelers
                    pure cancelers'
            in allOutletCancelers /\ fromMaybe allInletCancelers allInletCancelers'
        DisconnectAt inlet -> do
            -- TODO: think on the fact that last source could be not the found one!
            -- (because user sources, etc.)
            -- currently the logic of connecting/disconnecting + update, kinda guarantees that
            -- it is the same one, however it's better to be sure and do not only trust the
            -- core logic to be conformant with this one, but also may be introduce IDs to ensure
            -- everything is properly arranged...
            -- What to do with the Links in the Network also?
            let maybeCancel = Map.lookup inlet allInletCancelers >>= head
            cancel <- fromMaybe (pure $ pure unit) maybeCancel
            _ <- cancel
            pure $ allOutletCancelers /\ allInletCancelers
        _ -> pure $ allOutletCancelers /\ allInletCancelers


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

