module Rpd.Render.Create
    ( createRenderer
    ) where

import Prelude

import Rpd as R
import Rpd.Render

import Rpd.Flow (create, subscribe, fold, sampleOn_)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Filterable (filter)
import Control.Monad.Eff.Console (CONSOLE, log)


type Cancellers e = Map R.InletPath (Map Int (R.Canceller e))


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
        dataFlow = fold dataFoldingF' uiMsgFlow $ pure Map.empty
    { flow : cancellers, push : saveCanceller } <- create
    { flow : cancellerTriggers, push : triggerPrevCanceller } <- create
    -- FIXME: remove logs and CONSOLE effect everywhere
    let
        subscribeData' =
            subscribeData
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        --pastCancellers = map (\{ last } -> last) $ Event.withLast cancellers
        triggeredCancellers = sampleOn_ cancellers cancellerTriggers
        networksBylinksChanged = map (\(UI _ network) -> network)
            $ filter (\(UI state _) -> state.areLinksChanged) uiFlow
    _ <- subscribe triggeredCancellers $ \cancel -> do
        log $ "cancel called."
        _ <- cancel
        pure unit
    _ <- subscribe networksBylinksChanged $ \nw -> do
        log "trigger prev cancel"
        triggerPrevCanceller unit
        log "subscribe"
        subscriber <- subscribeData' nw
        cancelNext <- subscriber
        log "save canceller"
        _ <- saveCanceller cancelNext
        pure unit
    _ <- do
        log "first subscription"
        subscriber <- subscribeData' nw
        cancelNext <- subscriber
        _ <- saveCanceller cancelNext
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
    -> Eff e (Cancellers e)
    -> Eff e (Cancellers e)
dataFoldingF inletHandler outletHandler ((UI _ network) /\ msg) cancellersEff = do
    cancellers <- cancellersEff
    pure $ case msg of
        SubscribeAllData -> cancellers
            -- TODO: subscribe to all inlets and their sources
        ConnectTo inlet ->
            -- how to ensure if it is the correct source, not user source?
            -- or we are not allowing user sources after running a network?
            case R.findTopSource inlet network of
                Just source ->
                    let dataFlow = R.getFlowOf source
                    in do
                        --canceller <- subscribe dataFlow (\d -> inletHandler d inlet)
                        cancellers
                Nothing -> cancellers
        DisconnectAt inlet ->
            -- how to ensure if it is the correct source, not user source?
            -- or we are not allowing user sources after running a network?
            case R.findTopSource inlet network of
                Just source ->
                    let dataFlow = R.getFlowOf source
                    in do
                        --canceller <- subscribe dataFlow (\d -> inletHandler d inlet)
                        cancellers
                Nothing -> cancellers
        _ -> cancellers


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

