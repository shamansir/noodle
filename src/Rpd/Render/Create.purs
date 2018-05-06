module Rpd.Render.Create
    ( createRenderer
    ) where

import Prelude

import Rpd as R
import Rpd.Render

import Rpd.Flow (create, subscribe, fold, sampleOn_)

import Control.Monad.Eff (Eff)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Filterable (filter)
import Control.Monad.Eff.Console (CONSOLE, log)


type Cancellers e = Map R.InletPath (Map Int (R.Canceller e))


createRenderer :: forall d e. (Push d e -> UI d -> R.RenderEff e) -> R.Renderer d e
createRenderer render = (\nw -> do
    { flow : interactions, push : pushInteraction } <- create
    -- { event : messages, push : pushMsg } <- create
    -- TODO: Event UIState/Interaction + Event Network -> Event UI/Message
    -- FIXME: pass fired interactions to messages flow and adapt them
    let
        foldingF = \interaction ui@(UI state _) ->
            updateAndLog (interactionToMessage interaction state) ui
        -- TODO: Event.fold update messages $ UI init nw
        uiFlow = fold foldingF interactions $ UI init nw
        -- messages are lost, but we need them to know whar to do
        -- may be we need a flow where messages and states will be in a tuple
        -- (Writer Monad?)
        cancellers_ :: Eff e (Cancellers e)
        cancellers_ = pure Map.empty
        dataFoldingF = \ui cancellers -> cancellers
        dataFlow = fold dataFoldingF uiFlow $ cancellers_
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

