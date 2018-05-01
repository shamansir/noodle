module Rpd.Render.Create
    ( createRenderer
    ) where

import Prelude

import Rpd as R
import Rpd.Render

import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Class as Event

import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Filterable (filter)
import Control.Monad.Eff.Console (CONSOLE, log)


createRenderer :: forall d e. (Push e -> UI d -> R.RenderEff e) -> R.Renderer d e
createRenderer render = (\nw -> do
    { event : interactions, push : pushInteraction } <- create
    { event : messages, push : pushMsg } <- create
    let uiFlow = Event.fold updateAndLog messages $ UI init nw
    { event : cancellers, push : saveCanceller } <- create
    { event : cancellerTriggers, push : triggerPrevCanceller } <- create
    -- FIXME: remove logs and CONSOLE effect everywhere
    let
        subscribeData' = subscribeData (pushInletData pushMsg) (pushOutletData pushMsg)
        --pastCancellers = map (\{ last } -> last) $ Event.withLast cancellers
        pastCancellers = map (\c -> Just c) cancellers
        triggeredCancellers = Event.sampleOn_ pastCancellers cancellerTriggers
        networksBylinksChanged = map (\(UI _ network) -> network)
            $ filter (\(UI state _) -> state.areLinksChanged) uiFlow
    _ <- subscribe triggeredCancellers $ \maybeCancel -> do
        let cancel = fromMaybe (pure unit) maybeCancel
        log $ "cancel called: " <> maybe "empty" (const "some") maybeCancel
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
    pushMsg Init
)

pushInletData
    :: forall d e
     . (Message d -> R.RpdEff e Unit)
    -> (d -> R.InletPath -> R.RpdEff e Unit)
pushInletData push =
    (\d inletPath -> do
        -- log $ "Receive from " <> show inletPath
        push $ DataAtInlet inletPath d)


pushOutletData
    :: forall d e
     . (Message d -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
pushOutletData push =
    (\d outletPath -> do
        --log $ "Receive from " <> show outletPath
        push $ DataAtOutlet outletPath d)

