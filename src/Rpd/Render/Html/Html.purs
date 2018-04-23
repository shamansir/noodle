module Rpd.Render.Html where

import Prelude
import Rpd.Render

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Data.Array ((:))
import Data.Array (length)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Filterable (filter)
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Tuple (curry, uncurry)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Class as Event
import Rpd as R
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup ((#!), (!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM


type Listener e = EventListener (dom :: DOM, frp :: FRP | e)


type Markup e = H.Markup (Listener e)


type DomRenderer d e = R.Renderer d ( dom :: DOM | e )


type Push' d e = Push d ( dom :: DOM | e )

type Canceller' e = R.Canceller ( dom :: DOM | e )


--renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer
    :: forall d e
     . (Show d)
    => Element
    -> R.Network d
    -> R.RenderEff ( dom :: DOM | e )
renderer target nw = do
    --    let maybeDataSignal = R.subscribeDataSignal nw
    --    evtChannel <- SC.channel Start
    --    let evtSignal = SC.subscribe evtChannel
    --    let uiSignal = S.foldp update' (UI init nw) evtSignal

    --let foldingF = f push
    --evtChannel <- SC.channel Start
    --let evtSignal = SC.subscribe evtChannel
    -- let
    --     f :: Message d -> UI d -> Eff (R.RpdEff (dom :: DOM | e)) (UI d)
    --     f msg ui = do
    --         let ui' = update' msg ui
    --         render target ui' push
    --         pure ui'
                -- if (evt == ConnectTo) then
                        -- R.subscribeDataFlow nw
                -- pure ui'

    -- TODO: when uiState.lastConnection is not Nothing,
    --       subscribe again to R.subscribeDataFlow,
    --       save the returned canceller,
    --       and then call this canceller on next such case,
    --       just before subscribing to a new flow.



    { event : messages, push : pushMsg } <- create
    let uiFlow = Event.fold update' messages $ UI init nw
    { event : cancellers, push : pushCanceller } <- create
    let subscribeData' = subscribeData (pushInletData pushMsg) (pushOutletData pushMsg)
    let filteredUiFlow = filter (\(UI (UIState state) _) -> state.areLinksChanged) uiFlow
    let subFlow = map (\(UI _ network) -> do subscribeData' network) filteredUiFlow -- (subscribeData' nw)
        -- this should be called on every links update, so it pushes the corresponding
        -- FromInlet/FromOutlet messages

        -- dataSubUnsub' = dataSubUnsub (pushInletData pushMsg) (pushOutletData pushMsg)


        -- subscribeData :: forall d e. UI d -> Maybe (R.Canceller e) -> R.RpdEff e (Maybe (R.Canceller e) /\ UI d)



        -- subscribeData ui@(UI (UIState state) network) maybeCancel | state.areLinksChanged = do
        --     nextCancel <- dataSubUnsub' network maybeCancel
        --     pure $ (Just nextCancel) /\ ui
        -- subscribeData ui@(UI (UIState state) _) maybeCancel = do pure $ maybeCancel /\ ui
        -- _ = Event.mapAccum (subscribeData) uiFlow Nothing


        -- linksChanged -- Event (Maybe Canceller e)
        -- sampleOn ~= merge ??, merge uiFlow + cancellers, because we push cancellers there
        -- dataFlow =
        --     Event.fold
        --         (\ui maybeCancel ->
        --             if ui.linksChanged
        --                 then dataSubUnsub' network maybeCancel
        --                 else unit
        --         ) uiFlow Nothing

    -- _ <- subscribe uiFlow $ \ui -> if state.linksChanged then dataSubUnsub' network cancelled else unit
    -- _ <- subscribe subUnsubFlow $ \(maybeCancel /\ subscribe) -> do
    --         _ <- fromMaybe (pure unit) maybeCancel
    --         _ <- subscribe unit
    --         pure unit
    _ <- subscribe cancellers $ \canceller -> do
        cancel <- canceller
        _ <- cancel
        pure unit
    _ <- subscribe subFlow $ \subscriber -> do
        let cancelNext = do subscriber unit
        pushCanceller cancelNext
        pure unit
    _ <- subscribe uiFlow $ \ui -> render target pushMsg ui
    --pushLinksChanged Nothing
    pushMsg Start


    -- renderDataSignal <- maybeDataSignal >>= \dataSignal -> do
    --     let sendData = \dataEvt -> SC.send evtChannel dataEvt
    --     dataSignal S.~> Data S.~> sendData
    -- S.runSignal renderDataSignal
    -- where
    --     f evt ui = do
    --         let ui' = update' evt ui
    --         render target ui push
    --         -- if (evt == ConnectTo) then
    --         -- R.subscribeDataFlow nw
    --         pure ui'

    -- case maybeDataSignal of
    --     Just dataSignal -> do
    --         let sendData = (\dataEvt -> SC.send evtChannel dataEvt)
    --         let renderDataSignal = dataSignal S.~> Data S.~> sendData
    --         S.runSignal renderDataSignal
    --     Nothing -> pure unit
    -- pure $ uiSignal S.~> (\ui -> render target ui evtChannel)

-- forall t13 t16 t27. Bind t16 => Applicative t16 => (Network t13 -> Maybe t27 -> t16 t27) -> UI t13 -> Maybe t27 -> t16 (Tuple (Maybe t27) (UI t13))
-- subscribeData subUnsub ui@(UI (UIState state) network) maybeCancel | state.areLinksChanged = do
--     nextCancel <- subUnsub network maybeCancel
--     pure $ (Just nextCancel) /\ ui
-- subscribeData _ ui@(UI (UIState state) _) maybeCancel = do pure $ maybeCancel /\ ui


pushInletData
    :: forall d e
     . Push d e
    -> (d -> R.InletPath -> R.RpdEff e Unit)
pushInletData push =
    (\d inletPath -> push $ DataAtInlet inletPath d)


pushOutletData
    :: forall d e
     . Push d e
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
pushOutletData push =
    (\d outletPath -> push $ DataAtOutlet outletPath d)


render
    :: forall d e
     . Show d
    => Element
    -> Push' d e
    -> UI d
    -> R.RenderEff ( dom :: DOM | e )
render target push ui =
    ToDOM.patch target $ do
        H.div $ H.text $ show ui
        network push ui


subscribeDataFlow' :: forall d e. Push' d e -> R.Network d -> Canceller' e
subscribeDataFlow' push network =
    R.subscribeDataFlow network
        (\d inletPath -> push $ DataAtInlet inletPath d)
        (\d outletPath -> push $ DataAtOutlet outletPath d)


network :: forall d e. (Show d) => Push' d e -> UI d -> Markup e
network push ui@(UI (UIState s) (R.Network { patches })) =
    H.div ! HA.className "network" $ do
        H.p $ H.text $ "Network: " <> (show $ length patches) <> "P"
        H.div ! HA.className "patches" $
            for_ patches $ patch push ui


patch :: forall d e. (Show d) => Push' d e -> UI d -> R.Patch d -> Markup e
patch push ui (R.Patch { id, name, nodes, links }) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show id <> ": " <> name <> "> "
                <> "N" <> (show $ length nodes) <> " "
                <> "L" <> (show $ length links)
            H.div ! HA.className "nodes" $
                for_ nodes $ node push ui
        else
            H.p #! clickHandler $ H.text $ "[" <> show id <> "]"
    where
        isSelected = isPatchSelected (getSelection ui) id
        className = "patch " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt push $ Select (SPatch id)
        clickHandler = on "click" maybeSelect


node :: forall d e. (Show d) => Push' d e -> UI d -> R.Node d -> Markup e
node push ui (R.Node { path, name, inlets, outlets }) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show path <> ": " <> name <> "> "
                <> "I" <> (show $ length inlets) <> " "
                <> "O" <> (show $ length outlets)
            H.div ! HA.className "inlets" $ for_ inlets $ inlet push ui
            H.div ! HA.className "outlets" $ for_ outlets $ outlet push ui
        else
            H.p #! clickHandler $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isNodeSelected (getSelection ui) path
        className = "node " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt push $ Select (SNode path)
        clickHandler = on "click" maybeSelect


inlet :: forall d e. (Show d) => Push' d e -> UI d -> R.Inlet d -> Markup e
inlet push ui@(UI (UIState s) _) (R.Inlet { path, label, default, sources }) =
    H.div ! HA.className className $
        if isSelected then
            H.div $ do
                H.span ! HA.className "connector" #! connectorClickHandler $ H.text $ connectorLabel
                H.span #! clickHandler $ H.text $ "<" <> show path <> ": " <> label <> "> "
                H.span $ H.text dataText
        else
            H.div $ do
                H.span ! HA.className "connector" #! connectorClickHandler $ H.text $ connectorLabel
                H.span #! clickHandler $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isInletSelected (getSelection ui) path
        isWaitingForConnection = fromMaybe false $ R.notInTheSameNode path <$> getConnecting ui
        className = "inlet" <> (if isSelected then " _selected" else " ")
            <> (if isWaitingForConnection then " _waiting" else " ")
        maybeSelect = sendEvt push $ Select (SInlet path)
        maybeConnect = sendEvt push $ case s.connecting of
            Just outletPath -> ConnectTo path
            Nothing -> Skip
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel =
            if isWaitingForConnection then "(+)"
            else if length sources > 0 then "(" <> show (length sources) <> ")"
            else "(X)"
        dataText = show $ Map.lookup path s.lastInletData


outlet :: forall d e. (Show d) => Push' d e -> UI d -> R.Outlet d -> Markup e
outlet push ui@(UI (UIState s) _) (R.Outlet { path, label }) =
    H.div ! HA.className className $
        if isSelected then
            H.div $ do
                H.span ! HA.className "connector" #! connectorClickHandler $ H.text $ connectorLabel
                H.span #! clickHandler $ H.text $ "<" <> show path <> ": " <> label <> "> "
                H.span $ H.text dataText
        else
            H.div $ do
                H.span ! HA.className "connector" #! connectorClickHandler $ H.text $ connectorLabel
                H.span #! clickHandler $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isOutletSelected (getSelection ui) path
        isConnectingSomething = isJust $ getConnecting ui
        isCurrentlyConnecting = fromMaybe false $ ((==) path) <$> getConnecting ui
        className = "outlet" <> (if isSelected then " _selected" else " ")
                     <> (if isConnectingSomething then " _waiting" else " ")
                     <> (if isCurrentlyConnecting then " _connecting" else " ")
        maybeSelect = sendEvt push $ Select (SOutlet path)
        maybeConnect = sendEvt push $ ConnectFrom path
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel = if isCurrentlyConnecting then "(*)" else "(+)"
        dataText = show $ Map.lookup path s.lastOutletData



sendEvt :: forall d e. Push' d e -> Message d -> Listener e
sendEvt push evt =
    eventListener $ const $ push evt


-- updateAndLog :: forall d e. Event d -> UI d -> String /\ UI d


isMeaningfulMessage :: forall d. Message d -> Boolean
isMeaningfulMessage Start = true
isMeaningfulMessage Skip = true
isMeaningfulMessage (ConnectFrom _) = true
isMeaningfulMessage (ConnectTo _) = true
isMeaningfulMessage (Select _) = true
isMeaningfulMessage _ = false
-- isMeaningfulMessage _ = true

-- TODO: use Writer monad
update' :: forall d. Message d -> UI d -> UI d
update' msg state =
    let
        UI (UIState state) network = update msg state
        linksState' = areLinksChanged msg
        state' =
            if isMeaningfulMessage msg then
                state { lastMessages = Array.take 5 $ msg : state.lastMessages }
            else
                state

    in
        UI (UIState state') network
