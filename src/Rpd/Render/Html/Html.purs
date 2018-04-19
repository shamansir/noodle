module Rpd.Render.Html where

import Prelude
import Rpd.Render

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Data.Array ((:))
import Data.Array (length)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
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

data LinksState = NoLinksChanged | LinksChanged

type State d e = UI d /\ LinksState /\ Maybe (Canceller' e)


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

    { event : channel, push } <- create
    let uiFlow = Event.fold update' channel $ init' nw
    _ <- subscribe uiFlow $ \ui -> render' target push ui
    push Start


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


render'
    :: forall d e
     . Show d
    => Element
    -> Push' d e
    -> State d e
    -> R.RenderEff ( dom :: DOM | e )
render' target push (ui /\ NoLinksChanged /\ _) = render target push ui
render' target push (ui /\ LinksChanged /\ maybeCancel) = do
    cancelPrev <- fromMaybe (pure $ pure unit) maybeCancel
    cancelPrev
    let (UI _ network) = ui
    let cancelNext = subscribeDataFlow' push network
    render' target push (ui /\ NoLinksChanged /\ Just cancelNext)


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


areLinksChanged :: forall d. Message d -> LinksState
areLinksChanged (ConnectTo _) = LinksChanged
areLinksChanged _ = NoLinksChanged


-- updateAndLog :: forall d e. Event d -> UI d -> String /\ UI d


isMeaningfulMessage :: forall d. Message d -> Boolean
isMeaningfulMessage Start = true
isMeaningfulMessage Skip = true
isMeaningfulMessage (ConnectFrom _) = true
isMeaningfulMessage (ConnectTo _) = true
isMeaningfulMessage (Select _) = true
isMeaningfulMessage _ = false
-- isMeaningfulMessage _ = true


update' :: forall d e. Message d -> State d e -> State d e
update' msg (ui /\ linksState /\ c) =
    let
        UI (UIState state) network = update msg ui
        linksState' = areLinksChanged msg
        state' =
            if isMeaningfulMessage msg then
                state { lastMessages = Array.take 5 $ msg : state.lastMessages }
            else
                state

    in
        UI (UIState state') network /\ linksState' /\ c


init' :: forall d e. R.Network d -> State d e
init' nw = UI init nw /\ NoLinksChanged /\ Nothing
