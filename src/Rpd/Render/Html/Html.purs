module Rpd.Render.Html where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Foldable (for_)
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Rpd as R
import Rpd.Render
--import Rpd.Render as Render
import FRP.Event (Event, create, subscribe)
import FRP.Event.Class as Event
-- import Signal as S
-- import Signal.Channel as SC
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup ((#!), (!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM


type Listener e = EventListener (R.RpdEff (dom :: DOM | e))


type Markup e = H.Markup (Listener e)


type DomRenderer d e = R.Renderer d ( dom :: DOM | e )


type PushF' d e = PushF d ( dom :: DOM | e )


renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer target nw = do
    --let maybeDataSignal = R.subscribeDataFlow nw
    { event : channel, push } <- create
    --let foldingF = f push
    push Start
    --evtChannel <- SC.channel Start
    --let evtSignal = SC.subscribe evtChannel
    let
        f evt ui =
            let ui' = update' evt ui
            in do
                render target ui' push
                -- if (evt == ConnectTo) then
                        -- R.subscribeDataFlow nw
                pure ui'
    Event.fold f channel (UI init nw)

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
    -> UI d
    -> PushF' d e
    -> R.RpdEff' (dom :: DOM | e)
render target ui pushF = do
    ToDOM.patch target $ do
        H.div $ H.text $ show ui
        network ui pushF


network :: forall d e. (Show d) => UI d -> PushF' d e -> Markup e
network ui@(UI (UIState s) (R.Network { patches })) pushF =
    H.div ! HA.className "network" $ do
        H.p $ H.text $ "Network: " <> (show $ length patches) <> "P"
        H.div ! HA.className "patches" $
            for_ patches (\p -> patch ui pushF p)


patch :: forall d e. (Show d) => UI d -> PushF' d e -> R.Patch d -> Markup e
patch ui pushF (R.Patch { id, name, nodes, links }) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show id <> ": " <> name <> "> "
                <> "N" <> (show $ length nodes) <> " "
                <> "L" <> (show $ length links)
            H.div ! HA.className "nodes" $
                for_ nodes (\n -> node ui pushF n)
        else
            H.p #! clickHandler $ H.text $ "[" <> show id <> "]"
    where
        isSelected = isPatchSelected (getSelection ui) id
        className = "patch " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt pushF $ Select (SPatch id)
        clickHandler = on "click" maybeSelect


node :: forall d e. (Show d) => UI d -> PushF' d e -> R.Node d -> Markup e
node ui pushF (R.Node { path, name, inlets, outlets }) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show path <> ": " <> name <> "> "
                <> "I" <> (show $ length inlets) <> " "
                <> "O" <> (show $ length outlets)
            H.div ! HA.className "inlets" $ for_ inlets (\i -> inlet ui pushF i)
            H.div ! HA.className "outlets" $ for_ outlets (\o -> outlet ui pushF o)
        else
            H.p #! clickHandler $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isNodeSelected (getSelection ui) path
        className = "node " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt pushF $ Select (SNode path)
        clickHandler = on "click" maybeSelect


inlet :: forall d e. (Show d) => UI d -> PushF' d e-> R.Inlet d -> Markup e
inlet ui@(UI (UIState s) _) pushF (R.Inlet { path, label, default, sources }) =
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
        maybeSelect = sendEvt pushF $ Select (SInlet path)
        maybeConnect = sendEvt pushF $ case s.connecting of
            Just outletPath -> ConnectTo path
            Nothing -> Skip
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel =
            if isWaitingForConnection then "(+)"
            else if length sources > 0 then "(" <> show (length sources) <> ")"
            else "(X)"
        dataText = show $ Map.lookup path s.lastInletData


outlet :: forall d e. (Show d) => UI d -> PushF' d e -> R.Outlet d -> Markup e
outlet ui@(UI (UIState s) _) pushF (R.Outlet { path, label }) =
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
        maybeSelect = sendEvt pushF $ Select (SOutlet path)
        maybeConnect = sendEvt pushF $ ConnectFrom path
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel = if isCurrentlyConnecting then "(*)" else "(+)"
        dataText = show $ Map.lookup path s.lastOutletData



sendEvt :: forall d e. PushF' d e -> Message d -> Listener e
sendEvt pushF evt =
    eventListener $ const $ pushF evt


