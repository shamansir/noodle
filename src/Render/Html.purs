module Render.Html where

import Prelude
import Render

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
import Signal as S
import Signal.Channel as SC
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup ((#!), (!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM


type Listener e = EventListener ( channel :: SC.CHANNEL, dom :: DOM | e )


type Markup e = H.Markup (Listener e)


type DomRenderer d e = R.Renderer d ( dom :: DOM | e )


renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer target nw = do
    let maybeDataSignal = R.subscribeDataSignal nw
    evtChannel <- SC.channel Start
    let evtSignal = SC.subscribe evtChannel
    let uiSignal = S.foldp update' (UI init nw) evtSignal
    case maybeDataSignal of
        Just dataSignal -> do
            let sendData = (\dataEvt -> SC.send evtChannel dataEvt)
            let renderDataSignal = dataSignal S.~> Data S.~> sendData
            S.runSignal renderDataSignal
        Nothing -> pure unit
    pure $ uiSignal S.~> (\ui -> render target ui evtChannel)


render
    :: forall d e
     . Show d
    => Element
    -> UI d
    -> UIChannel d
    -> Eff ( channel :: SC.CHANNEL, dom :: DOM | e ) Unit
render target ui ch = do
    ToDOM.patch target $ do
        H.div $ H.text $ show ui
        network ui ch


network :: forall d e. (Show d) => UI d -> UIChannel d -> Markup e
network ui@(UI (UIState s) (R.Network patches)) ch =
    H.div ! HA.className "network" $ do
        H.p $ H.text $ "Network: " <> (show $ length patches) <> "P"
        H.div ! HA.className "patches" $
            for_ patches (\p -> patch ui ch p)


patch :: forall d e. (Show d) => UI d -> UIChannel d -> R.Patch d -> Markup e
patch ui ch (R.Patch patchId label nodes links) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show patchId <> ": " <> label <> "> "
                <> "N" <> (show $ length nodes) <> " "
                <> "L" <> (show $ length links)
            H.div ! HA.className "nodes" $
                for_ nodes (\n -> node ui ch n)
        else
            H.p #! clickHandler $ H.text $ "[" <> show patchId <> "]"
    where
        isSelected = isPatchSelected (getSelection ui) patchId
        className = "patch " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt ch $ Select (SPatch patchId)
        clickHandler = on "click" maybeSelect


node :: forall d e. (Show d) => UI d -> UIChannel d -> R.Node d -> Markup e
node ui ch (R.Node nodePath name inlets outlets _) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show nodePath <> ": " <> name <> "> "
                <> "I" <> (show $ length inlets) <> " "
                <> "O" <> (show $ length outlets)
            H.div ! HA.className "inlets" $ for_ inlets (\i -> inlet ui ch i)
            H.div ! HA.className "outlets" $ for_ outlets (\o -> outlet ui ch o)
        else
            H.p #! clickHandler $ H.text $ "[" <> show nodePath <> "]"
    where
        isSelected = isNodeSelected (getSelection ui) nodePath
        className = "node " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt ch $ Select (SNode nodePath)
        clickHandler = on "click" maybeSelect


inlet :: forall d e. (Show d) => UI d -> UIChannel d -> R.Inlet d -> Markup e
inlet ui@(UI (UIState s) _) ch (R.Inlet path label maybeDefault sources) =
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
        maybeSelect = sendEvt ch $ Select (SInlet path)
        maybeConnect = sendEvt ch $ case s.connecting of
            Just outletPath -> ConnectTo path
            Nothing -> Skip
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel =
            if isWaitingForConnection then "(+)"
            else if length sources > 0 then "(" <> show (length sources) <> ")"
            else "(X)"
        dataText = show $ Map.lookup path s.lastInletData


outlet :: forall d e. (Show d) => UI d -> UIChannel d -> R.Outlet d -> Markup e
outlet ui@(UI (UIState s) _) ch (R.Outlet path label _) =
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
        maybeSelect = sendEvt ch $ Select (SOutlet path)
        maybeConnect = sendEvt ch $ ConnectFrom path
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel = if isCurrentlyConnecting then "(*)" else "(+)"
        dataText = show $ Map.lookup path s.lastOutletData



sendEvt :: forall d e. UIChannel d -> Event d -> Listener e
sendEvt ch evt =
    eventListener $ const $ SC.send ch evt


