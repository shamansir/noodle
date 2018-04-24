module Rpd.Render.Html where

import Prelude
import Rpd.Render

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
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

-- type HtmlEffE e = R.RpdEffE ( dom :: DOM | e )
-- type HtmlEff e v = R.RenderEff (HtmlEffE e) v

-- FIXME: both CONSOLE and FRP should not be here
type Listener e = EventListener ( console :: CONSOLE, dom :: DOM, frp :: FRP | e )


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
    { event : messages, push : pushMsg } <- create
    let uiFlow = Event.fold update' messages $ UI init nw
    { event : cancellers, push : saveCanceller } <- create
    { event : cancellerTriggers, push : triggerPrevCanceller } <- create
    -- FIXME: remove logs and CONSOLE effect everywhere
    -- FIXME: move complex code to Render.purs
    let
        subscribeData' = subscribeData (pushInletData pushMsg) (pushOutletData pushMsg)
        pastCancellers = map (\{ last } -> last) $ Event.withLast cancellers
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
    _ <- subscribe uiFlow $ \ui -> render target pushMsg ui
    pushMsg Start


pushInletData
    :: forall d e
     . Push d e
    -> (d -> R.InletPath -> R.RpdEff e Unit)
pushInletData push =
    (\d inletPath -> do
        log $ "Receive from " <> show inletPath
        push $ DataAtInlet inletPath d)


pushOutletData
    :: forall d e
     . Push d e
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
pushOutletData push =
    (\d outletPath -> do
        log $ "Receive from " <> show outletPath
        push $ DataAtOutlet outletPath d)


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


network :: forall d e. (Show d) => Push' d e -> UI d -> Markup e
network push ui@(UI s (R.Network { patches })) =
    H.div ! HA.className "network" $ do
        H.p $ H.text $ "Network: " <> (show $ length patches) <> "P"
        H.div ! HA.className "patches" $
            for_ patches $ patch push ui


patch :: forall d e. (Show d) => Push' d e -> UI d -> R.Patch d -> Markup e
patch push ui@(UI s _) (R.Patch { id, name, nodes, links }) =
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
        isSelected = isPatchSelected s.selection id
        className = "patch " <> (if isSelected then "_selected" else "")
        maybeSelect = sendMsg push $ Select (SPatch id)
        clickHandler = on "click" maybeSelect


node :: forall d e. (Show d) => Push' d e -> UI d -> R.Node d -> Markup e
node push ui@(UI s _) (R.Node { path, name, inlets, outlets }) =
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
        isSelected = isNodeSelected s.selection path
        className = "node " <> (if isSelected then "_selected" else "")
        maybeSelect = sendMsg push $ Select (SNode path)
        clickHandler = on "click" maybeSelect


inlet :: forall d e. (Show d) => Push' d e -> UI d -> R.Inlet d -> Markup e
inlet push (UI s _) (R.Inlet { path, label, default, sources }) =
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
        isSelected = isInletSelected s.selection path
        isWaitingForConnection = fromMaybe false $ R.notInTheSameNode path <$> s.connecting
        className = "inlet" <> (if isSelected then " _selected" else " ")
            <> (if isWaitingForConnection then " _waiting" else " ")
        clickMsg :: Message d
        clickMsg =
            if isWaitingForConnection then ConnectTo path else Skip
        maybeSelect = sendMsg push $ Select (SInlet path)
        maybeConnect = sendMsg push clickMsg
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel =
            if isWaitingForConnection then "(+)" <> checkCon
            else if length sources > 0 then "(" <> show (length sources) <> ")" <> checkCon
            else "(X)" <> checkCon
        checkCon = maybe "Nah" (const "Jah") s.connecting <> show clickMsg
        dataText = show $ Map.lookup path s.lastInletData


outlet :: forall d e. (Show d) => Push' d e -> UI d -> R.Outlet d -> Markup e
outlet push (UI s _) (R.Outlet { path, label }) =
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
        isSelected = isOutletSelected s.selection path
        isConnectingSomething = isJust s.connecting
        isCurrentlyConnecting = fromMaybe false $ ((==) path) <$> s.connecting
        className = "outlet" <> (if isSelected then " _selected" else " ")
                     <> (if isConnectingSomething then " _waiting" else " ")
                     <> (if isCurrentlyConnecting then " _connecting" else " ")
        maybeSelect = sendMsg push $ Select (SOutlet path)
        maybeConnect = sendMsg push $ ConnectFrom path
        clickHandler = on "click" maybeSelect
        connectorClickHandler = on "click" maybeConnect
        connectorLabel = if isCurrentlyConnecting then "(*)" else "(+)"
        dataText = show $ Map.lookup path s.lastOutletData



sendMsg :: forall d e. (Show d) => Push' d e -> Message d -> Listener e
sendMsg push msg =
    -- eventListener $ const $ push evt
    -- _ <- log $ "<<<" <> show msg
    eventListener (\_ -> do
        log $ ">>>" <> show msg
        _ <- push msg
        pure unit
    )


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
update' msg ui =
    let
        UI state' network = update msg ui
        state'' =
            if isMeaningfulMessage msg then
                state' { lastMessages = Array.take 5 $ msg : state'.lastMessages }
            else
                state'

    in
        UI state'' network
