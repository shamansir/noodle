module Rpd.Render.Html where

import Prelude
import Rpd.Render
import Rpd.Render.Create

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


type FireInteraction e = Interaction -> Listener e


type Canceller' e = R.Canceller ( dom :: DOM | e )


--renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer
    :: forall d e
     . (Show d)
    => Element
    -> R.Renderer d ( dom :: DOM | e )
renderer target =
    createRenderer (\push ui -> render target push ui)


render
    :: forall d e
     . Show d
    => Element
    -> Push ( dom :: DOM | e )
    -> UI d
    -> R.RenderEff ( dom :: DOM | e )
render target push ui =
    ToDOM.patch target $ do
        H.div $ H.text $ show ui
        network fire ui
    where fire = prepareToFire push


network :: forall d e. (Show d) => FireInteraction e -> UI d -> Markup e
network fire ui@(UI s (R.Network { patches })) =
    H.div ! HA.className "network" $ do
        H.p $ H.text $ "Network: " <> (show $ length patches) <> "P"
        H.div ! HA.className "patches"
            $ for_ patches $ patch fire ui


patch :: forall d e. (Show d) => FireInteraction e -> UI d -> R.Patch d -> Markup e
patch fire ui@(UI s _) (R.Patch { id, name, nodes, links }) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! on "click" patchClick
                $ H.text $ "<" <> show id <> ": " <> name <> "> "
                    <> "N" <> (show $ length nodes) <> " "
                    <> "L" <> (show $ length links)
            H.div ! HA.className "nodes"
                $ for_ nodes $ node fire ui
        else
            H.p #! on "click" patchClick $ H.text $ "[" <> show id <> "]"
    where
        isSelected = isPatchSelected s.selection id
        className = "patch " <> (if isSelected then "_selected" else "")
        patchClick = fire $ Click (CSPatch id)


node :: forall d e. (Show d) => FireInteraction e -> UI d -> R.Node d -> Markup e
node fire ui@(UI s _) (R.Node { path, name, inlets, outlets }) =
    H.div ! HA.className className $
        if isSelected then do
            H.p #! on "click" nodeClick
                $ H.text $ "<" <> show path <> ": " <> name <> "> "
                    <> "I" <> (show $ length inlets) <> " "
                    <> "O" <> (show $ length outlets)
            H.div ! HA.className "inlets"
                $ for_ inlets $ inlet fire ui
            H.div ! HA.className "outlets"
                $ for_ outlets $ outlet fire ui
        else
            H.p #! on "click" nodeClick $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isNodeSelected s.selection path
        className = "node " <> (if isSelected then "_selected" else "")
        nodeClick = fire $ Click (CSNode path)

inlet :: forall d e. (Show d) => FireInteraction e -> UI d -> R.Inlet d -> Markup e
inlet fire (UI s _) (R.Inlet { path, label, default, sources }) =
    H.div ! HA.className className $
        if isSelected then
            H.div $ do
                H.span ! HA.className "connector"
                    #! on "click" inletConnectorClick $ H.text $ connectorLabel
                H.span #! on "click" inletClick
                    $ H.text $ "<" <> show path <> ": " <> label <> "> "
                H.span $ H.text dataText
        else
            H.div $ do
                H.span ! HA.className "connector"
                    #! on "click" inletConnectorClick $ H.text $ connectorLabel
                H.span #! on "click" inletClick $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isInletSelected s.selection path
        isWaitingForConnection = fromMaybe false $ R.notInTheSameNode path <$> s.connecting
        className = "inlet" <> (if isSelected then " _selected" else " ")
            <> (if isWaitingForConnection then " _waiting" else " ")
        inletClick = fire $ Click (CSInlet path)
        inletConnectorClick = fire $ Click (CSInletConnector path)
        connectorLabel =
            if isWaitingForConnection then "(+)"
            else if length sources > 0 then "(" <> show (length sources) <> ")"
            else "(X)"
        dataText = show $ Map.lookup path s.lastInletData


outlet :: forall d e. (Show d) => FireInteraction e -> UI d -> R.Outlet d -> Markup e
outlet fire (UI s _) (R.Outlet { path, label }) =
    H.div ! HA.className className $
        if isSelected then
            H.div $ do
                H.span ! HA.className "connector"
                    #! on "click" outletConnectorClick
                    $ H.text $ connectorLabel
                H.span #! on "click" outletClick
                    $ H.text $ "<" <> show path <> ": " <> label <> "> "
                H.span $ H.text dataText
        else
            H.div $ do
                H.span ! HA.className "connector"
                    #! on "click" outletConnectorClick
                    $ H.text $ connectorLabel
                H.span #! on "click" outletClick
                    $ H.text $ "[" <> show path <> "]"
    where
        isSelected = isOutletSelected s.selection path
        isConnectingSomething = isJust s.connecting
        isCurrentlyConnecting = fromMaybe false $ ((==) path) <$> s.connecting
        className = "outlet" <> (if isSelected then " _selected" else " ")
                     <> (if isConnectingSomething then " _waiting" else " ")
                     <> (if isCurrentlyConnecting then " _connecting" else " ")
        outletClick = fire $ Click (CSOutlet path)
        outletConnectorClick = fire $ Click (CSOutletConnector path)
        connectorLabel = if isCurrentlyConnecting then "(*)" else "(+)"
        dataText = show $ Map.lookup path s.lastOutletData


prepareToFire :: forall e. Push ( dom :: DOM | e ) -> FireInteraction e
prepareToFire push interaction =
    -- eventListener $ const $ push msg
    -- _ <- log $ "<<<" <> show msg
    eventListener (\_ -> do
        log $ ">>>" <> show interaction
        _ <- push interaction
        pure unit
    )
