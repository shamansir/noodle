module Render.Html where

import Prelude


import Control.Monad.Eff (Eff)
import Control.Alternative ((<|>))
import Data.Map (Map(..))
import Data.Foldable (for_)
import Data.Array (length)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Signal as S
import Signal.Channel as SC
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup ((#!), (!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM

import Rpd as R
import Render


type Listener e = EventListener ( channel :: SC.CHANNEL, dom :: DOM | e )


type Markup e = H.Markup (Listener e)


type DomRenderer d e = R.Renderer d ( dom :: DOM | e )


renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer target maybeDataSignal nw = do
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
        H.p $ H.text $ "Network: " <> friendlyLength "Patch" "Patches" patches
        H.div ! HA.className "patches" $
            for_ patches (\p -> patch ui ch p)


patch :: forall d e. (Show d) => UI d -> UIChannel d -> R.Patch d -> Markup e
patch ui ch (R.Patch patchId label nodes links) =
    H.div $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show patchId <> ": " <> label <> ">"
            H.p $ H.text $ friendlyLength "Node" "Nodes" nodes
            H.p $ H.text $ friendlyLength "Link" "Links" links
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
    H.div $
        if isSelected then do
            H.p #! clickHandler $ H.text $ "<" <> show nodePath <> ": " <> name <> ">"
            H.p $ H.text $ friendlyLength "Inlet" "Inlets" inlets
            H.p $ H.text $ friendlyLength "Outlet" "Outlets" outlets
            for_ inlets (\i -> inlet ui ch i)
            for_ outlets (\o -> outlet ui ch o)
        else
            H.p #! clickHandler $ H.text $ "[" <> show nodePath <> "]"
    where
        isSelected = isNodeSelected (getSelection ui) nodePath
        className = "node " <> (if isSelected then "_selected" else "")
        maybeSelect = sendEvt ch $ Select (SNode nodePath)
        clickHandler = on "click" maybeSelect


inlet :: forall d e. (Show d) => UI d -> UIChannel d -> R.Inlet d -> Markup e
inlet (UI (UIState s) _) ch (R.Inlet path label maybeDefault _) =
    H.div $ do
        H.p #! on "click" maybeConnect $ H.text $ "Inlet: " <> label <> " " <> show path
        H.p $ H.text $ dataText s.curDataMsg
    where
        maybeConnect = sendEvt ch $ case s.connecting of
            Just outletPath -> ConnectTo path
            Nothing -> Skip
        dataText dataMsg = maybe "No data" (\v -> "Has data: " <> show v) $ do
            dataMsg' <- dataMsg
            pure $ R.ifFromInlet path dataMsg' <|> Map.lookup path s.lastInletData



outlet :: forall d e. (Show d) => UI d -> UIChannel d -> R.Outlet d -> Markup e
outlet (UI (UIState s) _) ch (R.Outlet path label _) =
    H.div $ do
        H.p #! on "click" tryConnecting $ H.text $ "Outlet: " <> label <> " " <> show path
        H.p $ H.text $ dataText s.curDataMsg
    where
        tryConnecting = sendEvt ch $ case s.connecting of
            Just _ -> Skip
            Nothing -> ConnectFrom path
        dataText dataMsg = maybe "No data" (\v -> "Has data: " <> show v) $ do
            dataMsg' <- dataMsg
            pure $ R.ifFromOutlet path dataMsg' <|> Map.lookup path s.lastOutletData



sendEvt :: forall d e. UIChannel d -> Event d -> Listener e
sendEvt ch evt =
    eventListener $ const $ SC.send ch evt



friendlyLength :: forall a. String -> String -> Array a -> String
friendlyLength s m xs =
    case (length xs) of
        0 -> "No " <> m
        1 -> "One " <> s
        n -> show n <> " " <> m
