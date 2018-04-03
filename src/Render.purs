module Render
    ( renderer
    , Event(..)
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Map as Map
import Data.Map (Map(..))
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Text.Smolder.HTML as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM


newtype UIState d =
    UIState
        { dragging :: Maybe R.NodePath
        , connecting :: Maybe R.OutletPath
        , curDataMsg :: Maybe (R.DataMsg d)
        , lastInletData :: Map R.InletPath d
        , lastOutletData :: Map R.OutletPath d
        }


data Event d
    = Start
    | Skip
    | Connect String String
    | Drag Int Int
    | Data (R.DataMsg d)

-- type Updates d e = R.Updates (UIState d) d (dom :: DOM | e )


data UI d = UI (UIState d) (R.Network d)


type UIChannel d = SC.Channel (Event d)


type Listener e = EventListener ( channel :: SC.CHANNEL, dom :: DOM | e )


type Markup e = H.Markup (Listener e)


type DomRenderer d e = R.Renderer d ( dom :: DOM | e )


initState :: forall d. UIState d
initState =
    UIState
        { dragging : Nothing
        , connecting : Nothing
        , curDataMsg : Nothing
        , lastInletData : Map.empty
        , lastOutletData : Map.empty
        }


renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer target maybeDataSignal nw = do
    evtChannel <- SC.channel Start
    let evtSignal = SC.subscribe evtChannel
    let uiSignal = S.foldp update (UI initState nw) evtSignal
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
    ToDOM.patch target $ network ui ch


network :: forall d e. (Show d) => UI d -> UIChannel d -> Markup e
network ui@(UI (UIState s) (R.Network patches)) ch =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches (\p -> patch ui ch p)


patch :: forall d e. (Show d) => UI d -> UIChannel d -> R.Patch d -> Markup e
patch ui ch (R.Patch patchId label nodes links) =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label <> " " <> show patchId
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes (\n -> node ui ch n)


node :: forall d e. (Show d) => UI d -> UIChannel d -> R.Node d -> Markup e
node ui ch (R.Node path name inlets outlets _) =
    H.div $ do
        H.p $ H.text $ "Node: " <> name <> " " <> show path
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets (\i -> inlet ui ch i)
        for_ outlets (\o -> outlet ui ch o)


inlet :: forall d e. (Show d) => UI d -> UIChannel d -> R.Inlet d -> Markup e
inlet ui@(UI (UIState s) _) ch (R.Inlet path label maybeDefault _) =
    H.div $ do
        H.p #! on "click" (sendEvt ch evt) $ H.text $ "Inlet: " <> label <> " " <> show path
        H.p $ H.text $ dataText s.curDataMsg
    where
        evt = Connect "foo" "bar"
        dataText (Just dataMsg) =
            ---s.dataI >>>
            maybe "No data" (\val -> "Has data: " <> show val) $ R.ifFromInlet path dataMsg
        dataText Nothing = "No data"



outlet :: forall d e. UI d -> UIChannel d -> R.Outlet d -> Markup e
outlet ui ch (R.Outlet path label _) =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label <> " " <> show path


sendEvt :: forall d e. UIChannel d -> Event d -> Listener e
sendEvt ch evt =
    eventListener $ const $ SC.send ch evt

--transformWith :: Event ->
-- transformWith ch evt ui = (\_ -> SC.send ch $ update evt ui)


update :: forall d e. Event d -> UI d -> UI d
update (Data dataMsg) (UI (UIState state) network) =
    UI
        (UIState (state { curDataMsg = Just dataMsg }))
        network
update _ ui = ui


-- TODO: render :: forall d e. Network d -> Eff ( dom :: DOM | e ) Unit
