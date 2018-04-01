module Render
    ( renderer
    , Event(..)
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.Event as DOM
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Foldable (class Foldable, for_, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, forWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM


newtype UIState d =
    UIState
        { dragging :: Maybe R.NodePath
        , connecting :: Maybe R.OutletPath
        , dataI :: Maybe (R.InletPath /\ d)
        }


data Event d
    = Start
    | Skip
    | Connect String String
    | Drag Int Int
    | DataI (R.InletPath /\ d)

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
        , dataI : Nothing
        }


renderer :: forall d e. (Show d) => Element -> DomRenderer d e
renderer target nw = do
    evtChannel <- SC.channel Start
    let evtSignal = SC.subscribe evtChannel
    let uiSignal = S.foldp update (UI initState nw) evtSignal
    let dataSignal = getDataSignal nw S.~> (\dataEvt -> SC.send evtChannel dataEvt)
    S.runSignal dataSignal -- FIXME: should be performed by Rpd.run
    -- TODO: run data signal
    -- TODO: remove
    -- let sendToInlet = R.inletPath 0 0 0
    -- let dataEverySec = ST.every 1000.0 S.~> (\t -> SC.send evtChannel (DataI $ sendToInlet /\ t))
    -- S.runSignal dataEverySec
    -- TODO: /remove
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


getDataSignal :: forall d. R.Network d -> S.Signal (Event d)
getDataSignal nw =
    maybe
        (S.constant Skip)
        (\s -> s S.~> DataI)
        (R.subscribeAllData nw)


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
        H.p $ H.text $ dataText
    where
        evt = Connect "foo" "bar"
        dataText =
            ---s.dataI >>>
            maybe "No data"
                (\(path' /\ val) ->
                    if (path' == path) then
                        "Has Data: " <> show val
                    else "No Data")
                s.dataI


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
update evt (UI state@(UIState st) network) =
    case evt of
        DataI pathAndVal ->
            UI
                (UIState (st { dataI = Just pathAndVal }))
                network
        _ -> UI state network


-- TODO: render :: forall d e. Network d -> Eff ( dom :: DOM | e ) Unit
