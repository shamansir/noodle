module Render
    ( render
    , Event(..)
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.Event as DOM
import DOM.Event.EventTarget (EventListener, eventListener)
import Data.Array (length)
import Data.Foldable (class Foldable, for_, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\), type (/\))
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM
import DOM.Node.Types (Element)


newtype UIState d =
    UIState
        { dragging :: Maybe R.NodePath
        , connecting :: Maybe R.OutletPath
        , updates :: SC.Channel (UI d)
        }


initState :: forall d. SC.Channel (UI d) -> UIState d
initState chan =
    UIState
        { dragging : Nothing
        , connecting : Nothing
        , updates : chan
        }


data Event
    = Start
    | Connect String String
    | Drag Int Int

type Updates d e = R.Updates (UIState d) d (dom :: DOM | e )


type UI d = R.UI (UIState d) d


type Markup e = H.Markup (EventListener ( channel :: SC.CHANNEL, dom :: DOM | e ))


type DomRenderer d e = R.Renderer (UIState d) d e


renderer :: forall d e. Element -> DomRenderer d e
renderer elm =
    { init : initState
    , render : render elm
    }


render :: forall d e. Element -> UI d -> Eff ( channel :: SC.CHANNEL, dom :: DOM | e ) Unit
render target ui = do
    ToDOM.patch target (network ui)
    --ToDOM.render target (network ui ?what)
    -- let state = initState
    -- let ui = state /\ nw
    -- channel <- SC.channel nw
    -- let signal = SC.subscribe channel
    -- let sender = (\network -> do SC.send channel network)
    -- S.runSignal (signal S.~> (\network -> render target $ network network sender))


network :: forall d e. UI d -> Markup e
network ui@(R.UI _ (R.Network patches)) =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches (\p -> patch ui p)


patch :: forall d e. UI d -> R.Patch d -> Markup e
patch ui (R.Patch patchId label nodes links) =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label <> " " <> show patchId
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes (\n -> node ui n)


node :: forall d e. UI d -> R.Node d -> Markup e
node ui (R.Node path name inlets outlets _) =
    H.div $ do
        H.p $ H.text $ "Node: " <> name <> " " <> show path
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets (\i -> inlet ui i)
        for_ outlets (\o -> outlet ui o)


inlet :: forall d e. UI d -> R.Inlet d -> Markup e
inlet ui@(R.UI s _) (R.Inlet path label _) =
    H.div $ do
        H.p #! on "click" (eventListener $ transformWith updates evt ui) $ H.text $ "Inlet: " <> label <> " " <> show path
    where
        UIState { updates } = s
        evt = Connect "foo" "bar"


outlet :: forall d e. UI d -> R.Outlet d -> Markup e
outlet ui (R.Outlet path label _) =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label <> " " <> show path


--transformWith :: Event ->
transformWith l evt ui = (\_ -> l $ update evt ui)


-- TODO: Add UIState in the loop
update :: forall d e. Event -> UI d -> UI d
update evt (R.UI state network) =
    R.UI state network
    -- case evt of
    --     Start -> H.p $ H.text $ "Start"
    --     Connect s1 s2 -> H.p $ H.text $ "Connect: " <> s1 <> s2
    --     Drag i1 i2 -> H.p $ H.text $ "Drag: " <> show i1 <> show i2


-- TODO: render :: forall d e. Network d -> Eff ( dom :: DOM | e ) Unit
