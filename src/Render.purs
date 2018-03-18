module Render
    ( network
    , Event(..)
    , Updates
    , update
    ) where

import Prelude

import DOM (DOM)
import DOM.Event.Event as DOM
import DOM.Event.EventTarget (EventListener, eventListener)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (for_)
import Data.Array (length)

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.Markup ((#!), on)
-- import Text.Smolder.Renderer.DOM (render)

import Signal as S
import Signal.Channel as SC
import Signal.Time as ST

import Rpd as R


data Event
    = Start
    | Connect String String
    | Drag Int Int


type Updates d e = R.Network d -> Eff ( channel :: SC.CHANNEL | e ) Unit

type Markup e = H.Markup (EventListener ( channel :: SC.CHANNEL | e ))

-- type Listener e = DOM.Event -> Eff ( dom :: DOM | e ) Unit


-- listenerHtml :: forall e. Element -> Event -> Eff ( dom :: DOM | e ) Unit
-- listenerHtml trg evt =
--   render trg (H.div $ H.text "Got the event")

--     render element (H.div #! on "click" (eventListener $ listenerHtml element) $ H.text "Bar")


network :: forall d e. R.Network d -> Updates d e -> Markup e -- (S.Signal Event, Markup e)
network nw@(R.Network patches) l =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches (\p -> patch nw p l)


patch :: forall d e. R.Network d ->R.Patch d -> Updates d e -> Markup e
patch nw (R.Patch label nodes links) l =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes (\n -> node nw n l)


node :: forall d e. R.Network d ->R.Node d -> Updates d e -> Markup e
node nw (R.Node name inlets outlets _) l =
    H.div $ do
        H.p $ H.text $ "Node: " <> name
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets (\i -> inlet nw i l)
        for_ outlets (\o -> outlet nw o l)


inlet :: forall d e. R.Network d -> R.Inlet d -> Updates d e -> Markup e
inlet nw (R.Inlet label _) onUpdate =
    H.div $ do
        H.p #! on "click" (eventListener $ const $ onUpdate $ update evt nw) $ H.text $ "Inlet: " <> label
    where
        evt = Connect "foo" "bar"


outlet :: forall d e. R.Network d -> R.Outlet d -> Updates d e -> Markup e
outlet nw (R.Outlet label _) _ =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label


update :: forall d e. Event -> R.Network d -> R.Network d
update evt network =
    network
    -- case evt of
    --     Start -> H.p $ H.text $ "Start"
    --     Connect s1 s2 -> H.p $ H.text $ "Connect: " <> s1 <> s2
    --     Drag i1 i2 -> H.p $ H.text $ "Drag: " <> show i1 <> show i2
