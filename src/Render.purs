module Render
    ( network
    , Event
    , Listener
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

import Rpd as R


data Event
    = Connect String String
    | Drag Int Int


type Listener e = Event -> Eff ( dom :: DOM | e ) Unit


type DomMarkup e = H.Markup (EventListener ( dom :: DOM | e ))

-- type Listener e = DOM.Event -> Eff ( dom :: DOM | e ) Unit


-- listenerHtml :: forall e. Element -> Event -> Eff ( dom :: DOM | e ) Unit
-- listenerHtml trg evt =
--   render trg (H.div $ H.text "Got the event")

--     render element (H.div #! on "click" (eventListener $ listenerHtml element) $ H.text "Bar")


network :: forall e d. R.Network d -> Listener e -> DomMarkup e
network (R.Network patches) l =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches (\p -> patch p l)


patch :: forall e d. R.Patch d -> Listener e -> DomMarkup e
patch (R.Patch label nodes links) l =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes (\n -> node n l)


node :: forall e d. R.Node d -> Listener e -> DomMarkup e
node (R.Node name inlets outlets _) l =
    H.div $ do
        H.p $ H.text $ "Node: " <> name
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets (\i -> inlet i l)
        for_ outlets (\o -> outlet o l)


inlet :: forall e d. R.Inlet d -> Listener e -> DomMarkup e
inlet (R.Inlet label _) onEvent =
    H.div $ do
        H.p #! on "click" (eventListener $ const $ onEvent evt) $ H.text $ "Inlet: " <> label
    where
        evt = Connect "foo" "bar"


outlet :: forall e d. R.Outlet d -> Listener e -> DomMarkup e
outlet (R.Outlet label _) _ =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label


update :: forall e d. Event -> R.Network d -> DomMarkup e
update evt _ =
    case evt of
        Connect s1 s2 -> H.p $ H.text $ "Connect: " <> s1 <> s2
        Drag i1 i2 -> H.p $ H.text $ "Drag: " <> show i1 <> show i2
