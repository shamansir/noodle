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
import DOM.Node.Types (Element)


newtype UIState =
    UIState
        { dragging :: Maybe R.NodePath
        , connecting :: Maybe R.OutletPath
        }


initState :: UIState
initState =
    UIState
        { dragging : Nothing
        , connecting : Nothing
        }


data Event
    = Start
    | Connect String String
    | Drag Int Int


type Updates d e = R.Network d -> Eff ( channel :: SC.CHANNEL | e ) Unit

type Markup e = H.Markup (EventListener ( channel :: SC.CHANNEL | e ))

type UI d = UIState /\ R.Network d


render :: forall d e. Element -> R.Network d -> Markup e
render target nw =
    H.div $ H.text "Test"
    -- let state = initState
    -- let ui = state /\ nw
    -- channel <- SC.channel nw
    -- let signal = SC.subscribe channel
    -- let sender = (\network -> do SC.send channel network)
    -- S.runSignal (signal S.~> (\network -> render target $ network network sender))


network :: forall d e. UI d -> Updates d e -> Markup e
network ui@( _ /\ nw@(R.Network patches) ) l =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches (\p -> patch ui p l)


patch :: forall d e. UI d -> R.Patch d -> Updates d e -> Markup e
patch ui (R.Patch patchId label nodes links) l =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label <> " " <> show patchId
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes (\n -> node ui n l)


node :: forall d e. UI d -> R.Node d -> Updates d e -> Markup e
node ui (R.Node path name inlets outlets _) l =
    H.div $ do
        H.p $ H.text $ "Node: " <> name <> " " <> show path
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets (\i -> inlet ui i l)
        for_ outlets (\o -> outlet ui o l)


inlet :: forall d e. UI d -> R.Inlet d -> Updates d e -> Markup e
inlet ui (R.Inlet path label _) onUpdate =
    H.div $ do
        H.p #! on "click" (eventListener $ const $ onUpdate $ snd $ update evt ui) $ H.text $ "Inlet: " <> label <> " " <> show path
    where
        evt = Connect "foo" "bar"


outlet :: forall d e. UI d -> R.Outlet d -> Updates d e -> Markup e
outlet ui (R.Outlet path label _) _ =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label <> " " <> show path


-- TODO: Add UIState in the loop
update :: forall d e. Event -> Tuple UIState (R.Network d) -> Tuple UIState (R.Network d)
update evt (state /\ network) =
    Tuple state network
    -- case evt of
    --     Start -> H.p $ H.text $ "Start"
    --     Connect s1 s2 -> H.p $ H.text $ "Connect: " <> s1 <> s2
    --     Drag i1 i2 -> H.p $ H.text $ "Drag: " <> show i1 <> show i2


-- TODO: render :: forall d e. Network d -> Eff ( dom :: DOM | e ) Unit
