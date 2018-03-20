module Render
    ( network
    , Event(..)
    , Updates
    , update
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
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Markup as H


data Event
    = Start
    | Connect String String
    | Drag Int Int


type Updates d e = R.Network d -> Eff ( channel :: SC.CHANNEL | e ) Unit

type Markup e = H.Markup (EventListener ( channel :: SC.CHANNEL | e ))


network :: forall d e. R.Network d -> Updates d e -> Markup e
network nw@(R.Network patches) l =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        forIndexed_ patches (\idx p -> patch nw p l)


patch :: forall d e. R.Network d -> R.Patch d -> Updates d e -> Markup e
patch nw (R.Patch patchId label nodes links) l =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        forIndexed_ nodes (\nodeIdx n ->
            node nw n l)


node :: forall d e. R.Network d -> R.Node d -> Updates d e -> Markup e
node nw (R.Node path name inlets outlets _) l =
    H.div $ do
        H.p $ H.text $ "Node: " <> name
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        forIndexed_ inlets (\idx i ->
            inlet nw i l)
        forIndexed_ outlets (\idx o ->
            outlet nw o l)


inlet :: forall d e. R.Network d -> R.Inlet d -> Updates d e -> Markup e
inlet nw (R.Inlet path label _) onUpdate =
    H.div $ do
        H.p #! on "click" (eventListener $ const $ onUpdate $ update evt nw) $ H.text $ "Inlet: " <> label
    where
        evt = Connect "foo" "bar"


outlet :: forall d e. R.Network d -> R.Outlet d -> Updates d e -> Markup e
outlet nw (R.Outlet path label _) _ =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label


update :: forall d e. Event -> R.Network d -> R.Network d
update evt network =
    network
    -- case evt of
    --     Start -> H.p $ H.text $ "Start"
    --     Connect s1 s2 -> H.p $ H.text $ "Connect: " <> s1 <> s2
    --     Drag i1 i2 -> H.p $ H.text $ "Drag: " <> show i1 <> show i2


forIndexed_
    :: forall i f a m
    . FoldableWithIndex i f => Applicative m
    => f a -> (i -> a -> m Unit) -> m Unit
forIndexed_ =
    forWithIndex_
