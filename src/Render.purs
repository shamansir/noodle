module Render
    ( network
    ) where

import Prelude

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
-- import Text.Smolder.Renderer.DOM (render)
import Data.Foldable (for_)
import Data.Array (length)

import Rpd as R


network :: forall e d. R.Network d -> H.Markup e
network (R.Network patches) =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches patch


patch :: forall e d. R.Patch d -> H.Markup e
patch (R.Patch label nodes links) =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes node


node :: forall e d. R.Node d -> H.Markup e
node (R.Node name inlets outlets _) =
    H.div $ do
        H.p $ H.text $ "Node: " <> name
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets inlet
        for_ outlets outlet


inlet :: forall e d. R.Inlet d -> H.Markup e
inlet (R.Inlet label _) =
    H.div $ do
        H.p $ H.text $ "Inlet: " <> label


outlet :: forall e d. R.Outlet d -> H.Markup e
outlet (R.Outlet label _) =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label
