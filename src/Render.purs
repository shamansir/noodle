module Render
    ( network
    ) where

import Prelude

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM (render)
import Data.Foldable (for_)
import Data.Array

import Rpd as R

network :: forall e d. R.Network d -> H.Markup e
network (R.Network patches) =
  H.div $ do
    H.p $ H.text "Network"
    H.p $ H.text $ "Has " <> (show $ length patches) <> " Patches"
    for_ patches patch


patch :: forall e d. R.Patch d -> H.Markup e
patch (R.Patch label nodes links) =
    H.text label
