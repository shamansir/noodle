module Render
    ( network
    ) where

import Prelude

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM (render)
import Data.Foldable (for_)

import Rpd as R

network :: forall e d. R.Network d -> H.Markup e
network (R.Network patches) =
  H.div $
    for_ patches patch


patch :: forall e d. R.Patch d -> H.Markup e
patch (R.Patch label nodes links) =
    H.text label
