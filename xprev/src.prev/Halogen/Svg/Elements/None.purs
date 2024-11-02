module Halogen.Svg.Elements.None
    ( none ) where

import Halogen.Svg.Elements as HS
import Halogen.HTML.Core (HTML)


none :: forall p i. HTML p i
none = HS.g [] []