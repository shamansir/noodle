module Halogen.Svg.Attributes.Extended where


import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Attributes (attr)
import Web.HTML.Common (AttrName(..))

--import Halogen.Svg.Indexed as I


mask :: forall s i. String -> IProp (mask :: String | s) i
mask = attr (AttrName "mask")