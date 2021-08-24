module Halogen.Svg.Elements.Extended where


import Prelude (($))

import Halogen.HTML.Core (ElemName(ElemName))
import Halogen.HTML.Elements (Node)
import Halogen.Svg.Elements (element)

import Halogen.Svg.Indexed.Extended (SVGg') as I
import Halogen.Svg.Indexed (SVGg) as I


g' :: forall p i. Node I.SVGg' p i
g' = element $ ElemName "g"


mask :: forall p i. Node I.SVGg p i
mask = element $ ElemName "mask"