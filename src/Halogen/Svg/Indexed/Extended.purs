module Halogen.Svg.Indexed.Extended where


import Halogen.Svg.Indexed (SVGg)


type MaskAttributes r = ("mask" :: String | r)


type SVGg' = MaskAttributes SVGg