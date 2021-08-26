module Color.Extra where

import Color (Color)
import Color as C

import Halogen.Svg.Attributes as HSA


toSvg :: Color -> HSA.Color
toSvg color =
    case C.toRGBA color of
        { r, g, b, a } -> HSA.RGBA r g b a



transparent :: Color
transparent = C.rgba 0 0 0 0.0