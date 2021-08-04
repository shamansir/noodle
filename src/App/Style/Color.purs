module App.Style.Color where


import Prelude (($), (*))
import Data.Int (floor)

import Halogen.Svg.Attributes as HSA


type Color = HSA.Color


rgba :: Int -> Int -> Int -> Number -> Color
rgba = HSA.RGBA


rgba' :: Number -> Number -> Number -> Number -> Color
rgba' r g b = rgba (floor $ r * 255.0) (floor $ g * 255.0) (floor $ b * 255.0)


rgb :: Int -> Int -> Int -> Color
rgb = HSA.RGB


rgb' :: Number -> Number -> Number -> Color
rgb' r g b = rgb (floor $ r * 255.0) (floor $ g * 255.0) (floor $ b * 255.0)


named :: String -> Color
named = HSA.Named