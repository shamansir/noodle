module Toolkit.Hydra2.Group where

import Prelude


import Data.Symbol (class IsSymbol)


import Color (Color)
import Color as Color

import Data.Mark (class Mark)


import Noodle.Id (Family) as Node
import Noodle.Id (reflectFamily) as Id


data Group
    = Array
    | Audio
    | Blend
    | Color
    | Extsource
    | Geometry
    | Modulate
    | Out
    | Source
    | Synth
    | Unknown


-- just store group inside each generated node and have a function to return them ?

-- may be return a
toGroup :: forall f. IsSymbol f => Node.Family f -> Group
toGroup family = case Id.reflectFamily family of
    "ease" -> Array
    "fast" -> Array
    "fit" -> Array
    "offset" -> Array
    "smooth" -> Array

    "fft" -> Audio
    "hide" -> Audio
    "setBins" -> Audio
    "setCutoff" -> Audio
    "setScale" -> Audio
    "setSmooth" -> Audio

    "show" -> Blend
    "add" -> Blend
    "diff" -> Blend
    "layer" -> Blend
    "mask" -> Blend
    "mult" -> Blend
    "sub" -> Blend

    "r" -> Color
    "g" -> Color
    "a" -> Color
    "b" -> Color
    "brightness" -> Color
    "color" -> Color
    "colorama" -> Color
    "contrast" -> Color
    "hue" -> Color
    "invert" -> Color
    "luma" -> Color
    "posterize" -> Color
    "saturate" -> Color
    "shift" -> Color
    "sum" -> Color
    "tresh" -> Color

    "init" -> Extsource
    "initCam" -> Extsource
    "initImage" -> Extsource
    "initScreen" -> Extsource
    "initStream" -> Extsource
    "initVideo" -> Extsource

    "kaleid" -> Geometry
    "pixelate" -> Geometry
    "repeat" -> Geometry
    "repeatX" -> Geometry
    "repeatY" -> Geometry
    "rotate" -> Geometry
    "scale" -> Geometry
    "scroll" -> Geometry
    "scrollX" -> Geometry
    "scrollY" -> Geometry

    "modulate" -> Modulate
    "modulateKaleid" -> Modulate
    "modulatePixelate" -> Modulate
    "modulateRepeat" -> Modulate
    "modulateRepeatX" -> Modulate
    "modulateRepeatY" -> Modulate
    "modulateRotate" -> Modulate
    "modulateScale" -> Modulate
    "modulateScrollX" -> Modulate
    "modulateScrollY" -> Modulate

    "out" -> Out

    "gradient" -> Source
    "noise" -> Source
    "osc" -> Source
    "prev" -> Source
    "shape" -> Source
    "solid" -> Source
    "src" -> Source
    "voronoi" -> Source

    "bpm" -> Synth
    "height" -> Synth
    "hush" -> Synth
    "mouse" -> Synth
    "pi" -> Synth
    "render" -> Synth
    "setFunction" -> Synth
    "setResolution" -> Synth
    "speed" -> Synth
    "time" -> Synth
    "update" -> Synth
    "width" -> Synth

    _ -> Unknown


instance Mark Group where
    mark = case _ of
        Array -> Color.rgb 229 102 255
        Audio -> Color.rgb 255 102 179
        Blend -> Color.rgb 102 255 177
        Color -> Color.rgb 179 255 102
        Extsource -> Color.rgb 102 179 255
        Geometry -> Color.rgb 255 230 102
        Modulate -> Color.rgb 102 255 230
        Out -> Color.rgb 199 21 133 -- 102 205 170
        Source -> Color.rgb 255 127 102
        Synth -> Color.rgb 128 102 255
        Unknown -> Color.rgb 139 199 101


    -- [ hsl 10.0 1.0 0.7 "source"
    -- , hsl 50.0 1.0 0.7 "geometry"
    -- , hsl 90.0 1.0 0.7 "color"
    -- , hsl 130.0 1.0 0.7 "blend"
    -- , hsl 170.0 1.0 0.7 "modulate"
    -- , hsl 210.0 1.0 0.7 "extsource"
    -- , hsl 250.0 1.0 0.7 "synth"
    -- , hsl 290.0 1.0 0.7 "array"
    -- , hsl 330.0 1.0 0.7 "audio"
    -- ]