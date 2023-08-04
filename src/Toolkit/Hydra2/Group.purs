module Toolkit.Hydra2.Group where

import Prelude


import Data.Symbol (class IsSymbol)


import Color as Color

import Data.Mark (class Mark)


import Noodle.Id (Family) as Node
import Noodle.Id (FamilyR, familyR, reflectFamilyR) as Id


data Group
    = Array
    | Audio
    | Blend
    | Color
    | ExternalSources
    | Feed
    | Geometry
    | Modulate
    | Out
    | Source
    | Synth
    | Unknown


-- just store group inside each generated node and have a function to return them ?

-- may be return a
toGroup :: forall f. IsSymbol f => Node.Family f -> Group
toGroup = toGroupR <<< Id.familyR


toGroupR :: Id.FamilyR -> Group
toGroupR family = case Id.reflectFamilyR family of
    "number" -> Feed
    "pi" -> Feed

    "noise" -> Source
    "voronoi" -> Source
    "osc" -> Source
    "shape" -> Source
    "gradient" -> Source
    "src" -> Source
    "solid" -> Source
    "prev" -> Source

    "rotate" -> Geometry
    "scale" -> Geometry
    "pixelate" -> Geometry
    "repeat" -> Geometry
    "repeatX" -> Geometry
    "repeatY" -> Geometry
    "kaleid" -> Geometry
    "scroll" -> Geometry
    "scrollX" -> Geometry
    "scrollY" -> Geometry

    "posterize" -> Color
    "shift" -> Color
    "invert" -> Color
    "contrast" -> Color
    "brightness" -> Color
    "luma" -> Color
    "thresh" -> Color
    "color" -> Color
    "saturate" -> Color
    "hue" -> Color
    "colorama" -> Color
    "sum" -> Color
    "r" -> Color
    "g" -> Color
    "b" -> Color
    "a" -> Color

    "add" -> Blend
    "sub" -> Blend
    "layer" -> Blend
    "blend" -> Blend
    "mult" -> Blend
    "diff" -> Blend
    "mask" -> Blend

    "modulateRepeat" -> Modulate
    "modulateRepeatX" -> Modulate
    "modulateRepeatY" -> Modulate
    "modulateKaleid" -> Modulate
    "modulateScrollX" -> Modulate
    "modulateScrollY" -> Modulate
    "modulate" -> Modulate
    "modulateScale" -> Modulate
    "modulatePixelate" -> Modulate
    "modulateRotate" -> Modulate
    "modulateHue" -> Modulate

    "initCam" -> ExternalSources
    "initImage" -> ExternalSources
    "initVideo" -> ExternalSources
    "init" -> ExternalSources
    "initStream" -> ExternalSources
    "initScreen" -> ExternalSources

    "render" -> Synth
    "update" -> Synth
    "setResolution" -> Synth
    "hush" -> Synth
    "setFunction" -> Synth
    "speed" -> Synth
    "bpm" -> Synth
    "width" -> Synth
    "height" -> Synth
    "time" -> Synth
    "mouse" -> Synth

    "fft" -> Audio
    "setSmooth" -> Audio
    "setCutoff" -> Audio
    "setBins" -> Audio
    "setScale" -> Audio
    "hide" -> Audio
    "show" -> Audio

    "setScale" -> Audio

    "fast" -> Array
    "smooth" -> Array
    "ease" -> Array
    "offset" -> Array
    "fit" -> Array

    "out" -> Out

    _ -> Unknown


instance Mark Group where
    mark = case _ of
        Array -> Color.rgb 229 102 255
        Audio -> Color.rgb 255 102 179
        Blend -> Color.rgb 102 255 177
        Color -> Color.rgb 179 255 102
        ExternalSources -> Color.rgb 102 179 255
        Geometry -> Color.rgb 255 230 102
        Feed -> Color.rgb 255 255 255
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