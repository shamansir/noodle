module Noodle.Ui.Palette.Set.Hydra where

import Prelude

import Data.Int (toNumber)
import Data.Array (range)

import Noodle.Ui.Palette.Item (Item, hsl)


step :: Int -> String -> Item
step factor = hsl (10.0 + (toNumber factor * 40.0)) 1.0 0.7


{-
source = hsl 10.0 1.0 0.7 "source" :: Item
geometry = hsl 50.0 1.0 0.7 "geometry" :: Item
color = hsl 90.0 1.0 0.7 "color" :: Item
blend = hsl 130.0 1.0 0.7 "blend" :: Item
modulate = hsl 170.0 1.0 0.7 "modulate" :: Item
extsource = hsl 210.0 1.0 0.7 "extsource" :: Item
synth = hsl 250.0 1.0 0.7 "synth" :: Item
array = hsl 290.0 1.0 0.7 "array" :: Item
audio = hsl 330.0 1.0 0.7 "audio" :: Item
-}


source = step 0 "source" :: Item
geometry = step 1 "geometry" :: Item
color = step 2 "color" :: Item
blend = step 3 "blend" :: Item
modulate = step 4 "modulate" :: Item
extsource = step 5 "extsource" :: Item
synth = step 6 "synth" :: Item
array = step 7 "array" :: Item
audio = step 8 "audio" :: Item


hydraFns :: Array Item
hydraFns =
    [ source
    , geometry
    , color
    , blend
    , modulate
    , extsource
    , synth
    , array
    , audio
    ]


steps :: Int -> (Int -> String) -> Array Item
steps count namef =
    range 0 count <#> \idx -> step idx $ namef idx