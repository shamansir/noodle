module App.Style.Hydra.PatchTab where

import Color as C

import App.Style (PatchTabStyle)

patchTab :: PatchTabStyle
patchTab =
    { background : C.rgb 170 170 170
    , stroke : C.rgba 0 0 0 0.7
    }