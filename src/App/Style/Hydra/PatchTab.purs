module App.Style.Hydra.PatchTab where

import Halogen.Svg.Attributes (Color(..))

import App.Style (PatchTabStyle)

patchTab :: PatchTabStyle
patchTab =
    { background : RGB 170 170 170
    , stroke : RGBA 0 0 0 0.7
    }