module Prev.Web.App.Style.Hydra.Title where

import Color as C

import Web.App.Style (TitleStyle, TitleMode(..))
import Data.Vec2 ((<+>))


title :: TitleStyle
title =
    { mode : OutsideBody
    , background : C.rgba 0 0 0 0.0
    , fill : C.rgb 255 255 255
    , size : 20.0
    , padding : 3.0 <+> 10.0
    }