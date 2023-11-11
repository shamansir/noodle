module Prev.Web.App.Style.Quartz.Body where

import Color as C

import Web.App.Style (BodyStyle, ShadowType(..))
import Data.Vec2 ((<+>))


body :: BodyStyle
body =
    { shadow : Solid { offset : 5.0 <+> 5.0 }
    , size : 100.0
    , margin : 50.0 <+> 10.0
    , fill : C.rgb 80 96 126
    , stroke : C.rgba 255 255 255 0.4
    , strokeWidth : 1.0
    , cornerRadius : 0.0
    }