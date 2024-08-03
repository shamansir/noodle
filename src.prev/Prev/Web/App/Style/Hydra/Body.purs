module Prev.Web.App.Style.Hydra.Body where

import Color as C

import Prev.Web.App.Style (BodyStyle, ShadowType(..))
import Data.Vec2 ((<+>))


body :: BodyStyle
body =
    { shadow : Solid { offset : 5.0 <+> 5.0 }
    , size : 110.0
    , margin : 50.0 <+> 10.0
    , fill : C.rgba 0 0 0 0.9
    , stroke : C.rgb 0 0 0
    , strokeWidth : 1.0
    , cornerRadius : 5.0
    }