module App.Style where


import Halogen.Svg.Attributes (Color(..))


data Connector
    = Square
    | Circle


data NodeFlow
    = Vertical
    | Horizontal


type Units =
    { cellWidth :: Number
    , cellHeight :: Number
    , nodeBodyWidth :: Number
    , nodeBodyHeight :: Number
    , namePlateHeight :: Number
    , namePlateWidth :: Number
    , slotOuterWidth :: Number
    , slotOuterHeight :: Number
    , slotRadius :: Number
    , slotStrokeWidth :: Number
    , bodyStrokeWidth :: Number
    , bodyCornerRadius :: Number
    , bodyShadowShift :: Number
    }


type Colors =
    { background :: Color
    , tabBackground :: Color
    , slotStroke :: Color
    , slotFill :: Color
    , bodyFill :: Color
    , bodyShadow :: Color
    , bodyStroke :: Color
    , nodeName :: Color
    , namePlateBg :: Color
    }


type Style =
    { units :: NodeFlow -> Units
    , colors :: Colors
    , connector :: Connector
    }


transparent :: Color
transparent = RGBA 0 0 0 0.0