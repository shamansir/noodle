module App.Style where


import App.Style.Color (Color)
import App.Style.Color as Color

import Data.Set (Set)

import Data.Vec2 (Size_, Size, Pos)

import Noodle.Node (Family) as Node


data Connector
    = Square
    | Rect
    | Circle
    | DoubleCircle


data NodeFlow
    = Vertical
    | Horizontal


data SlotDirection
    = Inside
    | Between
    | Outside


data SlotInfoVisibility
    = Always
    | OnHover
    | Never


data LinkType
    = Straight
    | Curve


data Side
    = Fixed Number
    | Stretch


data ShadowType
    = None
    | Solid { offset :: Pos }
    | Blurred { offset :: Pos, blur :: Number }



type Units =
    { cell ::
        { size :: Size
        , padding :: Size -- node padding?
        }
    , nodeBody ::
        { size :: Size_ Side
        , margin :: Size
        , strokeWidth :: Number
        , cornerRadius :: Number
        , shadowShift :: Number
        , shadowBlur :: Number
        }
    , title ::
        { size :: Size_ Side
        , padding :: Size
        }
    -- , preview
    --    :: { size :: Size }
    , slot ::
        { outerSize :: Size -- size of the rect: name/value + connector
        --, padding :: Size
        , radius :: Number
        , strokeWidth :: Number
        , inletsOffset :: Pos
        , outletsOffset :: Pos
        }
    }


type Colors =
    { background :: Color
    , patchTab :: { background :: Color, stroke :: Color }
    , nodeTab :: { background :: Color, stroke :: Color }
    , slot :: { stroke :: Color, fill :: Color, label :: Color, value :: Color  }
    , body :: { fill :: Color, shadow :: Color, stroke :: Color }
    , title :: { fill :: Color, background :: Color }
    }


type Flags =
    { hasTitle :: Boolean
    , customBody :: Boolean
    , hasRemoveButton :: Boolean
    }


type Style =
    { units :: NodeFlow -> Units
    , colors :: Colors
    , slot ::
        { connector :: Connector
        , direction :: SlotDirection
        , info :: SlotInfoVisibility
        }
    , shadow :: ShadowType
    , link :: LinkType
    , supportedFlows :: Set NodeFlow
    , font :: { size :: Number, family :: String }
    }


transparent :: Color
transparent = Color.rgba 0 0 0 0.0


white :: Color
white = Color.named "white"


black :: Color
black = Color.named "black"