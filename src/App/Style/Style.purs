module App.Style where


import Prelude (class Show)

import App.Style.Color (Color)
import App.Style.Color as Color

import Data.Set (Set)
import Data.Set.Ordered (OSet)

import Data.Vec2 (Size_, Size, Pos, (<+>))
import Data.Tuple.Nested ((/\), type (/\))

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


data NodePart
    = Title
    | OnlyInlets
    | OnlyOutlets
    | UserBody Number
    | InletsAndOutlets
    | UserBodyBetweenSlots
    | UserBodyBetweenSlotsMin Number


data TitleMode
    = OutsideBody
    | InsideBody


type Order = OSet NodePart


data ShadowType
    = None
    | Solid { offset :: Pos }
    | Blurred { offset :: Pos, blur :: Number }


type Units =
    { cell ::
        { size :: Size
        }
    , body ::
        { size :: Number
        , margin :: Size
        , strokeWidth :: Number
        , cornerRadius :: Number
        }
    , title ::
        { size :: Number
        , padding :: Size
        }
    -- , preview
    --    :: { size :: Size }
    , slot ::
        { area :: Size -- size of the rect: name/value + connector
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
    { units :: NodeFlow -> Units -- join Units and Color into titleStyle - SlotStyle etc.?
    , order :: Order
    , colors :: Colors
    , slot ::
        { connector :: Connector
        , direction :: SlotDirection
        , info :: SlotInfoVisibility
        }
    , title :: TitleMode
    , shadow :: ShadowType
    , link :: LinkType
    , supportedFlows :: Set NodeFlow
    , font :: { size :: Number, family :: Array String }
    }


defaultFlags :: Flags
defaultFlags =
    { hasTitle : true
    , customBody : false
    , hasRemoveButton : true
    }


{- findBodySize :: NodeFlow -> (CalculateSide -> Number) -> BodySize -> Size
findBodySize Horizontal _ (h /\ Fixed w) = w <+> h
findBodySize Horizontal f (h /\ StretchByMax) = f StretchByMax <+> h
findBodySize Horizontal f (h /\ StretchBySum) = f StretchBySum <+> h -}



transparent :: Color
transparent = Color.rgba 0 0 0 0.0


white :: Color
white = Color.named "white"


black :: Color
black = Color.named "black"


instance showSlotDirection :: Show SlotDirection where
    show Inside = "inside"
    show Outside = "outside"
    show Between = "between"