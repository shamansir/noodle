module Web.App.Style where


import Prelude (class Show)

import Web.App.Style.Order as O

import Color (Color)
import Color as C

import Data.Set (Set)

import Data.Vec2 (Size_, Size, Pos, (<+>))
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Node (Family) as Node


type Flags =
    { hasTitle :: Boolean
    , hasRibbon :: Boolean
    , controlArea :: Boolean
    , hasRemoveButton :: Boolean
    }


type Radius = Number


type Shift = Number


data Connector
    = Square Number
    | Rect Size
    | Circle Radius
    | DoubleCircle Radius Radius


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
    | Pipe


data NodePart
    = Title
    | Ribbon
    | OnlyInlets
    | OnlyOutlets
    | UserBody Number
    | InletsAndOutlets
    | UserBodyBetweenSlots
    | UserBodyBetweenSlotsMin Number


data TitleMode
    = OutsideBody
    | InsideBody


data ShadowType
    = None
    | Solid { offset :: Pos }
    | Blurred { offset :: Pos, blur :: Number }


type CellStyle =
    { size :: Size
    }


type BackgroundStyle =
    { fill :: Color
    }


type PatchTabStyle =
    { background :: Color
    , stroke :: Color
    }


type NodeTabStyle =
    { background :: Color
    , stroke :: Color
    }


type SlotStyle =
    { stroke :: Color
    , fill :: Color
    , label :: { color :: Color, maxWidth :: Number }
    , value :: { color :: Color, maxWidth :: Number }
    , dimWhenNoLinks :: Boolean
    , connector :: Connector
    , offset :: Pos
    , direction :: SlotDirection
    , info :: SlotInfoVisibility
    , strokeWidth :: Number
    }


type TitleStyle =
    { mode :: TitleMode
    , fill :: Color
    , background :: Color
    , size :: Number
    , padding :: Size
    }


type BodyStyle =
    { shadow :: ShadowType
    , size :: Number
    , margin :: Size
    , fill :: Color
    , stroke :: Color
    , strokeWidth :: Number
    , cornerRadius :: Number
    }


type LinkStyle =
    { type :: LinkType
    }


type Style =
    { slot :: SlotStyle
    , bg :: BackgroundStyle
    , body :: BodyStyle
    , title :: TitleStyle
    , link :: LinkStyle

    , patchTab :: PatchTabStyle
    , nodeTab :: NodeTabStyle

    , order :: Order
    , supportedFlows :: Set NodeFlow
    , font :: { size :: Number, family :: Array String }
    }


type Order = O.Order NodePart


-- FIXME: get rid of, it's just a helper
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
        , labelMaxWidth :: Number
        , valueMaxWidth :: Number
        }
    }


-- FIXME: get rid of, it's just a helper
type Colors =
    { background :: Color
    , patchTab :: { background :: Color, stroke :: Color }
    , nodeTab :: { background :: Color, stroke :: Color }
    , slot :: { stroke :: Color, fill :: Color, label :: Color, value :: Color  }
    , body :: { fill :: Color, shadow :: Color, stroke :: Color }
    , title :: { fill :: Color, background :: Color }
    }



defaultFlags :: Flags
defaultFlags =
    { hasTitle : true
    , hasRibbon : true
    , controlArea : false
    , hasRemoveButton : true
    }


{- findBodySize :: NodeFlow -> (CalculateSide -> Number) -> BodySize -> Size
findBodySize Horizontal _ (h /\ Fixed w) = w <+> h
findBodySize Horizontal f (h /\ StretchByMax) = f StretchByMax <+> h
findBodySize Horizontal f (h /\ StretchBySum) = f StretchBySum <+> h -}


instance showSlotDirection :: Show SlotDirection where
    show Inside = "inside"
    show Outside = "outside"
    show Between = "between"