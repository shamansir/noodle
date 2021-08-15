module App.Style.Quartz
    (style) where


import Prelude (const)
import Data.Set as Set
import Data.Vec2 ((<+>))

import App.Style
    ( Style
    , NodeFlow(..)
    , Connector(..) , SlotDirection(..), SlotInfoVisibility(..), NodePart(..), TitleMode(..)
    , LinkType(..)
    , ShadowType(..)
    )

import App.Style.Order as Order

import App.Style.Quartz.Colors (colors)
import App.Style.Quartz.Units (units)


style :: Style
style =
    { order :
        Order.make [ Title, UserBodyBetweenSlots ]
    , bg :
        { fill : colors.background
        }
    , body :
        { shadow : Solid { offset : 5.0 <+> 5.0 }
        , size : units.body.size
        , margin : units.body.margin
        , fill : colors.body.fill
        , stroke : colors.body.stroke
        , strokeWidth : units.body.strokeWidth
        , cornerRadius : units.body.cornerRadius
        }
    , slot :
        { connector : Circle 5.0
        , direction : Outside
        , info : Always
        , strokeWidth : units.slot.strokeWidth
        , stroke : colors.slot.stroke
        , fill : colors.slot.fill
        , label : { color : colors.slot.label, maxWidth : units.slot.labelMaxWidth }
        , value : { color : colors.slot.value, maxWidth : units.slot.valueMaxWidth }
        }
    , link : { type : Straight }
    , title :
        { mode : InsideBody
        , background : colors.title.background
        , fill : colors.title.fill
        , size : units.title.size
        , padding : units.title.padding
        }
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0, family : [ "PT Mono", "Andale Mono", "Fira mono", "Menlo", "sans-serif" ] }
    , patchTab : { background : colors.patchTab.background, stroke : colors.patchTab.stroke }
    , nodeTab : { background : colors.nodeTab.background, stroke : colors.nodeTab.stroke }
    }