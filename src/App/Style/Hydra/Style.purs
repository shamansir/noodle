module App.Style.Hydra
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

import App.Style.Hydra.Colors (colors)
import App.Style.Hydra.Units (units)

import Data.Set as Set
import Data.Set.Ordered as OSet


style :: Style
style =
    { order :
        Order.fromFoldable
            [ Title, UserBody 55.0, OnlyInlets, OnlyOutlets ]
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
        , direction : Inside
        , info : Always
        , strokeWidth : units.slot.strokeWidth
        , stroke : colors.slot.stroke
        , fill : colors.slot.fill
        , label : { color : colors.slot.label, maxWidth : units.slot.labelMaxWidth }
        , value : { color : colors.slot.value, maxWidth : units.slot.valueMaxWidth }
        }
    , link : { type : Straight }
    , title :
        { mode : OutsideBody
        , background : colors.title.background
        , fill : colors.title.fill
        , size : units.title.size
        , padding : units.title.padding
        }
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0, family : [ "Trispace", "PT Mono", "Andale Mono", "Fira mono", "Menlo" ] }
    , patchTab : { background : colors.patchTab.background, stroke : colors.patchTab.stroke }
    , nodeTab : { background : colors.nodeTab.background, stroke : colors.nodeTab.stroke }
    }
