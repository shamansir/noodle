module App.Style.Quartz
    (style) where


import Prelude (const)
import Data.Set as Set
import Data.Vec2 ((<+>))

import App.Style
    ( Style
    , NodeFlow(..)
    , Connector(..) , SlotDirection(..), SlotInfoVisibility(..)
    , LinkType(..)
    , ShadowType(..)
    )

import App.Style.Quartz.Colors (colors)
import App.Style.Quartz.Units (units)


style :: Style
style =
    { colors
    , units
    , slot :
        { connector : Circle
        , direction : Outside
        , info : Always
        }
    , link : Straight
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0, family : "'PT Mono', 'Andale Mono', 'Fira mono', 'Menlo', sans-serif" }
    , shadow : Solid { offset : 5.0 <+> 5.0 }
    }