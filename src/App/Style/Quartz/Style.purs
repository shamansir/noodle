module App.Style.Quartz
    (style) where


import Prelude (const)
import Data.Set as Set

import App.Style
    ( Style
    , NodeFlow(..)
    , Connector(..) , SlotDirection(..), SlotInfoVisibility(..)
    , LinkType(..)
    , TitlePos(..)
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
    , title : const OnTop
    , link : { type : Straight }
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0 }
    }