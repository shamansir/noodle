module App.Style.Hydra
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

import App.Style.Hydra.Colors (colors)
import App.Style.Hydra.Units (units)


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
    , font : { size : 7.0, family : [ "Trispace", "PT Mono", "Andale Mono", "Fira mono", "Menlo" ] }
    , shadow : Solid { offset : 5.0 <+> 5.0 }
    }