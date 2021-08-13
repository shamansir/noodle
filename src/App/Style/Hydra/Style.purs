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

import App.Style.Hydra.Colors (colors)
import App.Style.Hydra.Units (units)

import Data.Set as Set
import Data.Set.Ordered as OSet


style :: Style
style =
    { colors
    , units
    , order : OSet.fromFoldable [ Title, UserBody 55.0, OnlyInlets, OnlyOutlets ]
    , slot :
        { connector : Circle
        , direction : Inside
        , info : Always
        }
    , link : Straight
    , title : OutsideBody
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0, family : [ "Trispace", "PT Mono", "Andale Mono", "Fira mono", "Menlo" ] }
    , shadow : Solid { offset : 5.0 <+> 5.0 }
    }
