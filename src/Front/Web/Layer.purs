module Web.Layer where

import Prelude


data TargetLayer
    = SVG
    -- | SVG_Empty
    | HTML
    -- | HTML_Empty


derive instance Eq TargetLayer
derive instance Ord TargetLayer
