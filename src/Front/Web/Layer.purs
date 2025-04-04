module Web.Layer where

import Prelude


data TargetLayer
    = SVG
    | HTML


derive instance Eq TargetLayer
derive instance Ord TargetLayer
