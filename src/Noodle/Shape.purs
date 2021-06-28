module Noodle.Shape
    where


type Shape d =
    { default :: d
    , accept :: (d -> Boolean)
    , isHot :: Boolean
    }