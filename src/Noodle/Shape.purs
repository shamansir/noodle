module Noodle.Shape
    where


import Noodle.Channel as N


type Shape d =
    { default :: d
    , channel :: N.Channel d
    , isHot :: Boolean
    }