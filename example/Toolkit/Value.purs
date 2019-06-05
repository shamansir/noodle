module Example.Toolkit.Value where

import Prelude (class Show)


data ParticleShape
    = Circle
    | Cross
    | Square
    | Diamond


data Value
    = Bang
    | Color Number Number Number
    | Shape ParticleShape
    | Random Number
    | Number' Number
    | Trigger Boolean
    | Period Number
    | Magic Number Number


instance showValue :: Show Value where
    show _ = "DATA"
    -- show Bang = "bang"
    -- show (Color r g b) = "color"
    -- show (Shape shape) = "shape"
    -- show (Random n) = "random"
    -- show (Number' n) = "number"
