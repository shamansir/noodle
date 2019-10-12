module Example.Toolkit.Value where

import Prelude (class Show, (<>), show)

import Data.DateTime.Instant (Instant)


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
    | Numeric Number
    | Trigger Boolean
    | Period Number
    | Magic Number Number
    | Time Instant


instance showValue :: Show Value where
    show Bang = "bang"
    show (Color r g b) = "color: (" <> show r <> ", " <> show g <> ", " <> show b <> ")"
    show (Shape shape) = "shape: " <> show shape
    show (Random n) = "random: " <> show n
    show (Numeric n) = "number: " <> show n
    show (Trigger state) = "trigger: " <> if state then "on" else "off"
    show (Period n) = "period: " <> show n
    show (Magic n1 n2) = "magin: " <> show n1 <> ", " <> show n2
    show (Time t) = "time: " <> show t


instance showParticleShape :: Show ParticleShape where
    show Circle = "circle"
    show Cross = "cross"
    show Square = "square"
    show Diamond = "diamond"

