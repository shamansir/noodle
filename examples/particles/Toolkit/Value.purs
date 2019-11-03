module Example.Toolkit.Value where

import Prelude (class Show, (<>), show)

import Data.DateTime.Instant (Instant)


newtype RgbaColor = RgbaColor { r :: Number, g :: Number, b :: Number, a :: Number }


class Lerp x where
    lerp :: Number -> { from :: x, to :: x } -> x


data Interpolation x =
    Interpolation
        (Lerp x => { from :: x, to :: x, count :: Int })


data DrawOp
    = Circle
    | Rect


data StyleOp
    = Fill RgbaColor
    | Stroke RgbaColor Number


data TransformOp
    = Move Number Number
    | Scale Number Number


data Instruction
    = NoOp
    | Draw DrawOp
    | Transform TransformOp
    | Style StyleOp
    | Spread (Interpolation Instruction)
    | Pair Instruction Instruction


data Value
    = Bang
    | Numerical Number
    | Drawing DrawOp
    | Color RgbaColor
    | Instructions (Array Instruction)


instance showValue :: Show Value where
    show _ = "v"

