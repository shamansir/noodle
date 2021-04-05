module RayDraw.Toolkit.Value where


import Prelude
import Data.Int (floor)

newtype RgbaColor = RgbaColor { r :: Number, g :: Number, b :: Number, a :: Number }

data Value
    = Bang
    | Color RgbaColor


data Aggregate
    = All
    | Exactly Int


instance showValue :: Show Value where
    show Bang = "◌"
    show (Color color) = "color: " <> show color


instance showRgbaColor :: Show RgbaColor where
    show (RgbaColor { r, g, b, a }) =
        "rgba(" <> show r <> ","
                <> show g <> ","
                <> show b <> ","
                <> show a <> ")"

colorToCss :: RgbaColor -> String
colorToCss (RgbaColor { r, g, b, a }) =
    "rgba(" <> (show $ floor $ r * 255.0) <> ","
            <> (show $ floor $ g * 255.0) <> ","
            <> (show $ floor $ b * 255.0) <> ","
            <> show a <> ")"
