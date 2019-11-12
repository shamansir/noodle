module Example.Toolkit.Value where

import Prelude

import Data.Int (toNumber, floor)
import Data.Array ((!!))
import Data.String (joinWith)
import Data.Array (range, catMaybes, length, mapWithIndex, zip) as Array
import Data.Maybe (Maybe(..))
import Data.DateTime.Instant (Instant)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Lerp (class Lerp, lerp)
import Data.Spread (Spread) as S
import Data.Spread (run) as Spread
import Data.Vec2


newtype RgbaColor = RgbaColor { r :: Number, g :: Number, b :: Number, a :: Number }


-- newtype Interpolation =
--     Interpolation (Lerp x => { from :: x, to :: x, count : Int })


data DrawOp
    = Ellipse Vec2 -- axes
    | Rect Vec2 -- dimensions


data StyleOp
    = Fill RgbaColor
    | Stroke RgbaColor Number -- color, width


data TransformOp
    = Move Vec2 -- position
    | Scale Vec2 -- axes


data Instruction
    = NoOp
    | Draw DrawOp
    | Transform TransformOp
    | Style StyleOp


data Value
    = Bang
    | Numerical Number
    | Vector Vec2
    | Color RgbaColor
    | Apply Instruction
    | Pair Value Value
    | Spread (S.Spread Value)


-- drawEllipse :: Number -> Number -> Instruction
-- drawEllipse a b = Draw $ Ellipse a b


instance lerpRgbaColor :: Lerp RgbaColor where
    lerp (RgbaColor from /\ RgbaColor to) amount =
        (\r g b a -> RgbaColor { r, g, b, a })
            <$> lerp (from.r /\ to.r) amount
            <*> lerp (from.g /\ to.g) amount
            <*> lerp (from.b /\ to.b) amount
            <*> lerp (from.a /\ to.a) amount


instance lerpDrawOp :: Lerp DrawOp where
    lerp (Ellipse from /\ Ellipse to) amount =
        Ellipse <$> lerp (from /\ to) amount
    lerp (Rect from /\ Rect to) amount =
        Rect <$> lerp (from /\ to) amount
    lerp _ _ = Nothing


instance lerpStyleOp :: Lerp StyleOp where
    lerp (Fill from /\ Fill to) amount =
        Fill <$> lerp (from /\ to) amount
    lerp (Stroke fromColor fromWidth /\ Stroke toColor toWidth) amount =
        Stroke
            <$> lerp (fromColor /\ toColor) amount
            <*> lerp (fromWidth /\ toWidth) amount
    lerp _ _ = Nothing


instance lerpTransformOp :: Lerp TransformOp where
    lerp (Move from /\ Move to) amount =
        Move <$> lerp (from /\ to) amount
    lerp (Scale from /\ Scale to) amount =
        Scale <$> lerp (from /\ to) amount
    lerp _ _ = Nothing


-- instance lerpInterpolation :: Lerp (Interpolation Instruction) where
--     lerp _ _ = Nothing


instance lerpInstruction :: Lerp Instruction where
    lerp (Draw drawFrom /\ Draw drawTo) amount =
        Draw <$> lerp (drawFrom /\ drawTo) amount
    lerp (Style styleFrom /\ Style styleTo) amount =
        Style <$> lerp (styleFrom /\ styleTo) amount
    lerp (Transform transformFrom /\ Transform transformTo) amount =
        Transform <$> lerp (transformFrom /\ transformTo) amount
    lerp _ _ = Nothing


instance lerpValue :: Lerp Value where
    lerp (Bang /\ Bang) _ = Just Bang
    lerp (Numerical from /\ Numerical to) amount =
        Numerical <$> lerp (from /\ to) amount
    lerp (Vector from /\ Vector to) amount =
        Vector <$> lerp (from /\ to) amount
    lerp (Color from /\ Color to) amount =
        Color <$> lerp (from /\ to) amount
    lerp (Apply from /\ Apply to) amount =
        Apply <$> lerp (from /\ to) amount
    lerp (Pair fromA fromB /\ Pair toA toB) amount =
        Pair
            <$> lerp (fromA /\ toA) amount
            <*> lerp (fromB /\ toB) amount
    lerp _ _ = Nothing


instance showRgbaColor :: Show RgbaColor where
    show (RgbaColor { r, g, b, a }) =
        "rgba(" <> show r <> ","
                <> show g <> ","
                <> show b <> ","
                <> show a <> ")"


instance showDrawOp :: Show DrawOp where
    show (Ellipse axes) = "ellipse: " <> show axes
    show (Rect dimensions) = "rect: " <> show dimensions


instance showStyleOp :: Show StyleOp where
    show (Fill color) = "fill: " <> show color
    show (Stroke color width) = "stroke: " <> show color <> "; " <> show width


instance showTransformOp :: Show TransformOp where
    show (Move position) = "move: " <> show position
    show (Scale axes) = "scale: " <> show axes


instance showInstruction :: Show Instruction where
    show NoOp = "no-op"
    show (Draw draw) = "draw: " <> show draw
    show (Style style) = "style: " <> show style
    show (Transform transform) = "transform: " <> show transform


instance showValue :: Show Value where
    show Bang = "bang"
    show (Numerical n) = "num: " <> show n
    show (Color color) = "color: " <> show color
    show (Vector vec) = "vec: " <> show vec
    show (Apply inst) = "apply: " <> show inst
    show (Pair valA valB) = "pair: ( " <> show valA <> " /\\ " <> show valB <> " )"
    show (Spread spread) = "spread: " <> show spread -- joinWith "," (show <$> Spread.run spread)


colorToCss :: RgbaColor -> String
colorToCss (RgbaColor { r, g, b, a }) =
    "rgba(" <> (show $ floor $ r * 255.0) <> ","
            <> (show $ floor $ g * 255.0) <> ","
            <> (show $ floor $ b * 255.0) <> ","
            <> show a <> ")"


ellipse :: Vec2 -> Value
ellipse = Apply <<< Draw <<< Ellipse


rect :: Vec2 -> Value
rect = Apply <<< Draw <<< Rect


move :: Vec2 -> Value
move = Apply <<< Transform <<< Move


scale :: Vec2 -> Value
scale = Apply <<< Transform <<< Scale


stroke :: RgbaColor -> Number -> Value
stroke color width = Apply $ Style $ Stroke color width


fill :: RgbaColor -> Value
fill = Apply <<< Style <<< Fill
