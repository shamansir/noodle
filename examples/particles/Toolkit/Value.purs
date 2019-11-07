module Example.Toolkit.Value where

import Prelude

import Data.Int (toNumber)
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


newtype RgbaColor = RgbaColor { r :: Number, g :: Number, b :: Number, a :: Number }


-- newtype Interpolation =
--     Interpolation (Lerp x => { from :: x, to :: x, count : Int })


data DrawOp
    = Ellipse Number Number
    | Rect Number Number


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
    | Pair Instruction Instruction


data Value
    = Bang
    | Numerical Number
    | Color RgbaColor
    | Apply Instruction
    | Spread (S.Spread Instruction)


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
    lerp (Ellipse fromA fromB /\ Ellipse toA toB) amount =
        Ellipse <$> lerp (fromA /\ toA) amount <*> lerp (fromB /\ toB) amount
    lerp (Rect fromW fromH /\ Rect toW toH) amount =
        Rect <$> lerp (fromW /\ toW) amount <*> lerp (fromH /\ toH) amount
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
    lerp (Move fromX fromY /\ Move toX toY) amount =
        Move <$> lerp (fromX /\ toX) amount <*> lerp (fromY /\ toY) amount
    lerp (Scale fromX fromY /\ Scale toX toY) amount =
        Scale <$> lerp (fromX /\ toX) amount <*> lerp (fromY /\ toY) amount
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
    show (Ellipse a b) = "ellipse: " <> show a <> "x" <> show b
    show (Rect w h) = "rect: " <> show w <> "x" <> show h


instance showStyleOp :: Show StyleOp where
    show (Fill color) = "fill: " <> show color
    show (Stroke color width) = "stroke: " <> show color <> "; " <> show width


instance showTransformOp :: Show TransformOp where
    show (Move x y) = "move: " <> show x <> ":" <> show y
    show (Scale x y) = "scale: " <> show x <> ":" <> show y


instance showInstruction :: Show Instruction where
    show NoOp = "no-op"
    show (Draw draw) = "draw: " <> show draw
    show (Style style) = "style: " <> show style
    show (Transform transform) = "transform: " <> show transform
    show (Pair instA instB) = "pair: ( " <> show instA <> " /\\ " <> show instB <> " )"


instance showValue :: Show Value where
    show Bang = "bang"
    show (Numerical n) = "num: " <> show n
    show (Color color) = "color: " <> show color
    show (Apply inst) = "apply: " <> show inst
    show (Spread spread) = "spread: " <> joinWith "," (show <$> Spread.run spread)

