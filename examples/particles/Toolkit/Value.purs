module Example.Toolkit.Value where

import Prelude

import Data.Maybe (Maybe(..))
import Data.DateTime.Instant (Instant)
import Data.Tuple.Nested ((/\), type (/\))


newtype RgbaColor = RgbaColor { r :: Number, g :: Number, b :: Number, a :: Number }


class Lerp x where
    -- lerp :: { from :: x, to :: x } -> Number -> x
    lerp :: x /\ x -> Number -> Maybe x


data Interpolation x =
    Interpolation
        (Lerp x => { from :: x, to :: x, count :: Int })


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
    | Spread (Interpolation Instruction)
    | Pair Instruction Instruction


data Value
    = Bang
    | Numerical Number
    | Drawing DrawOp
    | Color RgbaColor
    | Instructions Instruction


-- drawEllipse :: Number -> Number -> Instruction
-- drawEllipse a b = Draw $ Ellipse a b


instance lerpNumber :: Lerp Number where
    lerp (from /\ to) amount = Just $ from + ((to - from) * amount)


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


instance lerpInstruction :: Lerp Instruction where
    lerp (Draw drawFrom /\ Draw drawTo) amount =
        Draw <$> lerp (drawFrom /\ drawTo) amount
    lerp (Style styleFrom /\ Style styleTo) amount =
        Style <$> lerp (styleFrom /\ styleTo) amount
    lerp (Transform transformFrom /\ Transform transformTo) amount =
        Transform <$> lerp (transformFrom /\ transformTo) amount
    lerp (Spread fromSpread /\ Spread toSpread) amount =
        Nothing
        -- ?
    lerp (Pair fromA fromB /\ Pair toA toB) amount =
        Pair
            <$> lerp (fromA /\ toA) amount
            <*> lerp (fromB /\ toB) amount
    lerp _ _ = Nothing


instance showValue :: Show Value where
    show _ = "v"

