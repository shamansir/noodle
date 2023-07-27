module Toolkit.Hydra2.Lang.Fn where


import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..))
import Data.Array ((:))


import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap (WrapRepr)



-- TODO: use typelevel arguments counts like in https://pursuit.purescript.org/packages/purescript-fast-vect/1.1.0
--       or in example/Hydra/Fn


class ToFn arg a where
    toFn :: a -> String /\ Array arg


class PossiblyToFn arg a where
    possiblyToFn :: a -> Maybe (String /\ Array arg)


-- type ArgName = String


data TOrV
    = T Texture
    | V Value


-- instance ToFn (ArgName /\ arg) a => ToFn arg a where
--     toFn :: a -> String /\ Array arg
--     toFn a = map (map ?wh) <$> (toFn a :: String /\ Array (ArgName /\ arg))


{- TOFN GENERIC -}


instance ToFn Value ColorOp where
    toFn :: ColorOp -> String /\ Array Value
    toFn = case _ of
        R { scale, offset } -> "r" /\ [ scale, offset ]
        G { scale, offset } -> "g" /\ [ scale, offset ]
        B { scale, offset } -> "b" /\ [ scale, offset ]
        A { scale, offset } -> "a" /\ [ scale, offset ]
        Posterize { bins, gamma } -> "a" /\ [ bins, gamma ]
        Shift { r, g, b, a } -> "shift" /\ [ r, g, b, a ]
        Invert amount -> "invert" /\ [ amount ]
        Contrast amount -> "contrast" /\ [ amount ]
        Brightness amount -> "brightness" /\ [ amount ]
        Luma { treshold, tolerance } -> "luma" /\ [ treshold, tolerance ]
        Tresh { treshold, tolerance } -> "tresh" /\ [ treshold, tolerance ]
        Color { r, g, b, a } -> "color" /\ [ r, g, b, a ]
        Saturate amount -> "saturate" /\ [ amount ]
        Hue amount -> "hue" /\ [ amount ]
        Colorama amount -> "colorama" /\ [ amount ]


instance ToFn Value Modulate where
    toFn :: Modulate -> String /\ Array Value
    toFn = case _ of
        Modulate amount -> "modulate" /\ [ amount ]
        ModHue amount -> "modHue" /\ [ amount ]
        ModKaleid { nSides } -> "modKaleid" /\ [ nSides ]
        ModPixelate { multiple, offset } -> "modPixelate" /\ [ multiple, offset ]
        ModRepeat { offsetX, offsetY, repeatX, repeatY } -> "modRepeat" /\ [ offsetX, offsetY, repeatX, repeatY ]
        ModRepeatX { offset, reps } -> "modRepeatX" /\ [ offset, reps ]
        ModRepeatY { offset, reps } -> "modRepeatY" /\ [ offset, reps ]
        ModRotate { multiple, offset } -> "modRotate" /\ [ multiple, offset ]
        ModScale { multiple, offset } -> "modScale" /\ [ multiple, offset ]
        ModScroll { scrollX, scrollY, speedX, speedY } -> "modScroll" /\ [ scrollX, scrollY, speedX, speedY ]
        ModScrollX { scrollX, speed } -> "modScrollX" /\ [ scrollX, speed ]
        ModScrollY { scrollY, speed } -> "modScrollY" /\ [ scrollY, speed ]


instance ToFn Value Blend where
    toFn :: Blend -> String /\ Array Value
    toFn = case _ of
        Blend amount -> "blend" /\ [ amount ]
        Add amount -> "add" /\ [ amount ]
        Sub amount -> "sub" /\ [ amount ]
        Mult amount -> "mult" /\ [ amount ]
        Diff -> "diff" /\ []
        Layer _ -> "layer" /\ []
        Mask -> "mask" /\ []


instance ToFn Value Geometry where
    toFn :: Geometry -> String /\ Array Value
    toFn = case _ of
        GKaleid { nSides } -> "kaleid" /\ [ nSides ]
        GPixelate { pixelX, pixelY } -> "pixelate" /\ [ pixelX, pixelY ]
        GRepeat { offsetX, offsetY, repeatX, repeatY } -> "repeat" /\ [ offsetX, offsetY, repeatX, repeatY ]
        GRepeatX { offset, reps } -> "repeatX" /\ [ offset, reps ]
        GRepeatY { offset, reps } -> "repeatY" /\ [ offset, reps ]
        GRotate { angle, speed } -> "rotate" /\ [ angle, speed ]
        GScale { amount, xMult, yMult, offsetX, offsetY } -> "scale" /\ [ amount, xMult, yMult, offsetX, offsetY ]
        GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ [ scrollX, scrollY, speedX, speedY ]
        GScrollX { scrollX, speed } -> "scrollX" /\ [ scrollX, speed ]
        GScrollY { scrollY, speed } -> "scrollY" /\ [ scrollY, speed ]


instance ToFn Value Ease where
    toFn :: Ease -> String /\ Array Value
    toFn = case _ of
        Linear -> "linear" /\ []
        Fast v -> "fast" /\ [ v ]
        Smooth v -> "smooth" /\ [ v ]
        Fit { low, high } -> "fit" /\ [ low, high ]
        Offset v -> "offset" /\ [ v ]
        InOutCubic -> "inOutCubic" /\ []


instance PossiblyToFn Value Source where
    possiblyToFn :: Source -> Maybe (String /\ Array Value)
    possiblyToFn = case _ of
        Gradient { speed } -> Just $ "gradient" /\ [ speed ]
        Noise { scale, offset } -> Just $ "noise" /\ [ scale, offset ]
        Osc { frequency, sync, offset } -> Just $ "osc" /\ [ frequency, sync, offset ]
        Shape { sides, radius, smoothing } -> Just $ "shape" /\ [ sides, radius, smoothing ]
        Solid { r, g, b, a } -> Just $ "solid" /\ [ r, g, b, a ]
        Voronoi { scale, speed, blending } -> Just $ "voronoi" /\ [ scale, speed, blending ]
        _ -> Nothing


instance PossiblyToFn TOrV Texture where
    possiblyToFn :: Texture -> Maybe (String /\ Array TOrV)
    possiblyToFn = case _ of
        Empty -> Nothing
        From _ -> Nothing
        -- FIXME: use PossiblyToFn
        BlendOf { what, with } blend ->
            case (toFn blend :: String /\ Array Value) of
                name /\ args -> Just $ name /\ (T what : (V <$> args) <> [ T with ])
        WithColor texture cop ->
            case (toFn cop :: String /\ Array Value) of
                name /\ args -> Just $ name /\ ((V <$> args) <> [ T texture ])
        ModulateWith { what, with } mod ->
            case (toFn mod :: String /\ Array Value) of
                name /\ args -> Just $ name /\ (T what : (V <$> args) <> [ T with ])
        Geometry texture gmt ->
            case (toFn gmt :: String /\ Array Value) of
                name /\ args -> Just $ name /\ ((V <$> args) <> [ T texture ])