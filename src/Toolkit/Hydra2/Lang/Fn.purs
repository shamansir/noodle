module Toolkit.Hydra2.Lang.Fn where


import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple (snd) as Tuple
import Data.Maybe (Maybe(..))
import Data.Array ((:))


import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap (WrapRepr)



-- TODO: use typelevel arguments counts like in https://pursuit.purescript.org/packages/purescript-fast-vect/1.1.0
--       or in example/Hydra/Fn


data Argument x = Argument String x


type ArgumentName = String


class ToFn arg a where
    toFn :: a -> String /\ Array (Argument arg)


class PossiblyToFn arg a where
    possiblyToFn :: a -> Maybe (String /\ Array (Argument arg))


instance Functor Argument where
    map :: forall a b. (a -> b) -> Argument a -> Argument b
    map f (Argument name v) = Argument name $ f v


arg :: forall x. ArgumentName -> x -> Argument x
arg = Argument


q :: forall x. ArgumentName -> x -> Argument x
q = Argument -- TODO: private


argValue :: forall x. Argument x -> x
argValue (Argument _ x) = x


argName :: forall x. Argument x -> ArgumentName
argName (Argument name _) = name


data TOrV
    = T Texture
    | V Value


toFnX :: forall a arg. ToFn arg a => a -> String /\ Array arg
toFnX a = map argValue <$> (toFn a :: String /\ Array (Argument arg))


-- instance (ToFn x a) => ToFn x a where
--     toFn :: a -> String /\ Array x
--     toFn a = map val <$> (toFn a :: String /\ Array (Arg x))


{- TOFN GENERIC -}


instance ToFn Value ColorOp where
    toFn :: ColorOp -> String /\ Array (Argument Value)
    toFn = case _ of
        R { scale, offset } -> "r" /\ [ q "scale" scale, q "offset" offset ]
        G { scale, offset } -> "g" /\ [ q "scale" scale, q "offset" offset ]
        B { scale, offset } -> "b" /\ [ q "scale" scale, q "offset" offset ]
        A { scale, offset } -> "a" /\ [ q "scale" scale, q "offset" offset ]
        Posterize { bins, gamma } -> "posterize" /\ [ q "bins" bins, q "gamma" gamma ]
        Shift { r, g, b, a } -> "shift" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ]
        Invert amount -> "invert" /\ [ q "amount" amount ]
        Contrast amount -> "contrast" /\ [ q "amount" amount ]
        Brightness amount -> "brightness" /\ [ q "amount" amount ]
        Luma { treshold, tolerance } -> "luma" /\ [ q "treshold" treshold, q "tolerance" tolerance ]
        Tresh { treshold, tolerance } -> "tresh" /\ [ q "treshold" treshold, q "tolerance" tolerance ]
        Color { r, g, b, a } -> "color" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ]
        Saturate amount -> "saturate" /\ [ q "amount" amount ]
        Hue amount -> "hue" /\ [ q "amount" amount ]
        Colorama amount -> "colorama" /\ [ q "amount" amount ]


instance ToFn Value Modulate where
    toFn :: Modulate -> String /\ Array (Argument Value)
    toFn = case _ of
        Modulate amount -> "modulate" /\ [ q "amount" amount ]
        ModHue amount -> "modHue" /\ [ q "amount" amount ]
        ModKaleid { nSides } -> "modKaleid" /\ [ q "nSides" nSides ]
        ModPixelate { multiple, offset } -> "modPixelate" /\ [ q "multiple" multiple, q "offset" offset ]
        ModRepeat { offsetX, offsetY, repeatX, repeatY } -> "modRepeat" /\ [ q "offsetX" offsetX, q "offsetY" offsetY, q "repeatX" repeatX, q "repeatY" repeatY ]
        ModRepeatX { offset, reps } -> "modRepeatX" /\ [ q "offset" offset, q "reps" reps ]
        ModRepeatY { offset, reps } -> "modRepeatY" /\ [ q "offset" offset, q "reps" reps ]
        ModRotate { multiple, offset } -> "modRotate" /\ [ q "multiple" multiple, q "offset" offset ]
        ModScale { multiple, offset } -> "modScale" /\ [ q "multiple" multiple, q "offset" offset ]
        ModScroll { scrollX, scrollY, speedX, speedY } -> "modScroll" /\ [ q "scrollX" scrollX, q "scrollY" scrollY, q "speedX" speedX, q "speedY" speedY ]
        ModScrollX { scrollX, speed } -> "modScrollX" /\ [ q "scrollX" scrollX, q "speed" speed ]
        ModScrollY { scrollY, speed } -> "modScrollY" /\ [ q "scrollY" scrollY, q "speed" speed ]


instance ToFn Value Blend where
    toFn :: Blend -> String /\ Array (Argument Value)
    toFn = case _ of
        Blend amount -> "blend" /\ [ q "amount" amount ]
        Add amount -> "add" /\ [ q "amount" amount ]
        Sub amount -> "sub" /\ [ q "amount" amount ]
        Mult amount -> "mult" /\ [ q "amount" amount ]
        Diff -> "diff" /\ []
        Layer _ -> "layer" /\ []
        Mask -> "mask" /\ []


instance ToFn Value Geometry where
    toFn :: Geometry -> String /\ Array (Argument Value)
    toFn = case _ of
        GKaleid { nSides } -> "kaleid" /\ [ q "nSides" nSides ]
        GPixelate { pixelX, pixelY } -> "pixelate" /\ [ q "pixelX" pixelX, q "pixelY" pixelY ]
        GRepeat { offsetX, offsetY, repeatX, repeatY } -> "repeat" /\ [ q "offsetX" offsetX, q "offsetY" offsetY, q "repeatX" repeatX, q "repeatY" repeatY ]
        GRepeatX { offset, reps } -> "repeatX" /\ [ q "offset" offset, q "reps" reps ]
        GRepeatY { offset, reps } -> "repeatY" /\ [ q "offset" offset, q "reps" reps ]
        GRotate { angle, speed } -> "rotate" /\ [ q "angle" angle, q "speed" speed ]
        GScale { amount, xMult, yMult, offsetX, offsetY } -> "scale" /\ [ q "amount" amount, q "xMult" xMult, q "yMult" yMult, q "offsetX" offsetX, q "offsetY" offsetY ]
        GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ [ q "scrollX" scrollX, q "scrollY" scrollY, q "speedX" speedX, q "speedY" speedY ]
        GScrollX { scrollX, speed } -> "scrollX" /\ [ q "scrollX" scrollX, q "speed" speed ]
        GScrollY { scrollY, speed } -> "scrollY" /\ [ q "scrollY" scrollY, q "speed" speed ]


instance ToFn Value Ease where
    toFn :: Ease -> String /\ Array (Argument Value)
    toFn = case _ of
        Linear -> "linear" /\ []
        Fast v -> "fast" /\ [ q "v" v ]
        Smooth v -> "smooth" /\ [ q "v" v ]
        Fit { low, high } -> "fit" /\ [ q "low" low, q "high" high ]
        Offset v -> "offset" /\ [ q "v" v ]
        InOutCubic -> "inOutCubic" /\ []


instance PossiblyToFn Value Source where
    possiblyToFn :: Source -> Maybe (String /\ Array (Argument Value))
    possiblyToFn = case _ of
        Gradient { speed } -> Just $ "gradient" /\ [ q "speed" speed ]
        Noise { scale, offset } -> Just $ "noise" /\ [ q "scale" scale, q "offset" offset ]
        Osc { frequency, sync, offset } -> Just $ "osc" /\ [ q "frequency" frequency, q "sync" sync, q "offset" offset ]
        Shape { sides, radius, smoothing } -> Just $ "shape" /\ [ q "sides" sides, q "radius" radius, q "smoothing" smoothing ]
        Solid { r, g, b, a } -> Just $ "solid" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ]
        Voronoi { scale, speed, blending } -> Just $ "voronoi" /\ [ q "scale" scale, q "speed" speed, q "blending" blending ]
        _ -> Nothing


instance PossiblyToFn TOrV Texture where
    possiblyToFn :: Texture -> Maybe (String /\ Array (Argument TOrV))
    possiblyToFn = case _ of
        Empty -> Nothing
        From _ -> Nothing
        -- FIXME: use PossiblyToFn
        BlendOf { what, with } blend ->
            case (toFn blend :: String /\ Array (Argument Value)) of
                name /\ args -> Just $ name /\ ((q "what" $ T what) : (map V <$> args) <> [ q "with" $ T with ])
        WithColor texture cop ->
            case (toFn cop :: String /\ Array ((Argument Value))) of
                name /\ args -> Just $ name /\ ((map V <$> args) <> [ q "texture" $ T texture ])
        ModulateWith { what, with } mod ->
            case (toFn mod :: String /\ Array ((Argument Value))) of
                name /\ args -> Just $ name /\ ((q "what" $ T what) : (map V <$> args) <> [ q "with" $ T with ])
        Geometry texture gmt ->
            case (toFn gmt :: String /\ Array ((Argument Value))) of
                name /\ args -> Just $ name /\ ((map V <$> args) <> [ q "texture" $ T texture ])