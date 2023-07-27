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


-- TODO
newtype Fn arg = Fn (String /\ Array (Argument arg))


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
        ModRepeat { repeatX, repeatY, offsetX, offsetY } -> "modRepeat" /\ [ q "repeatX" repeatX, q "repeatY" repeatY, q "offsetX" offsetX, q "offsetY" offsetY ]
        ModRepeatX { reps, offset } -> "modRepeatX" /\ [ q "reps" reps, q "offset" offset ]
        ModRepeatY { reps, offset } -> "modRepeatY" /\ [ q "reps" reps, q "offset" offset ]
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
        GRepeat { repeatX, repeatY, offsetX, offsetY } -> "repeat" /\ [ q "repeatX" repeatX, q "repeatY" repeatY, q "offsetX" offsetX, q "offsetY" offsetY ]
        GRepeatX { reps, offset } -> "repeatX" /\ [ q "reps" reps, q "offset" offset ]
        GRepeatY { reps, offset } -> "repeatY" /\ [ q "reps" reps, q "offset" offset ]
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


newtype KnownFn = KnownFn String


nameOf :: KnownFn -> String
nameOf (KnownFn name) = name


instance PossiblyToFn Value KnownFn where
    possiblyToFn :: KnownFn -> Maybe (String /\ Array (Argument Value))
    possiblyToFn = nameOf >>> fromKnownFn



-- TODO: probably duplicates something
-- TODO: private
fromKnownFn :: String -> Maybe (String /\ Array (Argument Value))
fromKnownFn = case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ q "scale" $ Number 10.0, q "offset" $ Number 0.1 ]
    "voronoi" -> Just $ "voronoi" /\ [ q "scale" $ Number 5.0, q "speed" $ Number 0.3, q "blending" $ Number 0.3 ]
    "osc" -> Just $ "osc" /\ [ q "frequency" $ Number 60.0, q "sync" $ Number 0.1, q "offset" $ Number 0.0 ]
    "shape" -> Just $ "shape" /\ [ q "sides" $ Number 3.0, q "radius" $ Number 0.3, q "smoothing" $ Number 0.01 ]
    "gradient" -> Just $ "gradient" /\ [ q "speed" $ Number 0.0 ]
    -- "src" -> Source
    "solid" -> Just $ "solid" /\ [ q "r" $ Number 0.0, q "g" $ Number 0.0, q "b" $ Number 0.0, q "a" $ Number 1.0 ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ q "angle" $ Number 10.0, q "speed" $ Number 0.0 ]
    "scale" -> Just $ "scale" /\ [ q "amount" $ Number 1.5, q "xMult" $ Number 1.0, q "yMult" $ Number 1.0, q "offsetX" $ Number 0.5, q "offsetY" $ Number 0.5 ]
    "pixelate" -> Just $ "pixelate" /\ [ q "pixelX" $ Number 20.0, q "pixelY" $ Number 20.0 ]
    "repeat" -> Just $ "repeat" /\ [ q "repeatX" $ Number 3.0, q "repeatY" $ Number 3.0, q "offsetX" $ Number 0.0, q "offsetY" $ Number 0.0 ]
    "repeatX" -> Just $ "repeatX" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.0 ]
    "repeatY" -> Just $ "repeatY" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.0 ]
    "kaleid" -> Just $ "kaleid" /\ [ q "nSides" $ Number 4.0 ]
    "scroll" -> Just $ "scroll" /\ [ q "scrollX" $ Number 0.5, q "scrollY" $ Number 0.5, q "speedX" $ Number 0.0, q "speedY" $ Number 0.0 ]
    "scrollX" -> Just $ "scrollX" /\ [ q "scrollX" $ Number 0.5, q "speed" $ Number 0.0 ]
    "scrollY" -> Just $ "scrollY" /\ [ q "scrollY" $ Number 0.5, q "speed" $ Number 0.0 ]

    "posterize" -> Just $ "posterize" /\ [ q "bins" $ Number 3.0, q "gamma" $ Number 6.0 ]
    "shift" -> Just $ "shift" /\ [ q "r" $ Number 0.5, q "g" $ Number 0.0, q "b" $ Number 0.0, q "a" $ Number 0.5 ]
    "invert" -> Just $ "invert" /\ [ q "amount" $ Number 1.0 ]
    "contrast" -> Just $ "contrast" /\ [ q "amount" $ Number 1.6 ]
    "brightness" -> Just $ "brightness" /\ [ q "amount" $ Number 0.4 ]
    "luma" -> Just $ "luma" /\ [ q "treshold" $ Number 0.5, q "tolerance" $ Number 0.1 ]
    "tresh" -> Just $ "tresh" /\ [ q "treshold" $ Number 0.5, q "tolerance" $ Number 0.04 ]
    "color" -> Just $ "color" /\ [ q "r" $ Number 1.0, q "g" $ Number 1.0, q "b" $ Number 1.0, q "a" $ Number 1.0 ]
    "saturate" -> Just $ "saturate" /\ [ q "amount" $ Number 2.0 ]
    "hue" -> Just $ "hue" /\ [ q "amount" $ Number 0.4 ]
    "colorama" -> Just $ "colorama" /\ [ q "amount" $ Number 0.005 ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ]
    "g" -> Just $ "g" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ]
    "b" -> Just $ "b" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ]
    "a" -> Just $ "a" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ q "amount" $ Number 1.0 ]
    "sub" -> Just $ "sub" /\ [ q "amount" $ Number 1.0 ]
    "layer" -> Just $ "layer" /\ []
    "blend" -> Just $ "blend" /\ [ q "amount" $ Number 0.5 ]
    "mult" -> Just $ "mult" /\ [ q "amount" $ Number 1.0 ]
    "diff" -> Just $ "diff" /\ []
    "mask" -> Just $ "mask" /\ []

    "modulateRepeat" -> Just $ "modRepeat" /\ [ q "repeatX" $ Number 3.0, q "repeatY" $ Number 3.0, q "offsetX" $ Number 0.5, q "offsetY" $ Number 0.5 ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.5 ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.5 ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ q "nSides" $ Number 4.0 ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ q "scrollX" $ Number 0.5, q "speed" $ Number 0.0 ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ q "scrollY" $ Number 0.5, q "speed" $ Number 0.0 ]
    "modulate" -> Just $ "modulate" /\ [ q "amount" $ Number 0.1 ]
    "modulateScale" -> Just $ "modScale" /\ [ q "multiple" $ Number 1.0, q "offset" $ Number 1.0 ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ q "multiple" $ Number 10.0, q "offset" $ Number 3.0 ]
    "modulateRotate" -> Just $ "modRotate" /\ [ q "multiple" $ Number 1.0, q "offset" $ Number 0.0 ]
    "modulateHue" -> Just $ "modHue" /\ [ q "amount" $ Number 1.0 ]

    -- "initCam" -> ExternalSources
    -- "initImage" -> ExternalSources
    -- "initVideo" -> ExternalSources
    -- "init" -> ExternalSources
    -- "initStream" -> ExternalSources
    -- "initScreen" -> ExternalSources

    -- "render" -> SynthSettings
    -- "update" -> SynthSettings
    -- "setResolution" -> SynthSettings
    -- "hush" -> SynthSettings
    -- "setFunction" -> SynthSettings
    -- "speed" -> SynthSettings
    -- "bpm" -> SynthSettings
    -- "width" -> SynthSettings
    -- "height" -> SynthSettings
    -- "time" -> SynthSettings
    -- "mouse" -> SynthSettings
    -- "pi" -> SynthSettings

    -- "fft" -> Audio
    -- "setSmooth" -> Audio
    -- "setCutoff" -> Audio
    -- "setBins" -> Audio
    -- "setScale" -> Audio
    -- "hide" -> Audio
    -- "show" -> Audio

    -- "setScale" -> Audio

    -- "fast" -> Array
    -- "smooth" -> Array
    -- "ease" -> Array
    -- "offset" -> Array
    -- "fit" -> Array

    -- "out" -> Out

    "linear" -> Just $ "linear" /\ []
    "fast" -> Just $ "fast" /\ [ q "v" $ Number 1.0 ]
    "smooth" -> Just $ "smooth" /\ [ q "v" $ Number 1.0 ]
    "fit" -> Just $ "fit" /\ [ q "low" $ Number 0.0, q "high" $ Number 1.0 ]
    "offset" -> Just $ "offset" /\ [ q "v" $ Number 0.5 ]
    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing