module Hydra.Try where

import Prelude (($), unit, (>>>), (<<<), (>>=), (=<<), (<$>), (#))

-- import Data.Vec as Vec
-- import Data.Vec (Vec)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))



import Hydra
    ( Hydra(..), Value(..), Texture, Buffer(..)
    , Source(..), Geometry(..), Blend(..), Color(..), Modulate(..)
    )
import Hydra as H
import Hydra.Fn as Fn


{- ====== Source ====== -}

noise :: H.HydraFn2M
noise =
    tryV2M
        ( Num 10.0 /\ Num 0.1 )
        (\scale offset -> Tex $ H.textureOf $ Noise { scale, offset })


voronoi :: H.HydraFn3M
voronoi =
    tryV3M
        ( Num 5.0 /\ Num 0.3 /\ Num 0.3 )
        (\scale speed blending -> Tex $ H.textureOf $ Voronoi { scale, speed, blending })


osc :: H.HydraFn3M
osc =
    tryV3M
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Tex $ H.textureOf $ Osc { freq, sync, offset })


{- osc' :: HydraFn3
osc' =
    tryV3
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Tex $ textureOf $ Osc { freq, sync, offset }) -}


shape :: H.HydraFn3M
shape =
    tryV3M
        ( Num 3.0 /\ Num 0.3 /\ Num 0.01 )
        (\sides radius smoothing -> Tex $ H.textureOf $ Shape { sides, radius, smoothing })


gradient :: H.HydraFn1M
gradient =
    tryV1M
        ( CanvasWidth )
        (\speed -> Tex $ H.textureOf $ Gradient { speed })


solid :: H.HydraFn4M
solid =
    tryV4M
        ( Num 0.0 /\ Num 0.0 /\ Num 0.0 /\ Num 1.0 )
        (\r g b a -> Tex $ H.textureOf $ Solid { r, g, b, a })


{- ====== Geometry ====== -}


rotate :: H.HydraTFn2M
rotate =
    tryTV2M
        ( H.defaultTexture /\ Num 10.0 /\ Num 0.0 )
        (\what angle speed ->
            Tex $ H.addModifier what $ H.geometry $ Rotate { angle, speed }
        )


scale :: H.HydraTFn5M
scale =
    tryTV5M
        ( H.defaultTexture /\ Num 1.5 /\ Num 1.0 /\ Num 1.0 /\ Num 0.5 /\ Num 0.5 )
        (\what amount xMult yMult offsetX offsetY ->
            Tex $ H.addModifier what $ H.geometry $ Scale { amount, xMult, yMult, offsetX, offsetY }
        )


pixelate :: H.HydraTFn2M
pixelate =
    tryTV2M
        ( H.defaultTexture /\ Num 20.0 /\ Num 20.0 )
        (\what pixelX pixelY ->
            Tex $ H.addModifier what $ H.geometry $ Pixelate { pixelX, pixelY }
        )


repeat :: H.HydraTFn4M
repeat =
    tryTV4M
        ( H.defaultTexture /\ Num 3.0 /\ Num 3.0 /\ Num 0.0 /\ Num 0.0 )
        (\what repeatX repeatY offsetX offsetY ->
            Tex $ H.addModifier what $ H.geometry $ Repeat { repeatX, repeatY, offsetX, offsetY }
        )


repeatX :: H.HydraTFn2M
repeatX =
    tryTV2M
        ( H.defaultTexture /\ Num 3.0 /\ Num 0.0 )
        (\what reps offset ->
            Tex $ H.addModifier what $ H.geometry $ RepeatX { reps, offset }
        )


repeatY :: H.HydraTFn2M
repeatY =
    tryTV2M
        ( H.defaultTexture /\ Num 3.0 /\ Num 0.0 )
        (\what reps offset ->
            Tex $ H.addModifier what $ H.geometry $ RepeatY { reps, offset }
        )


kaleid :: H.HydraTFn1M
kaleid =
    tryTV1M
        ( H.defaultTexture /\ Num 4.0 )
        (\what nSides ->
            Tex $ H.addModifier what $ H.geometry $ Kaleid { nSides }
        )


scrollX :: H.HydraTFn2M
scrollX =
    tryTV2M
        ( H.defaultTexture /\ Num 0.5 /\ Num 0.0 )
        (\what scrollX speed ->
            Tex $ H.addModifier what $ H.geometry $ ScrollX { scrollX, speed }
        )


scrollY :: H.HydraTFn2M
scrollY =
    tryTV2M
        ( H.defaultTexture /\ Num 0.5 /\ Num 0.0 )
        (\what scrollY speed ->
            Tex $ H.addModifier what $ H.geometry $ ScrollY { scrollY, speed }
        )


{- ====== Color ====== -}


posterize :: H.HydraTFn2M
posterize =
    tryTV2M
        ( H.defaultTexture /\ Num 3.0 /\ Num 0.6 )
        (\what bins gamma ->
            Tex $ H.addModifier what $ H.color $ Posterize { bins, gamma }
        )


shift :: H.HydraTFn4M
shift =
    tryTV4M
        ( H.defaultTexture /\ Num 0.5 /\ Num 0.5 /\ Num 0.5 /\ Num 0.5 )
        (\what r g b a ->
            Tex $ H.addModifier what $ H.color $ Shift { r, g, b, a }
        )


invert :: H.HydraTFn1M
invert =
    tryTV1M
        ( H.defaultTexture /\ Num 1.0 )
        (\what amount ->
            Tex $ H.addModifier what $ H.color $ Invert { amount }
        )


contrast :: H.HydraTFn1M
contrast =
    tryTV1M
        ( H.defaultTexture /\ Num 1.0 )
        (\what amount ->
            Tex $ H.addModifier what $ H.color $ Contrast { amount }
        )


brightness :: H.HydraTFn1M
brightness =
    tryTV1M
        ( H.defaultTexture /\ Num 1.0 )
        (\what amount ->
            Tex $ H.addModifier what $ H.color $ Brightness { amount }
        )


luma :: H.HydraTFn2M
luma =
    tryTV2M
        ( H.defaultTexture /\ Num 0.5 /\ Num 0.1 )
        (\what threshold tolerance ->
            Tex $ H.addModifier what $ H.color $ Luma { threshold, tolerance }
        )


thresh :: H.HydraTFn2M
thresh =
    tryTV2M
        ( H.defaultTexture /\ Num 0.5 /\ Num 0.04 )
        (\what threshold tolerance ->
            Tex $ H.addModifier what $ H.color $ Thresh { threshold, tolerance }
        )


color :: H.HydraTFn4M
color =
    tryTV4M
        ( H.defaultTexture /\ Num 0.0 /\ Num 0.0 /\ Num 0.0 /\ Num 1.0 )
        (\what r g b a ->
            Tex $ H.addModifier what $ H.color $ Color { r, g, b, a }
        )


saturate :: H.HydraTFn1M
saturate =
    tryTV1M
        ( H.defaultTexture /\ Num 1.0 )
        (\what amount ->
            Tex $ H.addModifier what $ H.color $ Saturate { amount }
        )


hue :: H.HydraTFn1M
hue =
    tryTV1M
        ( H.defaultTexture /\ Num 1.0 )
        (\what amount ->
            Tex $ H.addModifier what $ H.color $ Hue { amount }
        )


colorama :: H.HydraTFn1M
colorama =
    tryTV1M
        ( H.defaultTexture /\ Num 1.0 )
        (\what amount ->
            Tex $ H.addModifier what $ H.color $ Colorama { amount }
        )


{- ====== Blend ====== -}


add :: H.HydraTTFn1M
add =
    tryTTV1M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 0.5 )
        (\trg src amount ->
            Tex $ H.addModifier trg $ H.blend $ Add { what : src, amount }
        )


layer :: H.HydraTTFn0M
layer =
    tryTTM
        ( H.defaultTexture /\ H.defaultTexture )
        (\trg src ->
            Tex $ H.addModifier trg $ H.blend $ Layer { what : src }
        )


blend :: H.HydraTTFn1M
blend =
    tryTTV1M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 0.5 )
        (\trg src amount ->
            Tex $ H.addModifier trg $ H.blend $ Blend { what : src, amount }
        )


mult :: H.HydraTTFn1M
mult =
    tryTTV1M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 0.5 )
        (\trg src amount ->
            Tex $ H.addModifier trg $ H.blend $ Mult { what : src, amount }
        )


diff :: H.HydraTTFn0M
diff =
    tryTTM
        ( H.defaultTexture /\ H.defaultTexture )
        (\trg src ->
            Tex $ H.addModifier trg $ H.blend $ Diff { what : src }
        )


mask :: H.HydraTTFn2M
mask =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 3.0 /\ Num 0.5 )
        (\trg src reps offset ->
            Tex $ H.addModifier trg $ H.blend $ Mask { what : src, reps, offset }
        )


{- ====== Modulate ====== -}


modRepeat :: H.HydraTTFn4M
modRepeat =
    tryTTV4M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 3.0 /\ Num 3.0 /\ Num 0.5 /\ Num 0.5 )
        (\trg src repeatX repeatY offsetX offsetY ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateRepeat { what : src, repeatX, repeatY, offsetX, offsetY  }
        )


modRepeatX :: H.HydraTTFn2M
modRepeatX =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 3.0 /\ Num 0.5 )
        (\trg src reps offset ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateRepeatX { what : src, reps, offset }
        )


modRepeatY :: H.HydraTTFn2M
modRepeatY =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 3.0 /\ Num 0.5 )
        (\trg src reps offset ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateRepeatY { what : src, reps, offset }
        )


modKaleid :: H.HydraTTFn1M
modKaleid =
    tryTTV1M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 4.0 )
        (\trg src nSides ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateKaleid { what : src, nSides }
        )


modScrollX :: H.HydraTTFn2M
modScrollX =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 0.5 /\ Num 0.0 )
        (\trg src scrollX speed ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateScrollX { what : src, scrollX, speed }
        )


modScrollY :: H.HydraTTFn2M
modScrollY =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 0.5 /\ Num 0.0 )
        (\trg src scrollY speed ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateScrollY { what : src, scrollY, speed }
        )


modulate :: H.HydraTTFn1M
modulate =
    tryTTV1M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 0.1 )
        (\trg src amount ->
            Tex $ H.addModifier trg $ H.modulate $ Modulate { what : src, amount }
        )


modScale :: H.HydraTTFn2M
modScale =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 1.0 /\ Num 1.0 )
        (\trg src multiple offset ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateScale { what : src, multiple, offset }
        )


modPixelate :: H.HydraTTFn2M
modPixelate =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 10.0 /\ Num 3.0 )
        (\trg src multiple offset ->
            Tex $ H.addModifier trg $ H.modulate $ ModulatePixelate { what : src, multiple, offset }
        )


modRotate :: H.HydraTTFn2M
modRotate =
    tryTTV2M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 1.0 /\ Num 0.0 )
        (\trg src multiple offset ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateRotate { what : src, multiple, offset }
        )


modHue :: H.HydraTTFn1M
modHue =
    tryTTV1M
        ( H.defaultTexture /\ H.defaultTexture /\ Num 1.0 )
        (\trg src amount ->
            Tex $ H.addModifier trg $ H.modulate $ ModulateHue { what : src, amount }
        )


-- TODO: check if we can use `Foldable` / `Unfoldable`

tryV1 :: Value -> H.ToHydraFn1 -> H.HydraFn1
tryV1 = Fn.tryFn1 H.toValue


tryV1M :: Value -> H.ToHydraFn1 -> H.HydraFn1M
tryV1M v fn = Fn.tryFn1 ((=<<) H.toValue) v $ Just <<< fn


tryV2 :: (Value /\ Value) -> H.ToHydraFn2 -> H.HydraFn2
tryV2 = Fn.tryFn2 H.toValue


tryV2M :: (Value /\ Value) -> H.ToHydraFn2 -> H.HydraFn2M
tryV2M v fn = Fn.tryFn2 ((=<<) H.toValue) v $ \v1 v2 -> Just $ fn v1 v2


tryV3 :: (Value /\ Value /\ Value) -> H.ToHydraFn3 -> H.HydraFn3
tryV3 = Fn.tryFn3 H.toValue


tryV3M :: (Value /\ Value /\ Value) -> H.ToHydraFn3 -> H.HydraFn3M
tryV3M v fn = Fn.tryFn3 ((=<<) H.toValue) v $ \v1 v2 v3 -> Just $ fn v1 v2 v3


tryV4 :: (Value /\ Value /\ Value /\ Value) -> H.ToHydraFn4 -> H.HydraFn4
tryV4 = Fn.tryFn4 H.toValue


tryV4M :: (Value /\ Value /\ Value /\ Value) -> H.ToHydraFn4 -> H.HydraFn4M
tryV4M v fn = Fn.tryFn4 ((=<<) H.toValue) v $ \v1 v2 v3 v4 -> Just $ fn v1 v2 v3 v4


tryV5 :: (Value /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraFn5 -> H.HydraFn5
tryV5 = Fn.tryFn5 H.toValue


tryV5M :: (Value /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraFn5 -> H.HydraFn5M
tryV5M v fn = Fn.tryFn5 ((=<<) H.toValue) v $ \v1 v2 v3 v4 v5 -> Just $ fn v1 v2 v3 v4 v5


tryT :: Texture -> H.ToHydraTFn0 -> H.HydraTFn0
tryT _ f (Tex t) = f t
tryT t f _       = f t


tryTV1 :: (Texture /\ Value) -> H.ToHydraTFn1 -> H.HydraTFn1
tryTV1 (_ /\ v) f (Tex t) = tryV1 v $ f t
tryTV1 (t /\ v) f _       = tryV1 v $ f t


tryTV2 :: (Texture /\ Value /\ Value) -> H.ToHydraTFn2 -> H.HydraTFn2
tryTV2 (_ /\ v1 /\ v2) f (Tex t) = tryV2 (v1 /\ v2) $ f t
tryTV2 (t /\ v1 /\ v2) f _       = tryV2 (v1 /\ v2) $ f t


tryTV3 :: (Texture /\ Value /\ Value /\ Value) -> H.ToHydraTFn3 -> H.HydraTFn3
tryTV3 (_ /\ v1 /\ v2 /\ v3) f (Tex t) = tryV3 (v1 /\ v2 /\ v3) $ f t
tryTV3 (t /\ v1 /\ v2 /\ v3) f _       = tryV3 (v1 /\ v2 /\ v3) $ f t


tryTV4 :: (Texture /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraTFn4 -> H.HydraTFn4
tryTV4 (_ /\ v1 /\ v2 /\ v3 /\ v4) f (Tex t) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f t
tryTV4 (t /\ v1 /\ v2 /\ v3 /\ v4) f _       = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f t


tryTV5 :: (Texture /\ Value /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraTFn5 -> H.HydraTFn5
tryTV5 (_ /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f (Tex t) = tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f t
tryTV5 (t /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f _       = tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f t


tryTM :: Texture -> H.ToHydraTFn0 -> H.HydraTFn0M
tryTM _ f (Just (Tex t)) = Just $ f t
tryTM t f _              = Just $ f t


tryTV1M :: (Texture /\ Value) -> H.ToHydraTFn1 -> H.HydraTFn1M
tryTV1M (_ /\ v) f (Just (Tex t)) = tryV1M v $ f t
tryTV1M (t /\ v) f _              = tryV1M v $ f t


tryTV2M :: (Texture /\ Value /\ Value) -> H.ToHydraTFn2 -> H.HydraTFn2M
tryTV2M (_ /\ v1 /\ v2) f (Just (Tex t)) = tryV2M (v1 /\ v2) $ f t
tryTV2M (t /\ v1 /\ v2) f _              = tryV2M (v1 /\ v2) $ f t


tryTV3M :: (Texture /\ Value /\ Value /\ Value) -> H.ToHydraTFn3 -> H.HydraTFn3M
tryTV3M (_ /\ v1 /\ v2 /\ v3) f (Just (Tex t)) = tryV3M (v1 /\ v2 /\ v3) $ f t
tryTV3M (t /\ v1 /\ v2 /\ v3) f _              = tryV3M (v1 /\ v2 /\ v3) $ f t


tryTV4M :: (Texture /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraTFn4 -> H.HydraTFn4M
tryTV4M (_ /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Tex t)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f t
tryTV4M (t /\ v1 /\ v2 /\ v3 /\ v4) f _              = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f t


tryTV5M :: (Texture /\ Value /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraTFn5 -> H.HydraTFn5M
tryTV5M (_ /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f (Just (Tex t)) = tryV5M (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f t
tryTV5M (t /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f _              = tryV5M (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f t


tryTT :: (Texture /\ Texture) -> H.ToHydraTTFn0 -> H.HydraEEFn0
tryTT ( _ /\ _ ) f (Tex t1) (Tex t2) = f t1 t2
tryTT ( _ /\ t2) f (Tex t1) _        = f t1 t2
tryTT (t1 /\ _ ) f _        (Tex t2) = f t1 t2
tryTT (t1 /\ t2) f _        _        = f t1 t2


tryTTV1 :: (Texture /\ Texture /\ Value) -> H.ToHydraTTFn1 -> H.HydraEEFn1
tryTTV1 ( _ /\ _  /\ v) f (Tex t1) (Tex t2) = tryV1 v $ f t1 t2
tryTTV1 ( _ /\ t2 /\ v) f (Tex t1) _        = tryV1 v $ f t1 t2
tryTTV1 (t1 /\ _  /\ v) f _        (Tex t2) = tryV1 v $ f t1 t2
tryTTV1 (t1 /\ t2 /\ v) f _        _        = tryV1 v $ f t1 t2


tryTTV2 :: (Texture /\ Texture /\ Value /\ Value) -> H.ToHydraTTFn2 -> H.HydraEEFn2
tryTTV2 ( _ /\ _  /\ v1 /\ v2) f (Tex t1) (Tex t2) = tryV2 (v1 /\ v2) $ f t1 t2
tryTTV2 ( _ /\ t2 /\ v1 /\ v2) f (Tex t1) _        = tryV2 (v1 /\ v2) $ f t1 t2
tryTTV2 (t1 /\ _  /\ v1 /\ v2) f _        (Tex t2) = tryV2 (v1 /\ v2) $ f t1 t2
tryTTV2 (t1 /\ t2 /\ v1 /\ v2) f _        _        = tryV2 (v1 /\ v2) $ f t1 t2


tryTTV3 :: (Texture /\ Texture /\ Value /\ Value /\ Value) -> H.ToHydraTTFn3 -> H.HydraEEFn3
tryTTV3 ( _ /\ _  /\ v1 /\ v2 /\ v3) f (Tex t1) (Tex t2) = tryV3 (v1 /\ v2 /\ v3) $ f t1 t2
tryTTV3 ( _ /\ t2 /\ v1 /\ v2 /\ v3) f (Tex t1) _        = tryV3 (v1 /\ v2 /\ v3) $ f t1 t2
tryTTV3 (t1 /\ _  /\ v1 /\ v2 /\ v3) f _        (Tex t2) = tryV3 (v1 /\ v2 /\ v3) $ f t1 t2
tryTTV3 (t1 /\ t2 /\ v1 /\ v2 /\ v3) f _        _        = tryV3 (v1 /\ v2 /\ v3) $ f t1 t2


tryTTV4 :: (Texture /\ Texture /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraTTFn4 -> H.HydraEEFn4
tryTTV4 ( _ /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f (Tex t1) (Tex t2) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
tryTTV4 ( _ /\ t2 /\ v1 /\ v2 /\ v3 /\ v4) f (Tex t1) _        = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
tryTTV4 (t1 /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f _        (Tex t2) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
tryTTV4 (t1 /\ t2 /\ v1 /\ v2 /\ v3 /\ v4) f _        _        = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2


tryTTM :: (Texture /\ Texture) -> H.ToHydraTTFn0 -> H.HydraTTFn0M
tryTTM ( _ /\ _ ) f (Just (Tex t1)) (Just (Tex t2)) = Just $ f t1 t2
tryTTM ( _ /\ t2) f (Just (Tex t1)) _               = Just $ f t1 t2
tryTTM (t1 /\ _ ) f _               (Just (Tex t2)) = Just $ f t1 t2
tryTTM (t1 /\ t2) f _               _               = Just $ f t1 t2


tryTTV1M :: (Texture /\ Texture /\ Value) -> H.ToHydraTTFn1 -> H.HydraTTFn1M
tryTTV1M ( _ /\ _  /\ v) f (Just (Tex t1)) (Just (Tex t2)) = tryV1M v $ f t1 t2
tryTTV1M ( _ /\ t2 /\ v) f (Just (Tex t1)) _               = tryV1M v $ f t1 t2
tryTTV1M (t1 /\ _  /\ v) f _               (Just (Tex t2)) = tryV1M v $ f t1 t2
tryTTV1M (t1 /\ t2 /\ v) f _               _               = tryV1M v $ f t1 t2


tryTTV2M :: (Texture /\ Texture /\ Value /\ Value) -> H.ToHydraTTFn2 -> H.HydraTTFn2M
tryTTV2M ( _ /\ _  /\ v1 /\ v2) f (Just (Tex t1)) (Just (Tex t2)) = tryV2M (v1 /\ v2) $ f t1 t2
tryTTV2M ( _ /\ t2 /\ v1 /\ v2) f (Just (Tex t1)) _               = tryV2M (v1 /\ v2) $ f t1 t2
tryTTV2M (t1 /\ _  /\ v1 /\ v2) f _               (Just (Tex t2)) = tryV2M (v1 /\ v2) $ f t1 t2
tryTTV2M (t1 /\ t2 /\ v1 /\ v2) f _               _               = tryV2M (v1 /\ v2) $ f t1 t2


tryTTV3M :: (Texture /\ Texture /\ Value /\ Value /\ Value) -> H.ToHydraTTFn3 -> H.HydraTTFn3M
tryTTV3M ( _ /\ _  /\ v1 /\ v2 /\ v3) f (Just (Tex t1)) (Just (Tex t2)) = tryV3M (v1 /\ v2 /\ v3) $ f t1 t2
tryTTV3M ( _ /\ t2 /\ v1 /\ v2 /\ v3) f (Just (Tex t1)) _               = tryV3M (v1 /\ v2 /\ v3) $ f t1 t2
tryTTV3M (t1 /\ _  /\ v1 /\ v2 /\ v3) f _               (Just (Tex t2)) = tryV3M (v1 /\ v2 /\ v3) $ f t1 t2
tryTTV3M (t1 /\ t2 /\ v1 /\ v2 /\ v3) f _               _               = tryV3M (v1 /\ v2 /\ v3) $ f t1 t2


tryTTV4M :: (Texture /\ Texture /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraTTFn4 -> H.HydraTTFn4M
tryTTV4M ( _ /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Tex t1)) (Just (Tex t2)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
tryTTV4M ( _ /\ t2 /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Tex t1)) _               = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
tryTTV4M (t1 /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f _               (Just (Tex t2)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
tryTTV4M (t1 /\ t2 /\ v1 /\ v2 /\ v3 /\ v4) f _               _               = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f t1 t2
