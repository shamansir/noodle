module Hydra.Try where

import Prelude (($), unit, (>>>), (<<<), (>>=), (=<<), (<$>), (#))

-- import Data.Vec as Vec
-- import Data.Vec (Vec)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))



import Hydra
    ( Hydra(..), Value(..), Output(..), Entity
    , Source(..), Geometry(..), Blend(..), Color(..), Modulate(..)
    )
import Hydra as H
import Hydra.Fn as Fn


{- ====== Source ====== -}

noise :: H.HydraFn2M
noise =
    tryV2M
        ( Num 10.0 /\ Num 0.1 )
        (\scale offset -> Hydra $ H.entityOf $ Noise { scale, offset })


voronoi :: H.HydraFn3M
voronoi =
    tryV3M
        ( Num 5.0 /\ Num 0.3 /\ Num 0.3 )
        (\scale speed blending -> Hydra $ H.entityOf $ Voronoi { scale, speed, blending })


osc :: H.HydraFn3M
osc =
    tryV3M
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Hydra $ H.entityOf $ Osc { freq, sync, offset })


{- osc' :: HydraFn3
osc' =
    tryV3
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Hydra $ entityOf $ Osc { freq, sync, offset }) -}


shape :: H.HydraFn3M
shape =
    tryV3M
        ( Num 3.0 /\ Num 0.3 /\ Num 0.01 )
        (\sides radius smoothing -> Hydra $ H.entityOf $ Shape { sides, radius, smoothing })


gradient :: H.HydraFn1M
gradient =
    tryV1M
        ( Width )
        (\speed -> Hydra $ H.entityOf $ Gradient { speed })


solid :: H.HydraFn4M
solid =
    tryV4M
        ( Num 0.0 /\ Num 0.0 /\ Num 0.0 /\ Num 1.0 )
        (\r g b a -> Hydra $ H.entityOf $ Solid { r, g, b, a })


{- ====== Geometry ====== -}


rotate :: H.HydraEFn2M
rotate =
    tryEV2M
        ( H.defaultEntity /\ Num 10.0 /\ Num 0.0 )
        (\what angle speed ->
            Hydra $ H.addModifier what $ H.geometry $ Rotate { angle, speed }
        )


scale :: H.HydraEFn5M
scale =
    tryEV5M
        ( H.defaultEntity /\ Num 1.5 /\ Num 1.0 /\ Num 1.0 /\ Num 0.5 /\ Num 0.5 )
        (\what amount xMult yMult offsetX offsetY ->
            Hydra $ H.addModifier what $ H.geometry $ Scale { amount, xMult, yMult, offsetX, offsetY }
        )


pixelate :: H.HydraEFn2M
pixelate =
    tryEV2M
        ( H.defaultEntity /\ Num 20.0 /\ Num 20.0 )
        (\what pixelX pixelY ->
            Hydra $ H.addModifier what $ H.geometry $ Pixelate { pixelX, pixelY }
        )


repeat :: H.HydraEFn4M
repeat =
    tryEV4M
        ( H.defaultEntity /\ Num 3.0 /\ Num 3.0 /\ Num 0.0 /\ Num 0.0 )
        (\what repeatX repeatY offsetX offsetY ->
            Hydra $ H.addModifier what $ H.geometry $ Repeat { repeatX, repeatY, offsetX, offsetY }
        )


repeatX :: H.HydraEFn2M
repeatX =
    tryEV2M
        ( H.defaultEntity /\ Num 3.0 /\ Num 0.0 )
        (\what reps offset ->
            Hydra $ H.addModifier what $ H.geometry $ RepeatX { reps, offset }
        )


repeatY :: H.HydraEFn2M
repeatY =
    tryEV2M
        ( H.defaultEntity /\ Num 3.0 /\ Num 0.0 )
        (\what reps offset ->
            Hydra $ H.addModifier what $ H.geometry $ RepeatY { reps, offset }
        )


kaleid :: H.HydraEFn1M
kaleid =
    tryEV1M
        ( H.defaultEntity /\ Num 4.0 )
        (\what nSides ->
            Hydra $ H.addModifier what $ H.geometry $ Kaleid { nSides }
        )


scrollX :: H.HydraEFn2M
scrollX =
    tryEV2M
        ( H.defaultEntity /\ Num 0.5 /\ Num 0.0 )
        (\what scrollX speed ->
            Hydra $ H.addModifier what $ H.geometry $ ScrollX { scrollX, speed }
        )


scrollY :: H.HydraEFn2M
scrollY =
    tryEV2M
        ( H.defaultEntity /\ Num 0.5 /\ Num 0.0 )
        (\what scrollY speed ->
            Hydra $ H.addModifier what $ H.geometry $ ScrollY { scrollY, speed }
        )


{- ====== Color ====== -}


posterize :: H.HydraEFn2M
posterize =
    tryEV2M
        ( H.defaultEntity /\ Num 3.0 /\ Num 0.6 )
        (\what bins gamma ->
            Hydra $ H.addModifier what $ H.color $ Posterize { bins, gamma }
        )


shift :: H.HydraEFn4M
shift =
    tryEV4M
        ( H.defaultEntity /\ Num 0.5 /\ Num 0.5 /\ Num 0.5 /\ Num 0.5 )
        (\what r g b a ->
            Hydra $ H.addModifier what $ H.color $ Shift { r, g, b, a }
        )


invert :: H.HydraEFn1M
invert =
    tryEV1M
        ( H.defaultEntity /\ Num 1.0 )
        (\what amount ->
            Hydra $ H.addModifier what $ H.color $ Invert { amount }
        )


contrast :: H.HydraEFn1M
contrast =
    tryEV1M
        ( H.defaultEntity /\ Num 1.0 )
        (\what amount ->
            Hydra $ H.addModifier what $ H.color $ Contrast { amount }
        )


brightness :: H.HydraEFn1M
brightness =
    tryEV1M
        ( H.defaultEntity /\ Num 1.0 )
        (\what amount ->
            Hydra $ H.addModifier what $ H.color $ Brightness { amount }
        )


luma :: H.HydraEFn2M
luma =
    tryEV2M
        ( H.defaultEntity /\ Num 0.5 /\ Num 0.1 )
        (\what treshold tolerance ->
            Hydra $ H.addModifier what $ H.color $ Luma { treshold, tolerance }
        )


tresh :: H.HydraEFn2M
tresh =
    tryEV2M
        ( H.defaultEntity /\ Num 0.5 /\ Num 0.04 )
        (\what treshold tolerance ->
            Hydra $ H.addModifier what $ H.color $ Tresh { treshold, tolerance }
        )


color :: H.HydraEFn4M
color =
    tryEV4M
        ( H.defaultEntity /\ Num 0.0 /\ Num 0.0 /\ Num 0.0 /\ Num 1.0 )
        (\what r g b a ->
            Hydra $ H.addModifier what $ H.color $ Color { r, g, b, a }
        )


saturate :: H.HydraEFn1M
saturate =
    tryEV1M
        ( H.defaultEntity /\ Num 1.0 )
        (\what amount ->
            Hydra $ H.addModifier what $ H.color $ Saturate { amount }
        )


hue :: H.HydraEFn1M
hue =
    tryEV1M
        ( H.defaultEntity /\ Num 1.0 )
        (\what amount ->
            Hydra $ H.addModifier what $ H.color $ Hue { amount }
        )


colorama :: H.HydraEFn1M
colorama =
    tryEV1M
        ( H.defaultEntity /\ Num 1.0 )
        (\what amount ->
            Hydra $ H.addModifier what $ H.color $ Colorama { amount }
        )


{- ====== Blend ====== -}


add :: H.HydraEEFn1M
add =
    tryEEV1M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 0.5 )
        (\trg src amount ->
            Hydra $ H.addModifier trg $ H.blend $ Add { what : src, amount }
        )


layer :: H.HydraEEFn0M
layer =
    tryEEM
        ( H.defaultEntity /\ H.defaultEntity )
        (\trg src ->
            Hydra $ H.addModifier trg $ H.blend $ Layer { what : src }
        )


blend :: H.HydraEEFn1M
blend =
    tryEEV1M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 0.5 )
        (\trg src amount ->
            Hydra $ H.addModifier trg $ H.blend $ Blend { what : src, amount }
        )


mult :: H.HydraEEFn1M
mult =
    tryEEV1M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 0.5 )
        (\trg src amount ->
            Hydra $ H.addModifier trg $ H.blend $ Mult { what : src, amount }
        )


diff :: H.HydraEEFn0M
diff =
    tryEEM
        ( H.defaultEntity /\ H.defaultEntity )
        (\trg src ->
            Hydra $ H.addModifier trg $ H.blend $ Diff { what : src }
        )


mask :: H.HydraEEFn0M
mask =
    tryEEM
        ( H.defaultEntity /\ H.defaultEntity )
        (\trg src ->
            Hydra $ H.addModifier trg $ H.blend $ Mask { what : src }
        )


{- ====== Modulate ====== -}


modRepeat :: H.HydraEEFn4M
modRepeat =
    tryEEV4M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 3.0 /\ Num 3.0 /\ Num 0.5 /\ Num 0.5 )
        (\trg src repeatX repeatY offsetX offsetY ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateRepeat { what : src, repeatX, repeatY, offsetX, offsetY  }
        )


modRepeatX :: H.HydraEEFn2M
modRepeatX =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 3.0 /\ Num 0.5 )
        (\trg src reps offset ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateRepeatX { what : src, reps, offset }
        )


modRepeatY :: H.HydraEEFn2M
modRepeatY =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 3.0 /\ Num 0.5 )
        (\trg src reps offset ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateRepeatY { what : src, reps, offset }
        )


modKaleid :: H.HydraEEFn1M
modKaleid =
    tryEEV1M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 4.0 )
        (\trg src nSides ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateKaleid { what : src, nSides }
        )


modScrollX :: H.HydraEEFn2M
modScrollX =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 0.5 /\ Num 0.0 )
        (\trg src scrollX speed ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateScrollX { what : src, scrollX, speed }
        )


modScrollY :: H.HydraEEFn2M
modScrollY =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 0.5 /\ Num 0.0 )
        (\trg src scrollY speed ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateScrollY { what : src, scrollY, speed }
        )


modulate :: H.HydraEEFn1M
modulate =
    tryEEV1M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 0.1 )
        (\trg src amount ->
            Hydra $ H.addModifier trg $ H.modulate $ Modulate { what : src, amount }
        )


modScale :: H.HydraEEFn2M
modScale =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 1.0 /\ Num 1.0 )
        (\trg src multiple offset ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateScale { what : src, multiple, offset }
        )


modPixelate :: H.HydraEEFn2M
modPixelate =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 10.0 /\ Num 3.0 )
        (\trg src multiple offset ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulatePixelate { what : src, multiple, offset }
        )


modRotate :: H.HydraEEFn2M
modRotate =
    tryEEV2M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 1.0 /\ Num 0.0 )
        (\trg src multiple offset ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateRotate { what : src, multiple, offset }
        )


modHue :: H.HydraEEFn1M
modHue =
    tryEEV1M
        ( H.defaultEntity /\ H.defaultEntity /\ Num 1.0 )
        (\trg src amount ->
            Hydra $ H.addModifier trg $ H.modulate $ ModulateHue { what : src, amount }
        )



{- ====== Out ====== -}

out :: Int -> H.HydraEFn0
out n =
    tryE H.defaultEntity (\entity -> Out [ entity /\ Output n ])



out' :: H.HydraEFn0
out' =
    tryE H.defaultEntity (\entity -> Out [ entity /\ Default ])



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


tryE :: Entity -> H.ToHydraEFn0 -> H.HydraEFn0
tryE _ f (Hydra e) = f e
tryE e f _ = f e


tryEV1 :: (Entity /\ Value) -> H.ToHydraEFn1 -> H.HydraEFn1
tryEV1 (_ /\ v) f (Hydra e) = tryV1 v $ f e
tryEV1 (e /\ v) f _         = tryV1 v $ f e


tryEV2 :: (Entity /\ Value /\ Value) -> H.ToHydraEFn2 -> H.HydraEFn2
tryEV2 (_ /\ v1 /\ v2) f (Hydra e) = tryV2 (v1 /\ v2) $ f e
tryEV2 (e /\ v1 /\ v2) f _         = tryV2 (v1 /\ v2) $ f e


tryEV3 :: (Entity /\ Value /\ Value /\ Value) -> H.ToHydraEFn3 -> H.HydraEFn3
tryEV3 (_ /\ v1 /\ v2 /\ v3) f (Hydra e) = tryV3 (v1 /\ v2 /\ v3) $ f e
tryEV3 (e /\ v1 /\ v2 /\ v3) f _         = tryV3 (v1 /\ v2 /\ v3) $ f e


tryEV4 :: (Entity /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraEFn4 -> H.HydraEFn4
tryEV4 (_ /\ v1 /\ v2 /\ v3 /\ v4) f (Hydra e) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e
tryEV4 (e /\ v1 /\ v2 /\ v3 /\ v4) f _         = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e


tryEV5 :: (Entity /\ Value /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraEFn5 -> H.HydraEFn5
tryEV5 (_ /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f (Hydra e) = tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e
tryEV5 (e /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f _         = tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e


tryEM :: Entity -> H.ToHydraEFn0 -> H.HydraEFn0M
tryEM _ f (Just (Hydra e)) = Just $ f e
tryEM e f _ = Just $ f e


tryEV1M :: (Entity /\ Value) -> H.ToHydraEFn1 -> H.HydraEFn1M
tryEV1M (_ /\ v) f (Just (Hydra e)) = tryV1M v $ f e
tryEV1M (e /\ v) f _                = tryV1M v $ f e


tryEV2M :: (Entity /\ Value /\ Value) -> H.ToHydraEFn2 -> H.HydraEFn2M
tryEV2M (_ /\ v1 /\ v2) f (Just (Hydra e)) = tryV2M (v1 /\ v2) $ f e
tryEV2M (e /\ v1 /\ v2) f _                = tryV2M (v1 /\ v2) $ f e


tryEV3M :: (Entity /\ Value /\ Value /\ Value) -> H.ToHydraEFn3 -> H.HydraEFn3M
tryEV3M (_ /\ v1 /\ v2 /\ v3) f (Just (Hydra e)) = tryV3M (v1 /\ v2 /\ v3) $ f e
tryEV3M (e /\ v1 /\ v2 /\ v3) f _                = tryV3M (v1 /\ v2 /\ v3) $ f e


tryEV4M :: (Entity /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraEFn4 -> H.HydraEFn4M
tryEV4M (_ /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Hydra e)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e
tryEV4M (e /\ v1 /\ v2 /\ v3 /\ v4) f _                = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e


tryEV5M :: (Entity /\ Value /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraEFn5 -> H.HydraEFn5M
tryEV5M (_ /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f (Just (Hydra e)) = tryV5M (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e
tryEV5M (e /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f _                = tryV5M (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e


tryEE :: (Entity /\ Entity) -> H.ToHydraEEFn0 -> H.HydraEEFn0
tryEE ( _ /\ _ ) f (Hydra e1) (Hydra e2) = f e1 e2
tryEE ( _ /\ e2) f (Hydra e1) _          = f e1 e2
tryEE (e1 /\ _ ) f _          (Hydra e2) = f e1 e2
tryEE (e1 /\ e2) f _          _          = f e1 e2


tryEEV1 :: (Entity /\ Entity /\ Value) -> H.ToHydraEEFn1 -> H.HydraEEFn1
tryEEV1 ( _ /\ _  /\ v) f (Hydra e1) (Hydra e2) = tryV1 v $ f e1 e2
tryEEV1 ( _ /\ e2 /\ v) f (Hydra e1) _          = tryV1 v $ f e1 e2
tryEEV1 (e1 /\ _  /\ v) f _          (Hydra e2) = tryV1 v $ f e1 e2
tryEEV1 (e1 /\ e2 /\ v) f _          _          = tryV1 v $ f e1 e2


tryEEV2 :: (Entity /\ Entity /\ Value /\ Value) -> H.ToHydraEEFn2 -> H.HydraEEFn2
tryEEV2 ( _ /\ _  /\ v1 /\ v2) f (Hydra e1) (Hydra e2) = tryV2 (v1 /\ v2) $ f e1 e2
tryEEV2 ( _ /\ e2 /\ v1 /\ v2) f (Hydra e1) _          = tryV2 (v1 /\ v2) $ f e1 e2
tryEEV2 (e1 /\ _  /\ v1 /\ v2) f _          (Hydra e2) = tryV2 (v1 /\ v2) $ f e1 e2
tryEEV2 (e1 /\ e2 /\ v1 /\ v2) f _          _          = tryV2 (v1 /\ v2) $ f e1 e2


tryEEV3 :: (Entity /\ Entity /\ Value /\ Value /\ Value) -> H.ToHydraEEFn3 -> H.HydraEEFn3
tryEEV3 ( _ /\ _  /\ v1 /\ v2 /\ v3) f (Hydra e1) (Hydra e2) = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3 ( _ /\ e2 /\ v1 /\ v2 /\ v3) f (Hydra e1) _          = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3 (e1 /\ _  /\ v1 /\ v2 /\ v3) f _          (Hydra e2) = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3 (e1 /\ e2 /\ v1 /\ v2 /\ v3) f _          _          = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2


tryEEV4 :: (Entity /\ Entity /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraEEFn4 -> H.HydraEEFn4
tryEEV4 ( _ /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f (Hydra e1) (Hydra e2) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4 ( _ /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f (Hydra e1) _          = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4 (e1 /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f _          (Hydra e2) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4 (e1 /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f _          _          = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2


tryEEM :: (Entity /\ Entity) -> H.ToHydraEEFn0 -> H.HydraEEFn0M
tryEEM ( _ /\ _ ) f (Just (Hydra e1)) (Just (Hydra e2)) = Just $ f e1 e2
tryEEM ( _ /\ e2) f (Just (Hydra e1)) _                 = Just $ f e1 e2
tryEEM (e1 /\ _ ) f _                 (Just (Hydra e2)) = Just $ f e1 e2
tryEEM (e1 /\ e2) f _                 _                 = Just $ f e1 e2


tryEEV1M :: (Entity /\ Entity /\ Value) -> H.ToHydraEEFn1 -> H.HydraEEFn1M
tryEEV1M ( _ /\ _  /\ v) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV1M v $ f e1 e2
tryEEV1M ( _ /\ e2 /\ v) f (Just (Hydra e1)) _                 = tryV1M v $ f e1 e2
tryEEV1M (e1 /\ _  /\ v) f _                 (Just (Hydra e2)) = tryV1M v $ f e1 e2
tryEEV1M (e1 /\ e2 /\ v) f _                 _                 = tryV1M v $ f e1 e2


tryEEV2M :: (Entity /\ Entity /\ Value /\ Value) -> H.ToHydraEEFn2 -> H.HydraEEFn2M
tryEEV2M ( _ /\ _  /\ v1 /\ v2) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV2M (v1 /\ v2) $ f e1 e2
tryEEV2M ( _ /\ e2 /\ v1 /\ v2) f (Just (Hydra e1)) _                 = tryV2M (v1 /\ v2) $ f e1 e2
tryEEV2M (e1 /\ _  /\ v1 /\ v2) f _                 (Just (Hydra e2)) = tryV2M (v1 /\ v2) $ f e1 e2
tryEEV2M (e1 /\ e2 /\ v1 /\ v2) f _                 _                 = tryV2M (v1 /\ v2) $ f e1 e2


tryEEV3M :: (Entity /\ Entity /\ Value /\ Value /\ Value) -> H.ToHydraEEFn3 -> H.HydraEEFn3M
tryEEV3M ( _ /\ _  /\ v1 /\ v2 /\ v3) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3M ( _ /\ e2 /\ v1 /\ v2 /\ v3) f (Just (Hydra e1)) _                 = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3M (e1 /\ _  /\ v1 /\ v2 /\ v3) f _                 (Just (Hydra e2)) = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3M (e1 /\ e2 /\ v1 /\ v2 /\ v3) f _                 _                 = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2


tryEEV4M :: (Entity /\ Entity /\ Value /\ Value /\ Value /\ Value) -> H.ToHydraEEFn4 -> H.HydraEEFn4M
tryEEV4M ( _ /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4M ( _ /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Hydra e1)) _                 = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4M (e1 /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f _                 (Just (Hydra e2)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4M (e1 /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f _                 _                 = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
