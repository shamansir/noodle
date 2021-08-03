module Hydra.Try where

import Prelude (($), unit, (>>>), (<<<), (>>=), (=<<), (<$>), (#))

-- import Data.Vec as Vec
-- import Data.Vec (Vec)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))



import Hydra
import Hydra.Fn as Fn


{- ====== Source ====== -}

noise :: HydraFn2M
noise =
    tryV2M
        ( Num 10.0 /\ Num 0.1 )
        (\scale offset -> Hydra $ entityOf $ Noise { scale, offset })


voronoi :: HydraFn3M
voronoi =
    tryV3M
        ( Num 5.0 /\ Num 0.3 /\ Num 0.3 )
        (\scale speed blending -> Hydra $ entityOf $ Voronoi { scale, speed, blending })


osc :: HydraFn3M
osc =
    tryV3M
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Hydra $ entityOf $ Osc { freq, sync, offset })


{- osc' :: HydraFn3
osc' =
    tryV3
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Hydra $ entityOf $ Osc { freq, sync, offset }) -}


shape :: HydraFn3M
shape =
    tryV3M
        ( Num 3.0 /\ Num 0.3 /\ Num 0.01 )
        (\sides radius smoothing -> Hydra $ entityOf $ Shape { sides, radius, smoothing })


gradient :: HydraFn1M
gradient =
    tryV1M
        ( X )
        (\speed -> Hydra $ entityOf $ Gradient { speed })


solid :: HydraFn4M
solid =
    tryV4M
        ( Num 0.0 /\ Num 0.0 /\ Num 0.0 /\ Num 1.0 )
        (\r g b a -> Hydra $ entityOf $ Solid { r, g, b, a })


{- ====== Geometry ====== -}


rotate :: HydraEFn2M
rotate =
    tryEV2M
        ( defaultEntity /\ Num 10.0 /\ Num 0.0 )
        (\what angle speed ->
            Hydra $ addModifier what $ geometry $ Rotate { angle, speed }
        )


scale :: HydraEFn5M
scale =
    tryEV5M
        ( defaultEntity /\ Num 1.5 /\ Num 1.0 /\ Num 1.0 /\ Num 0.5 /\ Num 0.5 )
        (\what amount xMult yMult offsetX offsetY ->
            Hydra $ addModifier what $ geometry $ Scale { amount, xMult, yMult, offsetX, offsetY }
        )


pixelate :: HydraEFn2M
pixelate =
    tryEV2M
        ( defaultEntity /\ Num 20.0 /\ Num 20.0 )
        (\what pixelX pixelY ->
            Hydra $ addModifier what $ geometry $ Pixelate { pixelX, pixelY }
        )


repeat :: HydraEFn4M
repeat =
    tryEV4M
        ( defaultEntity /\ Num 3.0 /\ Num 3.0 /\ Num 0.0 /\ Num 0.0 )
        (\what repeatX repeatY offsetX offsetY ->
            Hydra $ addModifier what $ geometry $ Repeat { repeatX, repeatY, offsetX, offsetY }
        )


repeatX :: HydraEFn2M
repeatX =
    tryEV2M
        ( defaultEntity /\ Num 3.0 /\ Num 0.0 )
        (\what reps offset ->
            Hydra $ addModifier what $ geometry $ RepeatX { reps, offset }
        )


repeatY :: HydraEFn2M
repeatY =
    tryEV2M
        ( defaultEntity /\ Num 3.0 /\ Num 0.0 )
        (\what reps offset ->
            Hydra $ addModifier what $ geometry $ RepeatY { reps, offset }
        )


kaleid :: HydraEFn1M
kaleid =
    tryEV1M
        ( defaultEntity /\ Num 4.0 )
        (\what nSides ->
            Hydra $ addModifier what $ geometry $ Kaleid { nSides }
        )


scrollX :: HydraEFn2M
scrollX =
    tryEV2M
        ( defaultEntity /\ Num 0.5 /\ Num 0.0 )
        (\what scrollX speed ->
            Hydra $ addModifier what $ geometry $ ScrollX { scrollX, speed }
        )


scrollY :: HydraEFn2M
scrollY =
    tryEV2M
        ( defaultEntity /\ Num 0.5 /\ Num 0.0 )
        (\what scrollY speed ->
            Hydra $ addModifier what $ geometry $ ScrollY { scrollY, speed }
        )


{- ====== Color ====== -}



{- ====== Blend ====== -}



{- ====== Modulate ====== -}



{- ====== Out ====== -}

out :: Int -> HydraEFn0
out n =
    tryE defaultEntity (\entity -> Out [ entity /\ Output n ])



out' :: HydraEFn0
out' =
    tryE defaultEntity (\entity -> Out [ entity /\ Default ])



-- TODO: check if we can use `Foldable` / `Unfoldable`

tryV1 :: Value -> ToHydraFn1 -> HydraFn1
tryV1 = Fn.tryFn1 toValue


tryV1M :: Value -> ToHydraFn1 -> HydraFn1M
tryV1M v fn = Fn.tryFn1 ((=<<) toValue) v $ Just <<< fn


tryV2 :: (Value /\ Value) -> ToHydraFn2 -> HydraFn2
tryV2 = Fn.tryFn2 toValue


tryV2M :: (Value /\ Value) -> ToHydraFn2 -> HydraFn2M
tryV2M v fn = Fn.tryFn2 ((=<<) toValue) v $ \v1 v2 -> Just $ fn v1 v2


tryV3 :: (Value /\ Value /\ Value) -> ToHydraFn3 -> HydraFn3
tryV3 = Fn.tryFn3 toValue


tryV3M :: (Value /\ Value /\ Value) -> ToHydraFn3 -> HydraFn3M
tryV3M v fn = Fn.tryFn3 ((=<<) toValue) v $ \v1 v2 v3 -> Just $ fn v1 v2 v3


tryV4 :: (Value /\ Value /\ Value /\ Value) -> ToHydraFn4 -> HydraFn4
tryV4 = Fn.tryFn4 toValue


tryV4M :: (Value /\ Value /\ Value /\ Value) -> ToHydraFn4 -> HydraFn4M
tryV4M v fn = Fn.tryFn4 ((=<<) toValue) v $ \v1 v2 v3 v4 -> Just $ fn v1 v2 v3 v4


tryV5 :: (Value /\ Value /\ Value /\ Value /\ Value) -> ToHydraFn5 -> HydraFn5
tryV5 = Fn.tryFn5 toValue


tryV5M :: (Value /\ Value /\ Value /\ Value /\ Value) -> ToHydraFn5 -> HydraFn5M
tryV5M v fn = Fn.tryFn5 ((=<<) toValue) v $ \v1 v2 v3 v4 v5 -> Just $ fn v1 v2 v3 v4 v5


tryE :: Entity -> ToHydraEFn0 -> HydraEFn0
tryE _ f (Hydra e) = f e
tryE e f _ = f e


tryEV1 :: (Entity /\ Value) -> ToHydraEFn1 -> HydraEFn1
tryEV1 (_ /\ v) f (Hydra e) = tryV1 v $ f e
tryEV1 (e /\ v) f _         = tryV1 v $ f e


tryEV2 :: (Entity /\ Value /\ Value) -> ToHydraEFn2 -> HydraEFn2
tryEV2 (_ /\ v1 /\ v2) f (Hydra e) = tryV2 (v1 /\ v2) $ f e
tryEV2 (e /\ v1 /\ v2) f _         = tryV2 (v1 /\ v2) $ f e


tryEV3 :: (Entity /\ Value /\ Value /\ Value) -> ToHydraEFn3 -> HydraEFn3
tryEV3 (_ /\ v1 /\ v2 /\ v3) f (Hydra e) = tryV3 (v1 /\ v2 /\ v3) $ f e
tryEV3 (e /\ v1 /\ v2 /\ v3) f _         = tryV3 (v1 /\ v2 /\ v3) $ f e


tryEV4 :: (Entity /\ Value /\ Value /\ Value /\ Value) -> ToHydraEFn4 -> HydraEFn4
tryEV4 (_ /\ v1 /\ v2 /\ v3 /\ v4) f (Hydra e) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e
tryEV4 (e /\ v1 /\ v2 /\ v3 /\ v4) f _         = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e


tryEV5 :: (Entity /\ Value /\ Value /\ Value /\ Value /\ Value) -> ToHydraEFn5 -> HydraEFn5
tryEV5 (_ /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f (Hydra e) = tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e
tryEV5 (e /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f _         = tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e


tryEM :: Entity -> ToHydraEFn0 -> HydraEFn0M
tryEM _ f (Just (Hydra e)) = Just $ f e
tryEM e f _ = Just $ f e


tryEV1M :: (Entity /\ Value) -> ToHydraEFn1 -> HydraEFn1M
tryEV1M (_ /\ v) f (Just (Hydra e)) = tryV1M v $ f e
tryEV1M (e /\ v) f _                = tryV1M v $ f e


tryEV2M :: (Entity /\ Value /\ Value) -> ToHydraEFn2 -> HydraEFn2M
tryEV2M (_ /\ v1 /\ v2) f (Just (Hydra e)) = tryV2M (v1 /\ v2) $ f e
tryEV2M (e /\ v1 /\ v2) f _                = tryV2M (v1 /\ v2) $ f e


tryEV3M :: (Entity /\ Value /\ Value /\ Value) -> ToHydraEFn3 -> HydraEFn3M
tryEV3M (_ /\ v1 /\ v2 /\ v3) f (Just (Hydra e)) = tryV3M (v1 /\ v2 /\ v3) $ f e
tryEV3M (e /\ v1 /\ v2 /\ v3) f _                = tryV3M (v1 /\ v2 /\ v3) $ f e


tryEV4M :: (Entity /\ Value /\ Value /\ Value /\ Value) -> ToHydraEFn4 -> HydraEFn4M
tryEV4M (_ /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Hydra e)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e
tryEV4M (e /\ v1 /\ v2 /\ v3 /\ v4) f _                = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e


tryEV5M :: (Entity /\ Value /\ Value /\ Value /\ Value /\ Value) -> ToHydraEFn5 -> HydraEFn5M
tryEV5M (_ /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f (Just (Hydra e)) = tryV5M (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e
tryEV5M (e /\ v1 /\ v2 /\ v3 /\ v4 /\ v5) f _                = tryV5M (v1 /\ v2 /\ v3 /\ v4 /\ v5) $ f e


tryEE :: (Entity /\ Entity) -> ToHydraEEFn0 -> HydraEEFn0
tryEE ( _ /\ _ ) f (Hydra e1) (Hydra e2) = f e1 e2
tryEE ( _ /\ e2) f (Hydra e1) _          = f e1 e2
tryEE (e1 /\ _ ) f _          (Hydra e2) = f e1 e2
tryEE (e1 /\ e2) f _          _          = f e1 e2


tryEEV1 :: (Entity /\ Entity /\ Value) -> ToHydraEEFn1 -> HydraEEFn1
tryEEV1 ( _ /\ _  /\ v) f (Hydra e1) (Hydra e2) = tryV1 v $ f e1 e2
tryEEV1 ( _ /\ e2 /\ v) f (Hydra e1) _          = tryV1 v $ f e1 e2
tryEEV1 (e1 /\ _  /\ v) f _          (Hydra e2) = tryV1 v $ f e1 e2
tryEEV1 (e1 /\ e2 /\ v) f _          _          = tryV1 v $ f e1 e2


tryEEV2 :: (Entity /\ Entity /\ Value /\ Value) -> ToHydraEEFn2 -> HydraEEFn2
tryEEV2 ( _ /\ _  /\ v1 /\ v2) f (Hydra e1) (Hydra e2) = tryV2 (v1 /\ v2) $ f e1 e2
tryEEV2 ( _ /\ e2 /\ v1 /\ v2) f (Hydra e1) _          = tryV2 (v1 /\ v2) $ f e1 e2
tryEEV2 (e1 /\ _  /\ v1 /\ v2) f _          (Hydra e2) = tryV2 (v1 /\ v2) $ f e1 e2
tryEEV2 (e1 /\ e2 /\ v1 /\ v2) f _          _          = tryV2 (v1 /\ v2) $ f e1 e2


tryEEV3 :: (Entity /\ Entity /\ Value /\ Value /\ Value) -> ToHydraEEFn3 -> HydraEEFn3
tryEEV3 ( _ /\ _  /\ v1 /\ v2 /\ v3) f (Hydra e1) (Hydra e2) = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3 ( _ /\ e2 /\ v1 /\ v2 /\ v3) f (Hydra e1) _          = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3 (e1 /\ _  /\ v1 /\ v2 /\ v3) f _          (Hydra e2) = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3 (e1 /\ e2 /\ v1 /\ v2 /\ v3) f _          _          = tryV3 (v1 /\ v2 /\ v3) $ f e1 e2


tryEEV4 :: (Entity /\ Entity /\ Value /\ Value /\ Value /\ Value) -> ToHydraEEFn4 -> HydraEEFn4
tryEEV4 ( _ /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f (Hydra e1) (Hydra e2) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4 ( _ /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f (Hydra e1) _          = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4 (e1 /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f _          (Hydra e2) = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4 (e1 /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f _          _          = tryV4 (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2


tryEEM :: (Entity /\ Entity) -> ToHydraEEFn0 -> HydraEEFn0M
tryEEM ( _ /\ _ ) f (Just (Hydra e1)) (Just (Hydra e2)) = Just $ f e1 e2
tryEEM ( _ /\ e2) f (Just (Hydra e1)) _                 = Just $ f e1 e2
tryEEM (e1 /\ _ ) f _                 (Just (Hydra e2)) = Just $ f e1 e2
tryEEM (e1 /\ e2) f _                 _                 = Just $ f e1 e2


tryEEV1M :: (Entity /\ Entity /\ Value) -> ToHydraEEFn1 -> HydraEEFn1M
tryEEV1M ( _ /\ _  /\ v) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV1M v $ f e1 e2
tryEEV1M ( _ /\ e2 /\ v) f (Just (Hydra e1)) _                 = tryV1M v $ f e1 e2
tryEEV1M (e1 /\ _  /\ v) f _                 (Just (Hydra e2)) = tryV1M v $ f e1 e2
tryEEV1M (e1 /\ e2 /\ v) f _                 _                 = tryV1M v $ f e1 e2


tryEEV2M :: (Entity /\ Entity /\ Value /\ Value) -> ToHydraEEFn2 -> HydraEEFn2M
tryEEV2M ( _ /\ _  /\ v1 /\ v2) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV2M (v1 /\ v2) $ f e1 e2
tryEEV2M ( _ /\ e2 /\ v1 /\ v2) f (Just (Hydra e1)) _                 = tryV2M (v1 /\ v2) $ f e1 e2
tryEEV2M (e1 /\ _  /\ v1 /\ v2) f _                 (Just (Hydra e2)) = tryV2M (v1 /\ v2) $ f e1 e2
tryEEV2M (e1 /\ e2 /\ v1 /\ v2) f _                 _                 = tryV2M (v1 /\ v2) $ f e1 e2


tryEEV3M :: (Entity /\ Entity /\ Value /\ Value /\ Value) -> ToHydraEEFn3 -> HydraEEFn3M
tryEEV3M ( _ /\ _  /\ v1 /\ v2 /\ v3) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3M ( _ /\ e2 /\ v1 /\ v2 /\ v3) f (Just (Hydra e1)) _                 = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3M (e1 /\ _  /\ v1 /\ v2 /\ v3) f _                 (Just (Hydra e2)) = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2
tryEEV3M (e1 /\ e2 /\ v1 /\ v2 /\ v3) f _                 _                 = tryV3M (v1 /\ v2 /\ v3) $ f e1 e2


tryEEV4M :: (Entity /\ Entity /\ Value /\ Value /\ Value /\ Value) -> ToHydraEEFn4 -> HydraEEFn4M
tryEEV4M ( _ /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Hydra e1)) (Just (Hydra e2)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4M ( _ /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f (Just (Hydra e1)) _                 = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4M (e1 /\ _  /\ v1 /\ v2 /\ v3 /\ v4) f _                 (Just (Hydra e2)) = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
tryEEV4M (e1 /\ e2 /\ v1 /\ v2 /\ v3 /\ v4) f _                 _                 = tryV4M (v1 /\ v2 /\ v3 /\ v4) $ f e1 e2
