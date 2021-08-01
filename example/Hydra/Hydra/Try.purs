module Hydra.Try where

import Prelude (($))

import Hydra
import Data.Tuple.Nested ((/\), type (/\))

{- tryV1 :: ToHydraFn1 -> HydraFn1
tryV1 f (Value v) = f v
tryV1 _ _ = None -}


tryV1 :: Value -> ToHydraFn1 -> HydraFn1
tryV1 _ f (Value v) = f v
tryV1 def f _ = f def

{-
tryV2 :: ToHydraFn2 -> HydraFn2
tryV2 f (Value v1) (Value v2) = f v1 v2
tryV2 _ _ _ = None
-}


tryV2 :: (Value /\ Value) -> ToHydraFn2 -> HydraFn2
tryV2 _ f (Value v1) (Value v2) = f v1 v2
tryV2 (v1 /\ v2) f _ _ = f v1 v2


{-
tryV3 :: ToHydraFn3 -> HydraFn3
tryV3 f (Value v1) (Value v2) (Value v3) = f v1 v2 v3
tryV3 _ _ _ _ = None
-}


tryV3 :: (Value /\ Value /\ Value) -> ToHydraFn3 -> HydraFn3
tryV3 _ f (Value v1) (Value v2) (Value v3) = f v1 v2 v3
tryV3 (v1 /\ v2 /\ v3) f _ _ _ = f v1 v2 v3

{-
tryV4 :: ToHydraFn3 -> HydraFn3
tryV4 f (Value v1) (Value v2) (Value v3) (Value v4) = f v1 v2 v3 v4
tryV4 _ _ _ _ _ = None
-}


tryV4 :: (Value /\ Value /\ Value /\ Value) -> ToHydraFn4 -> HydraFn4
tryV4 _ f (Value v1) (Value v2) (Value v3) (Value v4)  = f v1 v2 v3 v4
tryV4 (v1 /\ v2 /\ v3 /\ v4) f _ _ _ _ = f v1 v2 v3 v4

{-
tryV5 :: (Value -> Value -> Value -> Value -> Value -> Hydra) -> Hydra -> Hydra -> Hydra -> Hydra -> Hydra -> Hydra
tryV5 f (Value v1) (Value v2) (Value v3) (Value v4) (Value v5) = f v1 v2 v3 v4 v5
tryV5 _ _ _ _ _ _ = None
-}


tryV5 :: (Value /\ Value /\ Value /\ Value /\ Value) -> ToHydraFn5 -> HydraFn5
tryV5 _ f (Value v1) (Value v2) (Value v3) (Value v4) (Value v5) = f v1 v2 v3 v4 v5
tryV5 (v1 /\ v2 /\ v3 /\ v4 /\ v5) f _ _ _ _ _ = f v1 v2 v3 v4 v5

{-
tryE :: ToHydraEFn0 -> HydraFn1
tryE f (Hydra e) = f e
tryE _ _ = None
-}


tryE :: Entity -> ToHydraEFn0 -> HydraEFn0
tryE _ f (Hydra e) = f e
tryE e f _ = f e


{-
tryEV1 :: ToHydraEFn1 -> HydraEFn1
tryEV1 f (Hydra e) (Value v) = f e v
tryEV1 _ _ _ = None
-}


tryEV1 :: (Entity /\ Value) -> ToHydraEFn1 -> HydraEFn1
tryEV1 _ f (Hydra e) (Value v) = f e v
tryEV1 (e /\ v) f _ _ = f e v


{-
tryEV2 :: ToHydraEFn2 -> HydraEFn2
tryEV2 f (Hydra e) (Value v1) (Value v2) = f e v1 v2
tryEV2 _ _ _ _ = None
-}


tryEV2 :: (Entity /\ Value /\ Value) -> ToHydraEFn2 -> HydraEFn2
tryEV2 _ f (Hydra e) (Value v1) (Value v2) = f e v1 v2
tryEV2 (e /\ v1 /\ v2) f _ _ _ = f e v1 v2


{-
tryEV3 :: ToHydraEFn3 -> HydraEFn3
tryEV3 f (Hydra e) (Value v1) (Value v2) (Value v3) = f e v1 v2 v3
tryEV3 _ _ _ _ _ = None
-}


tryEV3 :: (Entity /\ Value /\ Value /\ Value) -> ToHydraEFn3 -> HydraEFn3
tryEV3 _ f (Hydra e) (Value v1) (Value v2) (Value v3) = f e v1 v2 v3
tryEV3 (e /\ v1 /\ v2 /\ v3) f _ _ _ _ = f e v1 v2 v3


{-
tryEV4 :: ToHydraEFn4 -> HydraEFn4
tryEV4 f (Hydra e) (Value v1) (Value v2) (Value v3) (Value v4) = f e v1 v2 v3 v4
tryEV4 _ _ _ _ _ _ = None
-}


tryEV4 :: (Entity /\ Value /\ Value /\ Value /\ Value) -> ToHydraEFn4 -> HydraEFn4
tryEV4 _ f (Hydra e) (Value v1) (Value v2) (Value v3) (Value v4) = f e v1 v2 v3 v4
tryEV4 (e /\ v1 /\ v2 /\ v3 /\ v4) f _ _ _ _ _ = f e v1 v2 v3 v4


osc :: HydraFn3
osc =
    tryV3
        ( Num 60.0 /\ Num 0.1 /\ Num 0.0 )
        (\freq sync offset -> Hydra $ entityOf $ Osc { freq, sync, offset })


out :: Int -> HydraEFn0
out n =
    tryE defaultEntity (\entity -> Out [ entity /\ Output n ])



out' :: HydraEFn0
out' =
    tryE defaultEntity (\entity -> Out [ entity /\ Default ])
