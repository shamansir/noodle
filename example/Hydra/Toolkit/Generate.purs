module Hydra.Tookit.Generate where


import Prelude (($), flip, (<$>))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Hydra (Hydra)
import Hydra.Fn (class ToFn, fn)

import Noodle.Node.Define (Def)
import Noodle.Node (Id) as Node


newtype GenId = GenId Node.Id


data EorV
    = E -- entity
    | V -- value


v1 :: String -> Array (String /\ EorV)
v1 v1 = [ v1 /\ V ]


v2 :: String -> String -> Array (String /\ EorV)
v2 v1 v2 = flip (/\) V <$> [ v1, v2 ]


v3 :: String -> String -> String -> Array (String /\ EorV)
v3 v1 v2 v3 = flip (/\) V <$> [ v1, v2, v3 ]


v4 :: String -> String -> String -> String -> Array (String /\ EorV)
v4 v1 v2 v3 v4 = flip (/\) V <$> [ v1, v2, v3, v4 ]


v5 :: String -> String -> String -> String -> String -> Array (String /\ EorV)
v5 v1 v2 v3 v4 v5 = flip (/\) V <$> [ v1, v2, v3, v4, v5 ]


e :: String -> String /\ EorV
e = flip (/\) E



instance ToFn String EorV where
    toFn "noise"    = fn "noise" $ v2 "scale" "offset"
    -- toFn (Voronoi _)  = fn "voronoi" $ v3 "scale" "speed" "blending"
    -- toFn (Osc _)      = fn "osc" $ v3 "freq" "sync" "offset"
    -- toFn (Shape _)    = fn "shape" $ v3 "sides" "radius" "smoothing"
    -- toFn (Gradient _) = fn "gradient" $ v1 "speed"
    -- toFn (Solid _)    = fn "solid" $ v4 "r" "g" "b" "a"
    toFn _ = fn "" []



generate :: GenId -> Maybe (Def Hydra)
generate _ = Nothing