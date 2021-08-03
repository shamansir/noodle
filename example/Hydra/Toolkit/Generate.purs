module Hydra.Tookit.Generate
    (all, generate, GenId) where


import Prelude (($), flip, (<$>), (<>), map, join)


import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))

import Hydra
    ( Hydra
    , HydraFn1M, HydraFn2M, HydraFn3M, HydraFn4M, HydraFn5M
    , HydraEFn0, HydraEFn1, HydraEFn2, HydraEFn3, HydraEFn4
    )
import Hydra.Fn (class ToFn, Fn, fn, toFn)
import Hydra.Fn as Fn
import Hydra.Try as Try
import Hydra.Toolkit.Shape as TShape

import Noodle.Node.Define (Def, Receive, Pass)
import Noodle.Node.Define (empty, define, defineEffectful, pass') as Def
import Noodle.Node (Id) as Node
import Noodle.Node ((+>), (<+))
import Noodle.Node.Shape as Shape
import Noodle.Node.Shape (Inlets, Outlets)
import Noodle.Channel.Shape (Shape') as Ch


newtype GenId = GenId Node.Id


all :: Array GenId
all = GenId <$> sources <> geom <> color <> blends <> mods
    where
        sources = [ "noise", "voronoi", "osc", "shape", "gradient" ]
        geom = [ "rotate", "scale", "pixelate", "repeat", "repeat-x", "repeat-y", "kaleid", "scroll-x", "scroll-y" ]
        color = [ "posterize", "shift", "invert", "contrast", "brightness", "luma", "tresh"
                , "color", "saturate", "hue", "colorama" ]
        blends = [ "add", "layer", "blend", "mult", "diff", "mask" ]
        mods = [ "mod-repeat", "mod-repeat-x", "mod-repeat-y", "mod-kaleid", "mod-scroll-x", "mod-scroll-y", "moduldate"
               , "mod-scale", "mod-pixelate", "mod-rotate", "mod-hue" ]


instance ToFn String EorV where

    {- Source -}
    toFn "noise"    = fn "noise" $ v2 "scale" "offset"
    toFn "voronoi"  = fn "voronoi" $ v3 "scale" "speed" "blending"
    toFn "osc"      = fn "osc" $ v3 "freq" "sync" "offset"
    toFn "shape"    = fn "shape" $ v3 "sides" "radius" "smoothing"
    toFn "gradient" = fn "gradient" $ v1 "speed"
    toFn "solid"    = fn "solid" $ v4 "r" "g" "b" "a"

    {- Geometry -}
    toFn "rotate"   = fn "rotate" $ v2 "angle" "speed"
    toFn "scale"    = fn "scale" $ v5 "amount" "x-mult" "y-mult" "offset-x" "offset-y"
    toFn "pixelate" = fn "pixelate" $ v2 "pixel-x" "pixel-y"
    toFn "repeat"   = fn "repeat" $ v4 "repeat-x" "repeat-y" "offset-x" "offset-y"
    toFn "repeat-x" = fn "repeat-x" $ v2 "reps" "offset"
    toFn "repeat-y" = fn "repeat-y" $ v2 "reps" "offset"
    toFn "kaleid"   = fn "kaleid" $ v1 "n-sides"
    toFn "scroll-x" = fn "scroll-x" $ v2 "amount" "speed"
    toFn "scroll-y" = fn "scroll-y" $ v2 "amount" "speed"

    {- Color -}
    toFn "posterize" = fn "posterize" $ v2 "bins" "gamma"
    toFn "shift"     = fn "shift" $ v4 "r" "g" "b" "a"
    toFn "invert"    = fn "invert" $ v1 "amount"
    toFn "contrast"  = fn "contrast" $ v1 "amount"
    toFn "brightness" = fn "brightness" $ v1 "amount"
    toFn "luma"      = fn "luma" $ v2 "treshhold" "tolerance"
    toFn "tresh"     = fn "tresh" $ v2 "treshhold" "tolerance"
    toFn "color"     = fn "color" $ v4 "r" "g" "b" "a"
    toFn "saturate"  = fn "saturate" $ v1 "amount"
    toFn "hue"       = fn "hue" $ v1 "amount"
    toFn "colorama"  = fn "hue" $ v1 "amount"

    {- Blend -}
    toFn "add"       = fn "add" $ ve1 "what" "amount"
    toFn "layer"     = fn "layer" $ ve0 "what"
    toFn "blend"     = fn "blend" $ ve1 "what" "amount"
    toFn "mult"      = fn "mult" $ ve1 "what" "amount"
    toFn "diff"      = fn "diff" $ ve0 "what"
    toFn "mask"      = fn "mask" $ ve0 "what"

    {- Modulate -}
    toFn "mod-repeat"   = fn "mod-repeat" $ ve4 "what" "repeat-x" "repeat-y" "offset-x" "offset-y"
    toFn "mod-repeat-x" = fn "mod-repeat-x" $ ve2 "what" "reps" "offset"
    toFn "mod-repeat-y" = fn "mod-repeat-y" $ ve2 "what" "reps" "offset"
    toFn "mod-kaleid"   = fn "mod-kaleid" $ ve1 "what" "n-sides"
    toFn "mod-scroll-x" = fn "mod-scroll-x" $ ve2 "what" "amount" "speed"
    toFn "mod-scroll-y" = fn "mod-scroll-y" $ ve2 "what" "amount" "speed"
    toFn "modulate"     = fn "modulate" $ ve1 "what" "amount"
    toFn "mod-scale"    = fn "mod-scale" $ ve2 "what" "multiple" "offset"
    toFn "mod-pixelate" = fn "mod-pixelate" $ ve2 "what" "multiple" "offset"
    toFn "mod-rotate"   = fn "mod-rotate" $ ve2 "what" "multiple" "offset"
    toFn "mod-hue"      = fn "mod-hue" $ ve1 "what" "amount"

    {- Other -}
    toFn _ = fn "" []


instance ToFn GenId EorV where
    toFn (GenId str) = toFn str


generate' :: String -> Def Hydra
{- Source -}
generate' "noise"    = fromFnV2 (toFn "noise") Try.noise
generate' "voronoi"  = fromFnV3 (toFn "voronoi") Try.voronoi
generate' "osc"      = fromFnV3 (toFn "osc") Try.osc
generate' "shape"    = fromFnV3 (toFn "shape") Try.shape
generate' "gradient" = fromFnV1 (toFn "gradient") Try.gradient
generate' "solid"    = fromFnV4 (toFn "solid") Try.solid

generate' _ = Def.empty


generate :: GenId -> String /\ Def Hydra
generate (GenId id) = id /\ generate' id


eovToShape :: EorV -> Ch.Shape' Hydra
eovToShape E = TShape.entity
eovToShape V = TShape.value


inletsFromFn :: Fn EorV -> Inlets Hydra
inletsFromFn fn =
    Shape.formInlets $ map eovToShape <$> Fn.getArgs fn


outletsFromFn :: Fn EorV -> Inlets Hydra
outletsFromFn fn =
    Shape.formOutlets [ Fn.getName fn /\ TShape.entity ]


-- loadFromInlet :: Receive Hydra -> String /\ EorV -> Maybe Hydra
-- loadFromInlet inlets (name /\ _) = name <+ inlets

loadFromInlet :: Receive Hydra -> String -> Maybe Hydra
loadFromInlet = flip (<+)


fromFn :: Fn EorV -> (Receive Hydra -> Pass Hydra) -> Def Hydra
fromFn fn =
    Def.define
        (inletsFromFn fn)
        (outletsFromFn fn)


makeReceiver :: forall f a. (f -> Fn (Maybe Hydra) -> Maybe (Maybe Hydra)) -> Fn a -> f -> Receive Hydra -> Pass Hydra
makeReceiver x fn f =
    (\inlets ->
        Def.pass'
            [ Fn.getName fn
                /\ join (x f $ loadFromInlet inlets <$> fst <$> Fn.addNames fn)
            ]
    )


fromFnV1 :: Fn EorV -> HydraFn1M -> Def Hydra
fromFnV1 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn1 fn f


fromFnV2 :: Fn EorV -> HydraFn2M -> Def Hydra
fromFnV2 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn2 fn f


fromFnV3 :: Fn EorV -> HydraFn3M -> Def Hydra
fromFnV3 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn3 fn f


fromFnV4 :: Fn EorV -> HydraFn4M -> Def Hydra
fromFnV4 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn4 fn f


fromFnV5 :: Fn EorV -> HydraFn5M -> Def Hydra
fromFnV5 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn5 fn f


data EorV
    = E -- entity
    | V -- value


v1 :: String -> Array (String /\ EorV)
v1 v1' = [ v v1' ]


v2 :: String -> String -> Array (String /\ EorV)
v2 v1' v2' = v <$> [ v1', v2' ]


v3 :: String -> String -> String -> Array (String /\ EorV)
v3 v1' v2' v3' = v <$> [ v1', v2', v3' ]


v4 :: String -> String -> String -> String -> Array (String /\ EorV)
v4 v1' v2' v3' v4' = v <$> [ v1', v2', v3', v4' ]


v5 :: String -> String -> String -> String -> String -> Array (String /\ EorV)
v5 v1' v2' v3' v4' v5' = v <$> [ v1', v2', v3', v4', v5' ]


ve0 :: String -> Array (String /\ EorV)
ve0 e0 = [ e e0 ]


ve1 :: String -> String -> Array (String /\ EorV)
ve1 e0' v1' = ve0 e0' <> v1 v1'


ve2 :: String -> String -> String -> Array (String /\ EorV)
ve2 e0' v1' v2' = ve0 e0' <> v2 v1' v2'


ve3 :: String -> String -> String -> String -> Array (String /\ EorV)
ve3 e0' v1' v2' v3' = ve0 e0' <> v3 v1' v2' v3'


ve4 :: String -> String -> String -> String -> String -> Array (String /\ EorV)
ve4 e0' v1' v2' v3' v4' = ve0 e0' <> v4 v1' v2' v3' v4'


v :: String -> String /\ EorV
v = flip (/\) V


e :: String -> String /\ EorV
e = flip (/\) E
