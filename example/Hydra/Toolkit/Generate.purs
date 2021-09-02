module Hydra.Toolkit.Generate
    (all, generate, GenId, Kind(..), byKind, ofKind) where


import Debug as Debug
import Prelude (($), flip, (<$>), (<>), map, join, (==))


import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))

import Hydra
    ( Hydra
    , HydraFn1M, HydraFn2M, HydraFn3M, HydraFn4M, HydraFn5M, HydraFn6M
    , HydraTFn0, HydraTFn1, HydraTFn2, HydraTFn3, HydraTFn4, HydraTFn5
    )
import Hydra.Fn (class ToFn, Fn, fn, toFn)
import Hydra.Fn as Fn
import Hydra.Try as Try
import Hydra.Toolkit.Shape as TShape

import Noodle.Node.Define (Def, Receive, Pass)
import Noodle.Node.Define (empty, define, defineEffectful, pass') as Def
import Noodle.Node (Id, Family) as Node
import Noodle.Node ((+>), (<+))
import Noodle.Node.Shape as Shape
import Noodle.Node.Shape (Inlets, Outlets)
import Noodle.Channel.Shape (Shape') as Ch


newtype GenId = GenId Node.Id


data Kind
    = Source
    | Geom
    | Color
    | Blend
    | Mod


byKind :: Kind -> Array Node.Family
byKind Source = [ "noise", "voronoi", "osc", "shape", "gradient", "solid" ]
byKind Geom = [ "rotate", "scale", "pixelate", "repeat", "repeat-x", "repeat-y", "kaleid", "scroll-x", "scroll-y" ]
byKind Color = [ "posterize", "shift", "invert", "contrast", "brightness", "luma", "tresh"
               , "color", "saturate", "hue", "colorama" ]
byKind Blend = [ "add", "layer", "blend", "mult", "diff", "mask" ]
byKind Mod = [ "mod-repeat", "mod-repeat-x", "mod-repeat-y", "mod-kaleid", "mod-scroll-x", "mod-scroll-y", "modulate"
             , "mod-scale", "mod-pixelate", "mod-rotate", "mod-hue" ]


ofKind :: Node.Family -> Maybe Kind
ofKind family =
    Array.findMap
        (\(kind /\ family') ->
            if (family' == family) then Just kind else Nothing
        )
        all'


all :: Array GenId
all = GenId <$> byKind Source <> byKind Geom <> byKind Color <> byKind Blend <> byKind Mod


all' :: Array (Kind /\ Node.Family)
all' = ((/\) Source <$> byKind Source)
    <> ((/\) Geom   <$> byKind Geom)
    <> ((/\) Color  <$> byKind Color)
    <> ((/\) Blend  <$> byKind Blend)
    <> ((/\) Mod    <$> byKind Mod)


instance ToFn String TextureOrValue where

    {- Source -}
    toFn "noise"    = fn "noise" $ v2 "scale" "offset"
    toFn "voronoi"  = fn "voronoi" $ v3 "scale" "speed" "blending"
    toFn "osc"      = fn "osc" $ v3 "freq" "sync" "offset"
    toFn "shape"    = fn "shape" $ v3 "sides" "radius" "smoothing"
    toFn "gradient" = fn "gradient" $ v1 "speed"
    toFn "solid"    = fn "solid" $ v4 "r" "g" "b" "a"

    {- Geometry -}
    toFn "rotate"   = fn "rotate" $ vt2 "src" "angle" "speed"
    toFn "scale"    = fn "scale" $ vt5 "src" "amount" "x-mult" "y-mult" "offset-x" "offset-y"
    toFn "pixelate" = fn "pixelate" $ vt2 "src" "pixel-x" "pixel-y"
    toFn "repeat"   = fn "repeat" $ vt4 "src" "repeat-x" "repeat-y" "offset-x" "offset-y"
    toFn "repeat-x" = fn "repeat-x" $ vt2 "src" "reps" "offset"
    toFn "repeat-y" = fn "repeat-y" $ vt2 "src" "reps" "offset"
    toFn "kaleid"   = fn "kaleid" $ vt1 "src" "n-sides"
    toFn "scroll-x" = fn "scroll-x" $ vt2 "src" "amount" "speed"
    toFn "scroll-y" = fn "scroll-y" $ vt2 "src" "amount" "speed"

    {- Color -}
    toFn "posterize" = fn "posterize" $ vt2 "src" "bins" "gamma"
    toFn "shift"     = fn "shift" $ vt4 "src" "r" "g" "b" "a"
    toFn "invert"    = fn "invert" $ vt1 "src" "amount"
    toFn "contrast"  = fn "contrast" $ vt1 "src" "amount"
    toFn "brightness" = fn "brightness" $ vt1 "src" "amount"
    toFn "luma"      = fn "luma" $ vt2 "src" "treshhold" "tolerance"
    toFn "tresh"     = fn "tresh" $ vt2 "src" "treshhold" "tolerance"
    toFn "color"     = fn "color" $ vt4 "src" "r" "g" "b" "a"
    toFn "saturate"  = fn "saturate" $ vt1 "src" "amount"
    toFn "hue"       = fn "hue" $ vt1 "src" "amount"
    toFn "colorama"  = fn "colorama" $ vt1 "src" "amount"

    {- Blend -}
    toFn "add"       = fn "add" $ vtt1 "src" "what" "amount"
    toFn "layer"     = fn "layer" $ vtt0 "src" "what"
    toFn "blend"     = fn "blend" $ vtt1 "src" "what" "amount"
    toFn "mult"      = fn "mult" $ vtt1 "src" "what" "amount"
    toFn "diff"      = fn "diff" $ vtt0 "src" "what"
    toFn "mask"      = fn "mask" $ vtt0 "src" "what"

    {- Modulate -}
    toFn "mod-repeat"   = fn "mod-repeat" $ vtt4 "src" "what" "repeat-x" "repeat-y" "offset-x" "offset-y"
    toFn "mod-repeat-x" = fn "mod-repeat-x" $ vtt2 "src" "what" "reps" "offset"
    toFn "mod-repeat-y" = fn "mod-repeat-y" $ vtt2 "src" "what" "reps" "offset"
    toFn "mod-kaleid"   = fn "mod-kaleid" $ vtt1 "src" "what" "n-sides"
    toFn "mod-scroll-x" = fn "mod-scroll-x" $ vtt2 "src" "what" "amount" "speed"
    toFn "mod-scroll-y" = fn "mod-scroll-y" $ vtt2 "src" "what" "amount" "speed"
    toFn "modulate"     = fn "modulate" $ vtt1 "src" "what" "amount"
    toFn "mod-scale"    = fn "mod-scale" $ vtt2 "src" "what" "multiple" "offset"
    toFn "mod-pixelate" = fn "mod-pixelate" $ vtt2 "src" "what" "multiple" "offset"
    toFn "mod-rotate"   = fn "mod-rotate" $ vtt2 "src" "what" "multiple" "offset"
    toFn "mod-hue"      = fn "mod-hue" $ vtt1 "src" "what" "amount"

    {- Other -}
    toFn _ = fn "" []


instance ToFn GenId TextureOrValue where
    toFn (GenId str) = toFn str


generate' :: String -> Def Hydra
{- Source -}
generate' "noise"    = fromFn2 (toFn "noise")    Try.noise
generate' "voronoi"  = fromFn3 (toFn "voronoi")  Try.voronoi
generate' "osc"      = fromFn3 (toFn "osc")      Try.osc
generate' "shape"    = fromFn3 (toFn "shape")    Try.shape
generate' "gradient" = fromFn1 (toFn "gradient") Try.gradient
generate' "solid"    = fromFn4 (toFn "solid")    Try.solid
{- Geometry -}
generate' "rotate"    = fromFn3 (toFn "rotate")   Try.rotate
generate' "scale"     = fromFn6 (toFn "scale")    Try.scale
generate' "pixelate"  = fromFn3 (toFn "pixelate") Try.pixelate
generate' "repeat"    = fromFn5 (toFn "repeat")   Try.repeat
generate' "repeat-x"  = fromFn3 (toFn "repeat-x") Try.repeatX
generate' "repeat-y"  = fromFn3 (toFn "repeat-y") Try.repeatY
generate' "kaleid"    = fromFn2 (toFn "kaleid")   Try.kaleid
generate' "scroll-x"  = fromFn3 (toFn "scroll-x") Try.scrollX
generate' "scroll-y"  = fromFn3 (toFn "scroll-y") Try.scrollY
{- Color -}
generate' "posterize"  = fromFn3 (toFn "posterize")  Try.posterize
generate' "shift"      = fromFn5 (toFn "shift")      Try.shift
generate' "invert"     = fromFn2 (toFn "invert")     Try.invert
generate' "contrast"   = fromFn2 (toFn "contrast")   Try.contrast
generate' "brightness" = fromFn2 (toFn "brightness") Try.brightness
generate' "luma"       = fromFn3 (toFn "luma")       Try.luma
generate' "tresh"      = fromFn3 (toFn "tresh")      Try.tresh
generate' "color"      = fromFn5 (toFn "color")      Try.color
generate' "saturate"   = fromFn2 (toFn "saturate")   Try.saturate
generate' "hue"        = fromFn2 (toFn "hue")        Try.hue
generate' "colorama"   = fromFn2 (toFn "colorama")   Try.colorama
{- Blend -}
generate' "add"   = fromFn3 (toFn "add")   Try.add
generate' "layer" = fromFn2 (toFn "layer") Try.layer
generate' "blend" = fromFn3 (toFn "blend") Try.blend
generate' "mult"  = fromFn3 (toFn "mult")  Try.mult
generate' "diff"  = fromFn2 (toFn "diff")  Try.diff
generate' "mask"  = fromFn2 (toFn "mask")  Try.mask
{- Modulate -}
generate' "mod-repeat"   = fromFn6 (toFn "mod-repeat")   Try.modRepeat
generate' "mod-repeat-x" = fromFn4 (toFn "mod-repeat-x") Try.modRepeatX
generate' "mod-repeat-y" = fromFn4 (toFn "mod-repeat-y") Try.modRepeatY
generate' "mod-kaleid"   = fromFn3 (toFn "mod-kaleid")   Try.modKaleid
generate' "mod-scroll-x" = fromFn4 (toFn "mod-scroll-x") Try.modScrollX
generate' "mod-scroll-y" = fromFn4 (toFn "mod-scroll-y") Try.modScrollY
generate' "modulate"     = fromFn3 (toFn "modulate")     Try.modulate
generate' "mod-scale"    = fromFn4 (toFn "mod-scale")    Try.modScale
generate' "mod-pixelate" = fromFn4 (toFn "mod-pixelate") Try.modPixelate
generate' "mod-rotate"   = fromFn4 (toFn "mod-rotate")   Try.modRotate
generate' "mod-hue"      = fromFn3 (toFn "mod-hue")      Try.modHue

generate' _ = Def.empty


generate :: GenId -> String /\ Def Hydra
generate (GenId id) = id /\ generate' id


tovToShape :: TextureOrValue -> Ch.Shape' Hydra
tovToShape T = TShape.texture
tovToShape V = TShape.value


inletsFromFn :: Fn TextureOrValue -> Inlets Hydra
inletsFromFn fn =
    Shape.formInlets $ map tovToShape <$> Fn.getArgs fn


outletsFromFn :: Fn TextureOrValue -> Inlets Hydra
outletsFromFn fn =
    Shape.formOutlets [ Fn.getName fn /\ TShape.texture ]


-- loadFromInlet :: Receive Hydra -> String /\ TextureOrValue -> Maybe Hydra
-- loadFromInlet inlets (name /\ _) = name <+ inlets

loadFromInlet :: Receive Hydra -> String -> Maybe Hydra
loadFromInlet = flip (<+)


fromFn :: Fn TextureOrValue -> (Receive Hydra -> Pass Hydra) -> Def Hydra
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


fromFn1 :: Fn TextureOrValue -> HydraFn1M -> Def Hydra
fromFn1 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn1 fn f


fromFn2 :: Fn TextureOrValue -> HydraFn2M -> Def Hydra
fromFn2 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn2 fn f


fromFn3 :: Fn TextureOrValue -> HydraFn3M -> Def Hydra
fromFn3 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn3 fn f


fromFn4 :: Fn TextureOrValue -> HydraFn4M -> Def Hydra
fromFn4 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn4 fn f


fromFn5 :: Fn TextureOrValue -> HydraFn5M -> Def Hydra
fromFn5 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn5 fn f


fromFn6 :: Fn TextureOrValue -> HydraFn6M -> Def Hydra
fromFn6 fn f =
    fromFn fn $
        makeReceiver Fn.applyFn6 fn f


data TextureOrValue
    = T -- texture
    | V -- value


v1 :: String -> Array (String /\ TextureOrValue)
v1 v1' = [ v v1' ]


v2 :: String -> String -> Array (String /\ TextureOrValue)
v2 v1' v2' = v <$> [ v1', v2' ]


v3 :: String -> String -> String -> Array (String /\ TextureOrValue)
v3 v1' v2' v3' = v <$> [ v1', v2', v3' ]


v4 :: String -> String -> String -> String -> Array (String /\ TextureOrValue)
v4 v1' v2' v3' v4' = v <$> [ v1', v2', v3', v4' ]


v5 :: String -> String -> String -> String -> String -> Array (String /\ TextureOrValue)
v5 v1' v2' v3' v4' v5' = v <$> [ v1', v2', v3', v4', v5' ]


vt0 :: String -> Array (String /\ TextureOrValue)
vt0 t0 = [ t t0 ]


vt1 :: String -> String -> Array (String /\ TextureOrValue)
vt1 t0' v1' = vt0 t0' <> v1 v1'


vt2 :: String -> String -> String -> Array (String /\ TextureOrValue)
vt2 t0' v1' v2' = vt0 t0' <> v2 v1' v2'


vt3 :: String -> String -> String -> String -> Array (String /\ TextureOrValue)
vt3 t0' v1' v2' v3' = vt0 t0' <> v3 v1' v2' v3'


vt4 :: String -> String -> String -> String -> String -> Array (String /\ TextureOrValue)
vt4 t0' v1' v2' v3' v4' = vt0 t0' <> v4 v1' v2' v3' v4'


vt5 :: String -> String -> String -> String -> String -> String -> Array (String /\ TextureOrValue)
vt5 t0' v1' v2' v3' v4' v5' = vt0 t0' <> v5 v1' v2' v3' v4' v5'


vtt0 :: String -> String -> Array (String /\ TextureOrValue)
vtt0 t0 t1 = [ t t0, t t1 ]


vtt1 :: String -> String -> String -> Array (String /\ TextureOrValue)
vtt1 t0' t1' v1' = vtt0 t0' t1' <> v1 v1'


vtt2 :: String -> String -> String -> String -> Array (String /\ TextureOrValue)
vtt2 t0' t1' v1' v2' = vtt0 t0' t1' <> v2 v1' v2'


vtt3 :: String -> String -> String -> String -> String -> Array (String /\ TextureOrValue)
vtt3 t0' t1' v1' v2' v3' = vtt0 t0' t1' <> v3 v1' v2' v3'


vtt4 :: String -> String -> String -> String -> String -> String -> Array (String /\ TextureOrValue)
vtt4 t0' t1' v1' v2' v3' v4' = vtt0 t0' t1' <> v4 v1' v2' v3' v4'



v :: String -> String /\ TextureOrValue
v = flip (/\) V


t :: String -> String /\ TextureOrValue
t = flip (/\) T
