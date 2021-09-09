module Hydra.API where


import Prelude (($), flip, (<<<))

import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Hydra as H
import Hydra.Queue as HQ



{- ##### -}
{- Queue -}
{- ##### -}


out :: H.Texture -> HQ.Queue
out = HQ.just


out' :: H.Buffer -> H.Texture -> HQ.Queue
out' buf tex = queue $ Array.singleton $ out'' buf tex


out'' :: H.Buffer -> H.Texture -> H.Buffer /\ H.Texture
out'' = (/\)


queue :: Array (H.Buffer /\ H.Texture) -> HQ.Queue
queue = HQ.fromFoldable



{- ##### -}
{- Value -}
{- ##### -}


n :: Number -> H.Value
n = H.Num


mouseX :: H.Value
mouseX = H.MouseX


mouseY :: H.Value
mouseY = H.MouseY


time :: H.Value
time = H.Time


seq :: Array H.Value -> H.Value
seq = H.Seq


canvasWidth :: H.Value
canvasWidth = H.CanvasWidth

canvasHeight :: H.Value
canvasHeight = H.CanvasHeight


windowWidth :: H.Value
windowWidth = H.WindowWidth

windowHeight :: H.Value
windowHeight = H.WindowHeight


pi :: H.Value
pi = H.Pi


dyn :: H.Value -> H.Value
dyn = H.Dynamic


ofTime :: H.Value -> H.Value
ofTime = H.OfTime


fft :: Int -> H.Value
fft = H.Harmonic


{- ########## -}
{- Expression -}
{- ########## -}


infixl 6 add' as /+/
infixl 6 subtract as /-/
infixl 7 multiply as /*/
infixl 7 divide as ///


-- implement arithmetics typeclasses on values?


expr :: H.Op -> H.Value -> H.Value -> H.Value
expr op v1 v2 = H.Expr v1 op v2


add' :: H.Value -> H.Value -> H.Value
add' = expr H.Addition


subtract :: H.Value -> H.Value -> H.Value
subtract = expr H.Subtraction


multiply :: H.Value -> H.Value -> H.Value
multiply = expr H.Multiplication


divide :: H.Value -> H.Value -> H.Value
divide = expr H.Division


{- ###### -}
{- Buffer -}
{- ###### -}


default :: H.Buffer
default = H.Default


o0 :: H.Buffer
o0 = H.O0


o1 :: H.Buffer
o1 = H.O1


o2 :: H.Buffer
o2 = H.O2


o3 :: H.Buffer
o3 = H.O3


s0 :: H.Buffer
s0 = H.S0


s1 :: H.Buffer
s1 = H.S1


s2 :: H.Buffer
s2 = H.S2


s3 :: H.Buffer
s3 = H.S3


{- ###### -}
{- Source -}
{- ###### -}


gradient  :: H.Value -> H.Texture
gradient  speed = H.textureOf $ H.Gradient { speed }
gradient0 :: H.Texture
gradient0 = gradient $ canvasWidth -- speed


noise  :: H.Value -> H.Value -> H.Texture
noise  scale offset = H.textureOf $ H.Noise { scale, offset }
noise1 :: H.Value -> H.Texture
noise1 scale = noise scale $ n 0.1 -- offset
noise0 :: H.Texture
noise0 = noise1 $ n 10.0 -- scale


osc  :: H.Value -> H.Value -> H.Value -> H.Texture
osc  freq sync offset = H.textureOf $ H.Osc { freq, sync, offset }
osc2 :: H.Value -> H.Value -> H.Texture
osc2 freq sync = osc freq sync $ n 0.0 -- offset
osc1 :: H.Value -> H.Texture
osc1 freq = osc2 freq $ n 0.1 -- sync
osc0 :: H.Texture
osc0 = osc1 $ n 60.0 -- freq


src  :: H.Buffer -> H.Texture
src  = H.textureOf <<< H.Source


shape :: H.Value -> H.Value -> H.Value -> H.Texture
shape  sides radius smoothing = H.textureOf $ H.Shape { sides, radius, smoothing }
shape2 :: H.Value -> H.Value -> H.Texture
shape2 sides radius = shape sides radius $ n 0.01 -- smoothing
shape1 :: H.Value -> H.Texture
shape1 sides = shape2 sides $ n 0.3 -- radius
shape0 :: H.Texture
shape0 = shape1 $ n 3.0 -- sides


solid :: H.Value -> H.Value -> H.Value -> H.Value -> H.Texture
solid  r g b a = H.textureOf $ H.Solid { r, g, b, a }
solid3 :: H.Value -> H.Value -> H.Value -> H.Texture
solid3 r g b = solid r g b $ n 1.0 -- a
solid2 :: H.Value -> H.Value -> H.Texture
solid2 r g = solid3 r g $ n 0.0 -- b
solid1 :: H.Value -> H.Texture
solid1 r = solid2 r $ n 0.0 -- g
solid0 :: H.Texture
solid0 = solid1 $ n 0.0 -- r


voronoi  :: H.Value -> H.Value -> H.Value -> H.Texture
voronoi  scale speed blending = H.textureOf $ H.Voronoi { scale, speed, blending }
voronoi2 :: H.Value -> H.Value -> H.Texture
voronoi2 scale speed = voronoi scale speed $ n 0.01 -- blending
voronoi1 :: H.Value -> H.Texture
voronoi1 scale = voronoi2 scale $ n 0.3 -- speed
voronoi0 :: H.Texture
voronoi0 = voronoi1 $ n 3.0 -- scale



{- ######### -}
{- Modifiers -}
{- ######### -}


modify :: H.Modifier -> H.Texture -> H.Texture
modify = flip H.addModifier


-- FIXME: use `ToFn` to build those?

type V0Modifier = H.Texture -> H.Texture
type V1Modifier = H.Value -> H.Texture -> H.Texture
type V2Modifier = H.Value -> H.Value -> H.Texture -> H.Texture
type V3Modifier = H.Value -> H.Value -> H.Value -> H.Texture -> H.Texture
type V4Modifier = H.Value -> H.Value -> H.Value -> H.Value -> H.Texture -> H.Texture
type V5Modifier = H.Value -> H.Value -> H.Value -> H.Value -> H.Value -> H.Texture -> H.Texture


type VT0Modifier = H.Texture -> H.Texture -> H.Texture
type VT1Modifier = H.Texture -> H.Value -> H.Texture -> H.Texture
type VT2Modifier = H.Texture -> H.Value -> H.Value -> H.Texture -> H.Texture
type VT3Modifier = H.Texture -> H.Value -> H.Value -> H.Value -> H.Texture -> H.Texture
type VT4Modifier = H.Texture -> H.Value -> H.Value -> H.Value -> H.Value -> H.Texture -> H.Texture


{- ##### -}
{- Color -}
{- ##### -}


brightness  :: V1Modifier
brightness  amount = modify $ H.color $ H.Brightness { amount }
brightness0 :: V0Modifier
brightness0 = brightness $ n 0.4 -- amount


contrast :: V1Modifier
contrast  amount = modify $ H.color $ H.Contrast { amount }
contrast0 :: V0Modifier
contrast0 = contrast $ n 1.6 -- amount


color  :: V4Modifier
color  r g b a = modify $ H.color $ H.Color { r, g, b, a }
color3 :: V3Modifier
color3 r g b = color r g b $ n 1.0 -- a
color2 :: V2Modifier
color2 r g = color3 r g $ n 0.0 --b
color1 :: V1Modifier
color1 r = color2 r $ n 0.0 -- g
color0 :: V0Modifier
color0 = color1 $ n 0.0 -- r


colorama  :: V1Modifier
colorama  amount = modify $ H.color $ H.Colorama { amount }
colorama0 :: V0Modifier
colorama0 = colorama $ n 0.005 -- amount


invert  :: V1Modifier
invert  amount = modify $ H.color $ H.Invert { amount }
invert0 :: V0Modifier
invert0 = invert $ n 1.0 -- amount


luma  :: V2Modifier
luma  treshold tolerance = modify $ H.color $ H.Luma { treshold, tolerance }
luma1 :: V1Modifier
luma1 treshold = luma treshold $ n 0.1 -- tolerance
luma0 :: V0Modifier
luma0 = luma1 $ n 0.5 -- treshold


posterize  :: V2Modifier
posterize  bins gamma = modify $ H.color $ H.Posterize { bins, gamma }
posterize1 :: V1Modifier
posterize1 bins = posterize bins $ n 0.6 -- gamma
posterize0 :: V0Modifier
posterize0 = posterize1 $ n 3.0 -- bins


saturate  :: V1Modifier
saturate  amount = modify $ H.color $ H.Saturate { amount }
saturate0 :: V0Modifier
saturate0 = saturate $ n 2.0


shift  :: V4Modifier
shift  r g b a = modify $ H.color $ H.Shift { r, g, b, a }
shift3 :: V3Modifier
shift3 r g b = shift r g b $ n 0.5 -- a
shift2 :: V2Modifier
shift2 r g = shift3 r g $ n 0.5 --b
shift1 :: V1Modifier
shift1 r = shift2 r $ n 0.5 --g
shift0 :: V0Modifier
shift0 = shift1 $ n 0.5 --r


tresh  :: V2Modifier
tresh  treshold tolerance = modify $ H.color $ H.Tresh { treshold, tolerance }
tresh1 :: V1Modifier
tresh1 treshold = tresh treshold $ n 0.04 -- tolerance
tresh0 :: V0Modifier
tresh0 = tresh1 $ n 0.5 -- treshold


{- ######## -}
{- Geometry -}
{- ######## -}


kaleid  :: V1Modifier
kaleid  nSides = modify $ H.geometry $ H.Kaleid { nSides }
kaleid0 :: V0Modifier
kaleid0 = kaleid $ n 4.0 -- nSides


pixelate  :: V2Modifier
pixelate  pixelX pixelY = modify $ H.geometry $ H.Pixelate { pixelX, pixelY }
pixelate1 :: V1Modifier
pixelate1 pixelX = pixelate pixelX $ n 20.0 -- pixelY
pixelate0 :: V0Modifier
pixelate0 = pixelate1 $ n 20.0 -- pixelX


repeat  :: V4Modifier
repeat  repeatX repeatY offsetX offsetY = modify $ H.geometry $ H.Repeat { repeatX, repeatY, offsetX, offsetY }
repeat3 :: V3Modifier
repeat3 repeatX repeatY offsetX = repeat repeatX repeatY offsetX $ n 0.0 -- offsetY
repeat2 :: V2Modifier
repeat2 repeatX repeatY = repeat3 repeatX repeatY $ n 0.0 -- offsetX
repeat1 :: V1Modifier
repeat1 repeatX = repeat2 repeatX $ n 3.0 -- repeatY
repeat0 :: V0Modifier
repeat0 = repeat1 $ n 3.0 -- repeatX


repeatX  :: V2Modifier
repeatX  reps offset = modify $ H.geometry $ H.RepeatX { reps, offset }
repeatX1 :: V1Modifier
repeatX1 reps = repeatX reps $ n 0.0 -- offset
repeatX0 :: V0Modifier
repeatX0 = repeatX1 $ n 3.0 -- reps


repeatY  :: V2Modifier
repeatY  reps offset = modify $ H.geometry $ H.RepeatY { reps, offset }
repeatY1 :: V1Modifier
repeatY1 reps = repeatY reps $ n 0.0 -- offset
repeatY0 :: V0Modifier
repeatY0 = repeatY1 $ n 3.0 -- reps


rotate  :: V2Modifier
rotate  angle speed = modify $ H.geometry $ H.Rotate { angle, speed }
rotate1 :: V1Modifier
rotate1 angle = rotate angle $ n 0.0 -- speed
rotate0 :: V0Modifier
rotate0 = rotate1 $ n 10.0 -- angle


scale  :: V5Modifier
scale  amount xMult yMult offsetX offsetY = modify $ H.geometry $ H.Scale { amount, xMult, yMult, offsetX, offsetY }
scale4 :: V4Modifier
scale4  amount xMult yMult offsetX = scale amount xMult yMult offsetX $ n 0.0 -- offsetY
scale3 :: V3Modifier
scale3  amount xMult yMult = scale4 amount xMult yMult $ n 0.0 -- offsetX
scale2 :: V2Modifier
scale2 amount xMult = scale3 amount xMult $ n 1.0 -- yMult
scale1 :: V1Modifier
scale1 amount = scale2 amount $ n 1.0 -- xMult
scale0 :: V0Modifier
scale0 = scale1 $ n 3.0 -- amount


scrollX  :: V2Modifier
scrollX  amount speed = modify $ H.geometry $ H.ScrollX { scrollX : amount, speed }
scrollX1 :: V1Modifier
scrollX1 amount = scrollX amount $ n 0.0 -- speed
scrollX0 :: V0Modifier
scrollX0 = scrollX1 $ n 0.5 -- amount


scrollY  :: V2Modifier
scrollY  amount speed = modify $ H.geometry $ H.ScrollY { scrollY : amount, speed }
scrollY1 :: V1Modifier
scrollY1 amount = scrollY amount $ n 0.0 -- speed
scrollY0 :: V0Modifier
scrollY0 = scrollY1 $ n 0.5 -- amount


{- ##### -}
{- Blend -}
{- ##### -}


add  :: VT1Modifier
add  what amount = modify $ H.blend $ H.Add { what, amount }
add0 :: VT0Modifier
add0 what = add what $ n 0.5 -- amount


blend  :: VT1Modifier
blend  what amount = modify $ H.blend $ H.Blend { what, amount }
blend0 :: VT0Modifier
blend0 what = blend what $ n 0.5 -- amount


diff  :: VT0Modifier
diff  what = modify $ H.blend $ H.Diff { what }


layer  :: VT0Modifier
layer  what = modify $ H.blend $ H.Layer { what }


mask  :: VT2Modifier
mask  what reps offset = modify $ H.blend $ H.Mask { what, reps, offset }
mask1 :: VT1Modifier
mask1 what reps = mask what reps $ n 0.5 -- offset
mask0 :: VT0Modifier
mask0 what = mask1 what $ n 3.0 -- reps


mult  :: VT1Modifier
mult  what amount = modify $ H.blend $ H.Mult { what, amount }
mult0 :: VT0Modifier
mult0 what = mult what $ n 0.5 -- amount


{- ######## -}
{- Modulate -}
{- ######## -}


modulate  :: VT1Modifier
modulate  what amount = modify $ H.modulate $ H.Modulate { what, amount }
modulate0 :: VT0Modifier
modulate0 what = modulate what $ n 0.1 -- amount


modulateHue  :: VT1Modifier
modulateHue  what amount = modify $ H.modulate $ H.ModulateHue { what, amount }
modulateHue0 :: VT0Modifier
modulateHue0 what = modulateHue what $ n 1.0 -- amount


modulateKaleid  :: VT1Modifier
modulateKaleid  what nSides = modify $ H.modulate $ H.ModulateKaleid { what, nSides }
modulateKaleid0 :: VT0Modifier
modulateKaleid0 what = modulateKaleid what $ n 4.0 -- nSides


modulatePixelate  :: VT2Modifier
modulatePixelate  what multiple offset = modify $ H.modulate $ H.ModulatePixelate { what, multiple, offset }
modulatePixelate1 :: VT1Modifier
modulatePixelate1 what multiple = modulatePixelate what multiple $ n 3.0 -- offset
modulatePixelate0 :: VT0Modifier
modulatePixelate0 what = modulatePixelate1 what $ n 10.0 -- multiple


modulateRepeat  :: VT4Modifier
modulateRepeat  what repeatX repeatY offsetX offsetY = modify $ H.modulate $ H.ModulateRepeat { what, repeatX, repeatY, offsetX, offsetY }
modulateRepeat3 :: VT3Modifier
modulateRepeat3 what repeatX repeatY offsetX = modulateRepeat what repeatX repeatY offsetX $ n 0.5 -- offsetY
modulateRepeat2 :: VT2Modifier
modulateRepeat2 what repeatX repeatY = modulateRepeat3 what repeatX repeatY $ n 0.5 -- offsetX
modulateRepeat1 :: VT1Modifier
modulateRepeat1 what repeatX = modulateRepeat2 what repeatX $ n 3.0 -- repeatY
modulateRepeat0 :: VT0Modifier
modulateRepeat0 what = modulateRepeat1 what $ n 3.0 -- repeatX


modulateRepeatX  :: VT2Modifier
modulateRepeatX  what reps offset = modify $ H.modulate $ H.ModulateRepeatX { what, reps, offset }
modulateRepeatX1 :: VT1Modifier
modulateRepeatX1 what reps = modulateRepeatX what reps $ n 0.5 -- offset
modulateRepeatX0 :: VT0Modifier
modulateRepeatX0 what = modulateRepeatX1 what $ n 3.0 -- reps


modulateRepeatY  :: VT2Modifier
modulateRepeatY  what reps offset = modify $ H.modulate $ H.ModulateRepeatY { what, reps, offset }
modulateRepeatY1 :: VT1Modifier
modulateRepeatY1 what reps = modulateRepeatY what reps $ n 0.5 -- offset
modulateRepeatY0 :: VT0Modifier
modulateRepeatY0 what = modulateRepeatY1 what $ n 3.0 -- reps


modulateRotate  :: VT2Modifier
modulateRotate  what multiple offset = modify $ H.modulate $ H.ModulateRotate { what, multiple, offset }
modulateRotate1 :: VT1Modifier
modulateRotate1 what multiple = modulateRotate what multiple $ n 0.0 -- offset
modulateRotate0 :: VT0Modifier
modulateRotate0 what = modulateRotate1 what $ n 1.0 -- multiple


modulateScale  :: VT2Modifier
modulateScale  what multiple offset = modify $ H.modulate $ H.ModulateScale { what, multiple, offset }
modulateScale1 :: VT1Modifier
modulateScale1 what multiple = modulateScale what multiple $ n 1.0 -- offset
modulateScale0 :: VT0Modifier
modulateScale0 what = modulateScale1 what $ n 1.0 -- multiple


modulateScrollX  :: VT2Modifier
modulateScrollX  what scrollX speed = modify $ H.modulate $ H.ModulateScrollX { what, scrollX, speed }
modulateScrollX1 :: VT1Modifier
modulateScrollX1 what scrollX = modulateScrollX what scrollX $ n 0.0 -- speed
modulateScrollX0 :: VT0Modifier
modulateScrollX0 what = modulateScrollX1 what $ n 0.5 -- scrollX


modulateScrollY  :: VT2Modifier
modulateScrollY  what scrollY speed = modify $ H.modulate $ H.ModulateScrollY { what, scrollY, speed }
modulateScrollY1 :: VT1Modifier
modulateScrollY1 what scrollY = modulateScrollY what scrollY $ n 0.0 -- speed
modulateScrollY0 :: VT0Modifier
modulateScrollY0 what = modulateScrollY1 what $ n 0.5 -- scrollY
