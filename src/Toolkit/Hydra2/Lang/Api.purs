module Toolkit.Hydra2.Lang.Api where

import Toolkit.Hydra2.Types

import Prelude (Unit, add, flip, unit, ($), (<<<))
import Toolkit.Hydra2.Lang (Command(..), Program(..), Single(..))
import Toolkit.Hydra2.Types (Value(..)) as T


unknown ∷ Program Unit -- private
unknown = Program Unknown unit


q :: Command -> Program Unit -- private
q cmd = Program cmd unit


s0 ∷ Source
s0 = S0


h0 :: AudioBin
h0 = H0


o0 ∷ Output
o0 = Output0


o1 ∷ Output
o1 = Output1


o2 ∷ Output
o2 = Output2


o3 ∷ Output
o3 = Output3


o4 ∷ Output
o4 = Output4


a :: Audio
a = Mic


show :: Audio -> Program Unit
show = q <<< One <<< WithAudio <<< Show


setBins :: Int -> Audio -> Program Unit
setBins bins = q <<< One <<< WithAudio <<< flip SetBins bins


setSmooth :: Audio -> Program Unit
setSmooth = q <<< One <<< WithAudio <<< SetSmooth


setCutoff :: Number -> Audio -> Program Unit
setCutoff n = q <<< One <<< WithAudio <<< flip SetCutoff n


setScale :: Number -> Audio -> Program Unit
setScale n = q <<< One <<< WithAudio <<< flip SetScale n


hide :: Audio -> Program Unit
hide = q <<< One <<< WithAudio <<< Hide


{- Source -}


noise :: Value -> Value -> Texture
noise scale offset =
    From $ Noise { scale, offset }


voronoi :: Value -> Value -> Value -> Texture
voronoi scale speed blending =
    From $ Voronoi { scale, speed, blending }


osc ∷ Value → Value → Value → Texture
osc frequency sync offset =
    From $ Osc { frequency, sync, offset }


shape :: Value -> Value -> Value -> Texture
shape sides radius smoothing =
    From $ Shape { sides, radius, smoothing }


gradient ∷ Value → Texture
gradient speed =
    From $ Gradient { speed }


src :: Source -> Texture
src = From


solid :: Value -> Value -> Value -> Value -> Texture
solid r g b a =
    From $ Solid { r, g, b, a }


{- Geometry -}


rotate ∷ Value → Value → Texture -> Texture
rotate angle speed =
    flip Geometry $ GRotate
        { angle
        , speed
        }


scale ∷ Value → Value -> Value -> Value -> Value -> Texture → Texture
scale amount xMult yMult offsetX offsetY =
    flip Geometry $ GScale
        { amount
        , xMult
        , yMult
        , offsetX
        , offsetY
        }


pixelate ∷ Value → Value → Texture -> Texture
pixelate pixelX pixelY =
    flip Geometry $ GPixelate
        { pixelX
        , pixelY
        }


repeat ∷ Value → Value -> Value -> Value -> Texture → Texture
repeat repeatX repeatY offsetX offsetY =
    flip Geometry $ GRepeat
        { repeatX
        , repeatY
        , offsetX
        , offsetY
        }


repeatX ∷ Value → Value -> Texture → Texture
repeatX reps offset =
    flip Geometry $ GRepeatX
        { reps
        , offset
        }


repeatY ∷ Value → Value -> Texture → Texture
repeatY reps offset =
    flip Geometry $ GRepeatY
        { reps
        , offset
        }


kaleid ∷ Value → Texture -> Texture
kaleid nSides =
    flip Geometry $ GKaleid
        { nSides
        }


scroll ∷ Value → Value -> Value -> Value -> Texture → Texture
scroll scrollX scrollY speedX speedY =
    flip Geometry $ GScroll
        { scrollX
        , scrollY
        , speedX
        , speedY
        }


scrollX ∷ Value → Value -> Texture → Texture
scrollX scrollX speed =
    flip Geometry $ GScrollX
        { scrollX
        , speed
        }


scrollY ∷ Value → Value -> Texture → Texture
scrollY scrollY speed =
    flip Geometry $ GScrollY
        { scrollY
        , speed
        }


{- Color -}


posterize :: Value -> Value -> Texture -> Texture
posterize bins gamma =
    flip WithColor $ Posterize { bins, gamma }


shift :: Value -> Value -> Value -> Value -> Texture -> Texture
shift r g b a =
    flip WithColor $ Shift { r, g, b, a }


invert :: Value -> Texture -> Texture
invert v =
    flip WithColor $ Invert v


saturate :: Value -> Texture -> Texture
saturate v =
    flip WithColor $ Saturate v


contrast :: Value -> Texture -> Texture
contrast v =
    flip WithColor $ Contrast v


brightness :: Value -> Texture -> Texture
brightness v =
    flip WithColor $ Brightness v


luma :: Value -> Value -> Texture -> Texture
luma treshold tolerance =
    flip WithColor $ Luma { treshold, tolerance }


tresh :: Value -> Value -> Texture -> Texture
tresh treshold tolerance =
    flip WithColor $ Tresh { treshold, tolerance }


color :: Value -> Value -> Value -> Value -> Texture -> Texture
color r g b a =
    flip WithColor $ Color { r, g, b, a }


hue :: Value -> Texture -> Texture
hue v =
    flip WithColor $ Hue v


colorama :: Value -> Texture -> Texture
colorama v =
    flip WithColor $ Colorama v


r :: Value -> Value -> Texture -> Texture
r scale offset =
    flip WithColor $ R { scale, offset }


g :: Value -> Value -> Texture -> Texture
g scale offset =
    flip WithColor $ G { scale, offset }


b :: Value -> Value -> Texture -> Texture
b scale offset =
    flip WithColor $ B { scale, offset }


alpha :: Value -> Value -> Texture -> Texture
alpha scale offset =
    flip WithColor $ A { scale, offset }


{- Blend -}


blend :: Texture -> Value -> Texture -> Texture
blend what v with =
    BlendOf { what, with } $ Blend v


add :: Texture -> Value -> Texture -> Texture
add what v with =
    BlendOf { what, with } $ Add v


sub :: Texture -> Value -> Texture -> Texture
sub what v with =
    BlendOf { what, with } $ Sub v


mult :: Texture -> Value -> Texture -> Texture
mult what v with =
    BlendOf { what, with } $ Mult v


layer :: Texture -> Value -> Texture -> Texture
layer what v with =
    BlendOf { what, with } $ Layer v


diff :: Texture -> Texture -> Texture
diff what with =
    BlendOf { what, with } $ Diff


mask :: Texture -> Texture -> Texture
mask what with =
    BlendOf { what, with } $ Mask


{- Modulate -}


modulate :: Texture -> Value -> Texture -> Texture
modulate what value with =
    ModulateWith { what, with } $ Modulate value


modulateRepeat :: Texture -> Value -> Value -> Value -> Value -> Texture -> Texture
modulateRepeat what repeatX repeatY offsetX offsetY with =
    ModulateWith { what, with } $ ModRepeat
        { repeatX
        , repeatY
        , offsetX
        , offsetY
        }


modulateRepeatX :: Texture -> Value -> Value -> Texture -> Texture
modulateRepeatX what reps offset with =
    ModulateWith { what, with } $ ModRepeatX
        { reps
        , offset
        }


modulateRepeatY :: Texture -> Value -> Value -> Texture -> Texture
modulateRepeatY what reps offset with =
    ModulateWith { what, with } $ ModRepeatY
        { reps
        , offset
        }


modulateScroll :: Texture -> Value -> Value -> Value -> Value -> Texture -> Texture
modulateScroll what scrollX scrollY speedX speedY with =
    ModulateWith { what, with } $ ModScroll
        { scrollX
        , scrollY
        , speedX
        , speedY
        }


modulateScrollX :: Texture -> Value -> Value -> Texture -> Texture
modulateScrollX what scrollX speed with =
    ModulateWith { what, with } $ ModScrollX
        { scrollX
        , speed
        }


modulateScrollY :: Texture -> Value -> Value -> Texture -> Texture
modulateScrollY what scrollY speed with =
    ModulateWith { what, with } $ ModScrollY
        { scrollY
        , speed
        }


modulateKaleid :: Texture -> Value -> Texture -> Texture
modulateKaleid what nSides with =
    ModulateWith { what, with } $ ModKaleid
        { nSides
        }


modulatePixelate :: Texture -> Value -> Value -> Texture -> Texture
modulatePixelate what multiple offset with =
    ModulateWith { what, with } $ ModPixelate
        { multiple
        , offset
        }


modulateScale :: Texture -> Value -> Value -> Texture -> Texture
modulateScale what multiple offset with =
    ModulateWith { what, with } $ ModScale
        { multiple
        , offset
        }


modulateRotate :: Texture -> Value -> Value -> Texture -> Texture
modulateRotate what multiple offset with =
    ModulateWith { what, with } $ ModRotate
        { multiple
        , offset
        }


modulateHue :: Texture -> Value -> Texture -> Texture
modulateHue what value with =
    ModulateWith { what, with } $ ModHue value


{- External Sources -}


initCam ∷ Source → Program Unit
initCam = q <<< One <<< InitCam


initCam' ∷ Source → Value -> Program Unit
initCam' s = q <<< One <<< InitCamIdx s


initScreen ∷ Source → Program Unit
initScreen = q <<< One <<< InitScreen


clear ∷ Source → Program Unit
clear = q <<< One <<< Clear


{- Other -}

fn :: (Context -> Value) -> Value
fn = Dep


outs :: Texture -> Program Unit
outs =
    q <<< End Screen


out :: Output -> Texture -> Program Unit
out output =
    q <<< End output


fft :: AudioBin -> Audio -> Value
fft = flip Audio


render :: Output -> Program Unit
render = q <<< One <<< Render <<< Output


renderAll :: Program Unit
renderAll = q $ One $ Render All


n :: Number -> Value
n = Number