module Toolkit.Hydra2.Lang.Api where

import Prelude (Unit, flip, unit, ($), (<<<))


import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Types (Value(..)) as T

import Toolkit.Hydra2.Lang (Command(..), Program(..), Single(..))


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


-- posterize, shift, invert, contrast, brightness, luma, tresh, color, saturate, hue, colorama, sum, r, g, b, a


saturate :: Value -> Texture -> Texture
saturate v =
    flip WithColor $ Saturate v


{- Blend -}


-- add, sub, layer, blend, mult, diff, mask


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