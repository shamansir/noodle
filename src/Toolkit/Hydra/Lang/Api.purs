module Tookit.Hydra.Lang.Api where

import Tookit.Hydra.Types

import Prelude

import Tookit.Hydra.Lang (Command(..), Program(..), Single(..))
import Tookit.Hydra.Types (Value(..)) as T


unknown ∷ Program Unit -- private
unknown = Program Unknown unit


q :: Command -> Program Unit -- private
q cmd = Program cmd unit


s0 ∷ SourceN
s0 = Source0


h0 :: AudioBin
h0 = AudioBin 0


o0 ∷ OutputN
o0 = Output0


o1 ∷ OutputN
o1 = Output1


o2 ∷ OutputN
o2 = Output2


o3 ∷ OutputN
o3 = Output3


o4 ∷ OutputN
o4 = Output4


a :: AudioSource
a = Mic


show :: AudioSource -> Program Unit
show = q <<< One <<< WithAudio <<< Show


setBins :: Int -> AudioSource -> Program Unit
setBins bins = q <<< One <<< WithAudio <<< flip SetBins bins


setSmooth :: Number -> AudioSource -> Program Unit
setSmooth n = q <<< One <<< WithAudio <<< flip SetSmooth n


setCutoff :: Number -> AudioSource -> Program Unit
setCutoff n = q <<< One <<< WithAudio <<< flip SetCutoff n


setScale :: Number -> AudioSource -> Program Unit
setScale n = q <<< One <<< WithAudio <<< flip SetScale n


hide :: AudioSource -> Program Unit
hide = q <<< One <<< WithAudio <<< Hide


{- Source -}


noise :: Value -> Value -> Texture
noise scale offset =
    Start $ Noise { scale, offset }


voronoi :: Value -> Value -> Value -> Texture
voronoi scale speed blending =
    Start $ Voronoi { scale, speed, blending }


osc ∷ Value → Value → Value → Texture
osc frequency sync offset =
    Start $ Osc { frequency, sync, offset }


shape :: Value -> Value -> Value -> Texture
shape sides radius smoothing =
    Start $ Shape { sides, radius, smoothing }


gradient ∷ Value → Texture
gradient speed =
    Start $ Gradient { speed }


solid :: Value -> Value -> Value -> Value -> Texture
solid r g b a =
    Start $ Solid { r, g, b, a }


src :: SourceN -> Texture
src sn = Start $ External sn $ Camera 0 -- FIXME


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
    flip Filter $ Posterize { bins, gamma }


shift :: Value -> Value -> Value -> Value -> Texture -> Texture
shift r g b a =
    flip Filter $ Shift { r, g, b, a }


invert :: Value -> Texture -> Texture
invert v =
    flip Filter $ Invert v


saturate :: Value -> Texture -> Texture
saturate v =
    flip Filter $ Saturate v


contrast :: Value -> Texture -> Texture
contrast v =
    flip Filter $ Contrast v


brightness :: Value -> Texture -> Texture
brightness v =
    flip Filter $ Brightness v


luma :: Value -> Value -> Texture -> Texture
luma threshold tolerance =
    flip Filter $ Luma { threshold, tolerance }


thresh :: Value -> Value -> Texture -> Texture
thresh threshold tolerance =
    flip Filter $ Thresh { threshold, tolerance }


color :: Value -> Value -> Value -> Value -> Texture -> Texture
color r g b a =
    flip Filter $ Color { r, g, b, a }


hue :: Value -> Texture -> Texture
hue v =
    flip Filter $ Hue v


colorama :: Value -> Texture -> Texture
colorama v =
    flip Filter $ Colorama v


r :: Value -> Value -> Texture -> Texture
r scale offset =
    flip Filter $ R { scale, offset }


g :: Value -> Value -> Texture -> Texture
g scale offset =
    flip Filter $ G { scale, offset }


b :: Value -> Value -> Texture -> Texture
b scale offset =
    flip Filter $ B { scale, offset }


alpha :: Value -> Value -> Texture -> Texture
alpha scale offset =
    flip Filter $ A { scale, offset }


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
fn f = Dep $ Fn (\ctx -> pure $ f ctx)


outs :: Texture -> Program Unit
outs =
    q <<< End Output0


out :: OutputN -> Texture -> Program Unit
out output =
    q <<< End output


fft :: AudioBin -> AudioSource -> Value
fft = const <<< Fft


render :: OutputN -> Program Unit
render = q <<< One <<< Render <<< Output


renderAll :: Program Unit
renderAll = q $ One $ Render Four


n :: Number -> Value
n = Number